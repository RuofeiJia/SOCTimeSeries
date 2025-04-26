# find regional numbers by decade by regions in Pan et al. from bootstrapped models
# 

rm(list = ls(all = TRUE))

# package and direcotry
library(dplyr)
library(raster)
library(terra)
library(tictoc)
library(sp)

# working directory should be set by default to the folder of entire repo (R project)

#============= load pixel-level prediction results =======================
tic(); print("start: loading results")
load("./results/prediction_result.rda")

toc(); print("results loaded")

#========== load proportion_decade_df ===========
tic(); print("start: load propotion_df and contour_dfs")
load("./data/proportion_decade_df.rda")

# define boreal, temperate, tropical region
boreal_regions <- c("Canada","Northern Europe","Asian Russia","European Russia", "Alaska")
temperate_regions <- c("Continental United States", "Europe", "China", "Japan and Korea", "Australia", 
                       "New Zealand", "Other temperate", "Others") # 
tropical_regions <- c("Africa", "Mexico and Central America", "South America", "Southeast Asia", "South Asia")

# generate proportion_biome_df with column biome
proportion_zone_df <- proportion_decade_df %>%
  dplyr::mutate(zone = case_when(
    pan_region_new2 %in% boreal_regions ~ "boreal",
    pan_region_new2 %in% temperate_regions ~ "temperate",
    pan_region_new2 %in% tropical_regions ~ "tropical"
  ))

toc(); print("proportion_decade_df loaded") 
#================ load functions =====================
tic(); print("start: loading functions")

# function to get mode (https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
getmode <- function(v) {
  round_v <- round(v,1)
  uniqv <- unique(round_v, 1)
  uniqv[which.max(tabulate(match(round_v, uniqv)))]
}

# sum delta_df_list by region
# return summarized df with each region per row, each column per bootstrapped model
# Values are total SOC change for one posterior sample (column) in one region (row)
sum_by_region <- function(delta_df_list=delta_youngforest1990_df_list, proportion_str="youngforest1990",
                          type_str="young forest", decade_str="1990") {
  
  # merge proportion_decade_df
  delta_df_list <- merge(delta_df_list, proportion_decade_df, by=c("x","y"), all.x=T)
  
  # set up for loop
  num_values <- sum(grepl("value", colnames(delta_df_list)))
  value_columns <- grep("value", names(delta_df_list), value = TRUE)
  index <- 1
  sum_region <- data.frame(pan_region_new2 = unique(proportion_decade_df$pan_region_new2))
  
  # rename relevant proportion column into "proportion"
  colnames(delta_df_list)[which(colnames(delta_df_list)==proportion_str)] <- "proportion"
  
  for (col in value_columns) {
    # rename column in loop
    colnames(delta_df_list)[which(colnames(delta_df_list)==col)] <- "col_temp"
    
    df_temp <- delta_df_list %>% group_by(pan_region_new2) %>%
      summarize(delta_total = sum(col_temp*cellsize_ha*proportion, na.rm=T)/1e9 ) # Mg into Pg/yr
    
    # rename temp delta_total
    colnames(df_temp)[which(colnames(df_temp)=="delta_total")] <- paste0("delta_total_", index)
    
    # rename back
    colnames(delta_df_list)[which(colnames(delta_df_list)=="col_temp")] <- col
    
    # add to sum_region df
    sum_region <- merge(sum_region, df_temp, by="pan_region_new2", all.x=T)
    index <- index + 1
  }
  
  # construct region_area_df with one number each pan_region_new2
  region_area_df <- delta_df_list %>% group_by(pan_region_new2) %>%
    summarize(region_area_ha= sum(cellsize_ha*proportion) )
  
  # add type, decade, and area column
  sum_region <- merge(sum_region, region_area_df, by="pan_region_new2", all.x=T)
  
  sum_region <- sum_region %>%
    dplyr::mutate(type=type_str, decade=decade_str)
  
  return(sum_region)
}

# sum delta_df by zone (tropical, temperate, boreal)
sum_by_zone <- function(delta_df_list=delta_youngforest1990_df_list, proportion_str="youngforest1990",
                        type_str="young forest", decade_str="1990") {
  
  # merge proportion_zone_df
  delta_df_list <- merge(delta_df_list, proportion_zone_df, by=c("x","y"), all.x=T)
  
  # set up for loop
  num_values <- sum(grepl("value", colnames(delta_df_list)))
  value_columns <- grep("value", names(delta_df_list), value = TRUE)
  index <- 1
  sum_zone <- data.frame(zone = unique(proportion_zone_df$zone))
  
  # rename relevant proportion column into "proportion"
  colnames(delta_df_list)[which(colnames(delta_df_list)==proportion_str)] <- "proportion"
  
  for (col in value_columns) {
    # rename column in loop
    colnames(delta_df_list)[which(colnames(delta_df_list)==col)] <- "col_temp"
    
    df_temp <- delta_df_list %>% group_by(zone) %>%
      summarize(delta_total = sum(col_temp*cellsize_ha*proportion, na.rm=T)/1e9 ) # Mg into Pg/yr
    
    # rename temp delta_total
    colnames(df_temp)[which(colnames(df_temp)=="delta_total")] <- paste0("delta_total_", index)
    
    # rename back
    colnames(delta_df_list)[which(colnames(delta_df_list)=="col_temp")] <- col
    
    # add to sum_zone df
    sum_zone <- merge(sum_zone, df_temp, by="zone", all.x=T)
    index <- index + 1
  }
  
  # construct zone_area_df with one number each zone
  zone_area_df <- delta_df_list %>% group_by(zone) %>%
    summarize(zone_area_ha= sum(cellsize_ha*proportion) )
  
  # add type, decade, and area column
  sum_zone <- merge(sum_zone, zone_area_df, by="zone", all.x=T)
  
  sum_zone <- sum_zone %>%
    dplyr::mutate(type=type_str, decade=decade_str)
  
  return(sum_zone)
}

# sum delta_df by type (global young/old forest, grassland)
# sum delta_df by type by decade (global)
sum_by_type <- function(delta_df_list=delta_youngforest1990_df_list, proportion_str="youngforest1990",
                        type_str="young forest", decade_str="1990") {
  
  # merge proportion_type_df
  delta_df_list <- merge(delta_df_list, proportion_zone_df, by=c("x","y"), all.x=T)
  
  # set up for loop
  num_values <- sum(grepl("value", colnames(delta_df_list)))
  value_columns <- grep("value", names(delta_df_list), value = TRUE)
  index <- 1
  sum_type <- data.frame(type=type_str)
  
  # rename relevant proportion column into "proportion"
  colnames(delta_df_list)[which(colnames(delta_df_list)==proportion_str)] <- "proportion"
  
  for (col in value_columns) {
    # rename column in loop
    colnames(delta_df_list)[which(colnames(delta_df_list)==col)] <- "col_temp"
    
    df_temp <- delta_df_list %>% ungroup() %>%
      summarize(delta_total = sum(col_temp*cellsize_ha*proportion, na.rm=T)/1e9 ) # Mg into Pg/yr
    
    # rename temp delta_total
    colnames(df_temp)[which(colnames(df_temp)=="delta_total")] <- paste0("delta_total_", index)
    
    # rename back
    colnames(delta_df_list)[which(colnames(delta_df_list)=="col_temp")] <- col
    
    # add to sum_type df
    sum_type <- cbind(sum_type, df_temp)
    index <- index + 1
  }
  
  # construct type_area_df with one number each type
  type_area_df <- delta_df_list %>% ungroup() %>%
    summarize(type_area_ha= sum(cellsize_ha*proportion) )
  
  # add type, decade, and area column
  sum_type <- cbind(sum_type, type_area_df)
  
  sum_type <- sum_type %>%
    dplyr::mutate(decade=decade_str)
  
  return(sum_type)
}

# function that summarize pixel level results
summarize_pixel_result <- function(delta_list, decade_str, type_str) {
  # delta_list has columns x,y, value1, value2, ...
  summary <- delta_list %>% 
    dplyr::mutate(
      decade = decade_str,
      type = type_str
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(delta_mean =  mean(c_across(contains("value")), na.rm = TRUE),
                  delta_median =  median(c_across(contains("value")), na.rm = TRUE),
                  delta_mode = getmode(c_across(contains("value"))),
                  delta_sd = sd(c_across(contains("value")), na.rm = TRUE),
                  delta_2.5 = quantile(c_across(contains("value")), 0.025, na.rm = TRUE),
                  delta_5 = quantile(c_across(contains("value")), 0.05, na.rm = TRUE),
                  delta_95 = quantile(c_across(contains("value")), 0.95, na.rm = TRUE),
                  delta_97.5 = quantile(c_across(contains("value")), 0.975, na.rm = TRUE),
                  percentage_above_zero = sum(c_across(contains("value"))>0)/length(c_across(contains("value")) )
    ) %>%
    dplyr::ungroup() %>% # stop rowwise
    dplyr::select(-contains("value")) %>%
    dplyr::mutate(yearspan=case_when(
      grepl("1990", decade) ~ 8,
      grepl("2000", decade) ~ 10,
      grepl("2010", decade) ~ 10,
      grepl("1992-2020", decade) ~ 28,
      grepl("2010-2018", decade) ~ 8
    )) %>%
    dplyr::mutate( # total annual change PgC/yr
      annual_mean = delta_mean/yearspan,
      annual_median = delta_median/yearspan,
      annual_mode = delta_mode/yearspan,
      annual_sd = delta_sd/yearspan,
      annual_2.5 = delta_2.5/yearspan,
      annual_5 = delta_5/yearspan,
      annual_95 = delta_95/yearspan,
      annual_97.5 = delta_97.5/yearspan
    ) 
  
  # merge with area proportion and contour
  summary_new <- merge(summary, proportion_decade_df, by=c("x","y"), all.x=T)
  
  return(summary_new)
}

# calculate total SOC change per pixel in gigagram
calculate_SOC_change_pixel <- function(summary_df, proportion_str="youngforest1990") {
  # rename relevant proportion column into "proportion"
  colnames(summary_df)[which(colnames(summary_df)==proportion_str)] <- "proportion"
  
  summary_df_new <- summary_df %>%
    mutate( # calculate total SOC change per pixel in Gigagram/yr
      annual_mean_GgperPixel = annual_mean*proportion*cellsize_ha/1e3,
      annual_median_GgperPixel = annual_median*proportion*cellsize_ha/1e3) %>%
    dplyr::select( # select only columns relevant to this type and decaade
      x,y,decade,type,delta_mean,delta_median,delta_mode,delta_sd,
      delta_2.5,delta_5,delta_95,delta_97.5,percentage_above_zero,yearspan,
      annual_mean,annual_median,annual_mode,annual_sd,
      annual_2.5,annual_5,annual_95,annual_97.5,
      proportion,cellsize_ha,pan_region_new2,
      annual_mean_GgperPixel,annual_median_GgperPixel
    )
  
  return(summary_df_new)
}

#==================== compile results ==========================================
tic(); print("start: compile results")
# young forest
delta_youngforest1990_df_list <- delta_youngforest1990_df_listb; rm(delta_youngforest1990_df_listb) # to save memory
delta_youngforest2000_df_list <- delta_youngforest2000_df_listb; rm(delta_youngforest2000_df_listb)
delta_youngforest2010_df_list <- delta_youngforest2010_df_listb; rm(delta_youngforest2010_df_listb)
delta_youngforest_total_df_list <- delta_youngforest_total_df_listb; rm(delta_youngforest_total_df_listb)

# old forest
delta_oldforest1990_df_list <- delta_oldforest1990_df_listb; rm(delta_oldforest1990_df_listb)
delta_oldforest2000_df_list <- delta_oldforest2000_df_listb; rm(delta_oldforest2000_df_listb)
delta_oldforest2010_df_list <- delta_oldforest2010_df_listb; rm(delta_oldforest2010_df_listb)
delta_oldforest_total_df_list <- delta_oldforest_total_df_listb; rm(delta_oldforest_total_df_listb)

# grassland
delta_grassland1990_df_list <- delta_grassland1990_df_listb; rm(delta_grassland1990_df_listb)
delta_grassland2000_df_list <- delta_grassland2000_df_listb; rm(delta_grassland2000_df_listb)
delta_grassland2010_df_list <- delta_grassland2010_df_listb; rm(delta_grassland2010_df_listb)
delta_grassland_total_df_list <- delta_grassland_total_df_listb; rm(delta_grassland_total_df_listb)

toc(); print("result lists compiled")

#================== sum regional numbers from posterior predictions ==============
tic(); print("start: sum pixels by model by pan_region_new2")
# young forest
sum_youngforest1990_delta_all <- sum_by_region(delta_youngforest1990_df_list, proportion_str="youngforest1990",
                                               type_str="young forest", decade_str="1990")
sum_youngforest2000_delta_all <- sum_by_region(delta_youngforest2000_df_list, proportion_str="youngforest2000",
                                               type_str="young forest", decade_str="2000")
sum_youngforest2010_delta_all <- sum_by_region(delta_youngforest2010_df_list, proportion_str="youngforest2010",
                                               type_str="young forest", decade_str="2010")
sum_youngforest1992_2020_delta_all <- sum_by_region(delta_youngforest_total_df_list, proportion_str="avg_youngforest1992_2020",
                                                    type_str="young forest", decade_str="1992-2020")

# old forest
sum_oldforest1990_delta_all <- sum_by_region(delta_oldforest1990_df_list, proportion_str="oldforest1990",
                                             type_str="old forest", decade_str="1990")
sum_oldforest2000_delta_all <- sum_by_region(delta_oldforest2000_df_list, proportion_str="oldforest2000",
                                             type_str="old forest", decade_str="2000")
sum_oldforest2010_delta_all <- sum_by_region(delta_oldforest2010_df_list, proportion_str="oldforest2010",
                                             type_str="old forest", decade_str="2010")
sum_oldforest1992_2020_delta_all <- sum_by_region(delta_oldforest_total_df_list, proportion_str="avg_oldforest1992_2020",
                                                  type_str="old forest", decade_str="1992-2020")

# grassland
sum_grassland1990_delta_all <- sum_by_region(delta_grassland1990_df_list, proportion_str="grassland1990",
                                             type_str="grassland", decade_str="1990")
sum_grassland2000_delta_all <- sum_by_region(delta_grassland2000_df_list, proportion_str="grassland2000",
                                             type_str="grassland", decade_str="2000")
sum_grassland2010_delta_all <- sum_by_region(delta_grassland2010_df_list, proportion_str="grassland2010",
                                             type_str="grassland", decade_str="2010")
sum_grassland1992_2020_delta_all <- sum_by_region(delta_grassland_total_df_list, proportion_str="grassland1992_2020",
                                                  type_str="grassland", decade_str="1992-2020")

# combine all regionals (one type, decade, pan_region_new2 per row, posterior prediction of each sample)
sum_delta_combined <- rbind(
  sum_youngforest1990_delta_all,sum_youngforest2000_delta_all,sum_youngforest2010_delta_all,sum_youngforest1992_2020_delta_all,
  sum_oldforest1990_delta_all,sum_oldforest2000_delta_all,sum_oldforest2010_delta_all,sum_oldforest1992_2020_delta_all,
  sum_grassland1990_delta_all,sum_grassland2000_delta_all,sum_grassland2010_delta_all,sum_grassland1992_2020_delta_all
)

toc(); print("done: sum pixels by model by pan_region_new2")
#================== find mean, median, 2.5, 5, 95, 97.5 percentiles ==========================
tic(); print("start: find quantiles for each region")
pan_region_CI <- sum_delta_combined %>%
  dplyr::rowwise() %>%
  dplyr::mutate(delta_mean =  mean(c_across(contains("delta_total")), na.rm = TRUE),
                delta_median =  median(c_across(contains("delta_total")), na.rm = TRUE),
                delta_mode = getmode(c_across(contains("delta_total"))),
                delta_sd = sd(c_across(contains("delta_total")), na.rm = TRUE),
                delta_2.5 = quantile(c_across(contains("delta_total")), 0.025, na.rm = TRUE),
                delta_5 = quantile(c_across(contains("delta_total")), 0.05, na.rm = TRUE),
                delta_95 = quantile(c_across(contains("delta_total")), 0.95, na.rm = TRUE),
                delta_97.5 = quantile(c_across(contains("delta_total")), 0.975, na.rm = TRUE),
                percentage_above_zero = sum(c_across(contains("delta_total"))>0)/length(c_across(contains("delta_total")) )
  ) %>%
  dplyr::ungroup() %>% # stop rowwise
  dplyr::select(-contains("delta_total")) %>%
  dplyr::mutate(yearspan=case_when(
    grepl("1990", decade) ~ 8,
    grepl("2000", decade) ~ 10,
    grepl("2010", decade) ~ 10,
    grepl("1992-2020", decade) ~ 28
  )) %>%
  dplyr::mutate( # total annual change PgC/yr
    annual_mean = delta_mean/yearspan,
    annual_median = delta_median/yearspan,
    annual_mode = delta_mode/yearspan,
    annual_sd = delta_sd/yearspan,
    annual_2.5 = delta_2.5/yearspan,
    annual_5 = delta_5/yearspan,
    annual_95 = delta_95/yearspan,
    annual_97.5 = delta_97.5/yearspan
  ) %>%
  dplyr::mutate( # percentiles on average SOC annual change Mg/ha/yr
    annual_mean_perha = annual_mean*1e9/region_area_ha,
    annual_median_perha = annual_median*1e9/region_area_ha,
    annual_mode_perha = annual_mode*1e9/region_area_ha,
    annual_sd_perha = annual_sd*1e9/region_area_ha,
    annual_2.5_perha = annual_2.5*1e9/region_area_ha,
    annual_5_perha = annual_5*1e9/region_area_ha,
    annual_95_perha = annual_95*1e9/region_area_ha,
    annual_97.5_perha = annual_97.5*1e9/region_area_ha
  )

toc(); print("done: find quantiles for each region")

#================== sum zone numbers from posterior predictions  =================
tic(); print("start: sum pixels by model by zone")
# young forest
sum_youngforest1990_delta_zone_all <- sum_by_zone(delta_youngforest1990_df_list, proportion_str="youngforest1990",
                                                  type_str="young forest", decade_str="1990")
sum_youngforest2000_delta_zone_all <- sum_by_zone(delta_youngforest2000_df_list, proportion_str="youngforest2000",
                                                  type_str="young forest", decade_str="2000")
sum_youngforest2010_delta_zone_all <- sum_by_zone(delta_youngforest2010_df_list, proportion_str="youngforest2010",
                                                  type_str="young forest", decade_str="2010")
sum_youngforest1992_2020_delta_zone_all <- sum_by_zone(delta_youngforest_total_df_list, proportion_str="avg_youngforest1992_2020",
                                                       type_str="young forest", decade_str="1992-2020")

# old forest
sum_oldforest1990_delta_zone_all <- sum_by_zone(delta_oldforest1990_df_list, proportion_str="oldforest1990",
                                                type_str="old forest", decade_str="1990")
sum_oldforest2000_delta_zone_all <- sum_by_zone(delta_oldforest2000_df_list, proportion_str="oldforest2000",
                                                type_str="old forest", decade_str="2000")
sum_oldforest2010_delta_zone_all <- sum_by_zone(delta_oldforest2010_df_list, proportion_str="oldforest2010",
                                                type_str="old forest", decade_str="2010")
sum_oldforest1992_2020_delta_zone_all <- sum_by_zone(delta_oldforest_total_df_list, proportion_str="avg_oldforest1992_2020",
                                                     type_str="old forest", decade_str="1992-2020")

# grassland
sum_grassland1990_delta_zone_all <- sum_by_zone(delta_grassland1990_df_list, proportion_str="grassland1990",
                                                type_str="grassland", decade_str="1990")
sum_grassland2000_delta_zone_all <- sum_by_zone(delta_grassland2000_df_list, proportion_str="grassland2000",
                                                type_str="grassland", decade_str="2000")
sum_grassland2010_delta_zone_all <- sum_by_zone(delta_grassland2010_df_list, proportion_str="grassland2010",
                                                type_str="grassland", decade_str="2010")
sum_grassland1992_2020_delta_zone_all <- sum_by_zone(delta_grassland_total_df_list, proportion_str="grassland1992_2020",
                                                     type_str="grassland", decade_str="1992-2020")

# combine all zone numbers (one type, decade, zone, per row, posterior prediction of each sample)
sum_delta_combined_zone <- rbind(
  sum_youngforest1990_delta_zone_all,sum_youngforest2000_delta_zone_all,sum_youngforest2010_delta_zone_all,sum_youngforest1992_2020_delta_zone_all,
  sum_oldforest1990_delta_zone_all,sum_oldforest2000_delta_zone_all,sum_oldforest2010_delta_zone_all,sum_oldforest1992_2020_delta_zone_all,
  sum_grassland1990_delta_zone_all,sum_grassland2000_delta_zone_all,sum_grassland2010_delta_zone_all,sum_grassland1992_2020_delta_zone_all
)

toc(); print("done: sum pixels by model by zone")
#================== find 2.5, 5, 95, 97.5 percentiles ==========================
tic(); print("start: find quantiles for each zone")
pan_zone_CI <- sum_delta_combined_zone %>%
  dplyr::rowwise() %>%
  dplyr::mutate(delta_mean =  mean(c_across(contains("delta_total")), na.rm = TRUE),
                delta_median =  median(c_across(contains("delta_total")), na.rm = TRUE),
                delta_mode = getmode(c_across(contains("delta_total"))),
                delta_sd = sd(c_across(contains("delta_total")), na.rm = TRUE),
                delta_2.5 = quantile(c_across(contains("delta_total")), 0.025, na.rm = TRUE),
                delta_5 = quantile(c_across(contains("delta_total")), 0.05, na.rm = TRUE),
                delta_95 = quantile(c_across(contains("delta_total")), 0.95, na.rm = TRUE),
                delta_97.5 = quantile(c_across(contains("delta_total")), 0.975, na.rm = TRUE),
                percentage_above_zero = sum(c_across(contains("delta_total"))>0)/length(c_across(contains("delta_total")) )
  ) %>%
  dplyr::ungroup() %>% # stop rowwise
  dplyr::select(-contains("delta_total")) %>%
  dplyr::mutate(yearspan=case_when(
    grepl("1990", decade) ~ 8,
    grepl("2000", decade) ~ 10,
    grepl("2010", decade) ~ 10,
    grepl("1992-2020", decade) ~ 28
  )) %>%
  dplyr::mutate( # total annual change PgC/yr
    annual_mean = delta_mean/yearspan,
    annual_median = delta_median/yearspan,
    annual_mode = delta_mode/yearspan,
    annual_sd = delta_sd/yearspan,
    annual_2.5 = delta_2.5/yearspan,
    annual_5 = delta_5/yearspan,
    annual_95 = delta_95/yearspan,
    annual_97.5 = delta_97.5/yearspan
  ) %>%
  dplyr::mutate( # percentiles on average SOC annual change Mg/ha/yr
    annual_mean_perha = annual_mean*1e9/zone_area_ha,
    annual_median_perha = annual_median*1e9/zone_area_ha,
    annual_mode_perha = annual_mode*1e9/zone_area_ha,
    annual_sd_perha = annual_sd*1e9/zone_area_ha,
    annual_2.5_perha = annual_2.5*1e9/zone_area_ha,
    annual_5_perha = annual_5*1e9/zone_area_ha,
    annual_95_perha = annual_95*1e9/zone_area_ha,
    annual_97.5_perha = annual_97.5*1e9/zone_area_ha
  )

toc(); print("done: find quantiles for each zone")
#================== sum type (global) number from posterior predictions ==========
tic(); print("start: sum pixels by model by type")
# young forest
sum_youngforest1990_delta_type_all <- sum_by_type(delta_youngforest1990_df_list, proportion_str="youngforest1990",
                                                  type_str="young forest", decade_str="1990")
sum_youngforest2000_delta_type_all <- sum_by_type(delta_youngforest2000_df_list, proportion_str="youngforest2000",
                                                  type_str="young forest", decade_str="2000")
sum_youngforest2010_delta_type_all <- sum_by_type(delta_youngforest2010_df_list, proportion_str="youngforest2010",
                                                  type_str="young forest", decade_str="2010")
sum_youngforest1992_2020_delta_type_all <- sum_by_type(delta_youngforest_total_df_list, proportion_str="avg_youngforest1992_2020",
                                                       type_str="young forest", decade_str="1992-2020")

# old forest
sum_oldforest1990_delta_type_all <- sum_by_type(delta_oldforest1990_df_list, proportion_str="oldforest1990",
                                                type_str="old forest", decade_str="1990")
sum_oldforest2000_delta_type_all <- sum_by_type(delta_oldforest2000_df_list, proportion_str="oldforest2000",
                                                type_str="old forest", decade_str="2000")
sum_oldforest2010_delta_type_all <- sum_by_type(delta_oldforest2010_df_list, proportion_str="oldforest2010",
                                                type_str="old forest", decade_str="2010")
sum_oldforest1992_2020_delta_type_all <- sum_by_type(delta_oldforest_total_df_list, proportion_str="avg_oldforest1992_2020",
                                                     type_str="old forest", decade_str="1992-2020")

# grassland
sum_grassland1990_delta_type_all <- sum_by_type(delta_grassland1990_df_list, proportion_str="grassland1990",
                                                type_str="grassland", decade_str="1990")
sum_grassland2000_delta_type_all <- sum_by_type(delta_grassland2000_df_list, proportion_str="grassland2000",
                                                type_str="grassland", decade_str="2000")
sum_grassland2010_delta_type_all <- sum_by_type(delta_grassland2010_df_list, proportion_str="grassland2010",
                                                type_str="grassland", decade_str="2010")
sum_grassland1992_2020_delta_type_all <- sum_by_type(delta_grassland_total_df_list, proportion_str="grassland1992_2020",
                                                     type_str="grassland", decade_str="1992-2020")

# combine all type numbers (one type, decade, per row, posterior prediction of each sample)
sum_delta_combined_type <- rbind(
  sum_youngforest1990_delta_type_all,sum_youngforest2000_delta_type_all,sum_youngforest2010_delta_type_all,sum_youngforest1992_2020_delta_type_all,
  sum_oldforest1990_delta_type_all,sum_oldforest2000_delta_type_all,sum_oldforest2010_delta_type_all,sum_oldforest1992_2020_delta_type_all,
  sum_grassland1990_delta_type_all,sum_grassland2000_delta_type_all,sum_grassland2010_delta_type_all,sum_grassland1992_2020_delta_type_all
)

toc(); print("done: sum pixels by model by type")
#================== find 2.5, 5, 95, 97.5 percentiles==========================
tic(); print("start: find quantiles for each type")
pan_type_CI <- sum_delta_combined_type %>%
  dplyr::rowwise() %>%
  dplyr::mutate(delta_mean =  mean(c_across(contains("delta_total")), na.rm = TRUE),
                delta_median =  median(c_across(contains("delta_total")), na.rm = TRUE),
                delta_mode = getmode(c_across(contains("delta_total"))),
                delta_sd = sd(c_across(contains("delta_total")), na.rm = TRUE),
                delta_2.5 = quantile(c_across(contains("delta_total")), 0.025, na.rm = TRUE),
                delta_5 = quantile(c_across(contains("delta_total")), 0.05, na.rm = TRUE),
                delta_95 = quantile(c_across(contains("delta_total")), 0.95, na.rm = TRUE),
                delta_97.5 = quantile(c_across(contains("delta_total")), 0.975, na.rm = TRUE),
                percentage_above_zero = sum(c_across(contains("delta_total"))>0)/length(c_across(contains("delta_total")) )
                
  ) %>%
  dplyr::ungroup() %>% # stop rowwise
  dplyr::select(-contains("delta_total")) %>%
  dplyr::mutate(yearspan=case_when(
    grepl("1990", decade) ~ 8,
    grepl("2000", decade) ~ 10,
    grepl("2010", decade) ~ 10,
    grepl("1992-2020", decade) ~ 28
  )) %>%
  dplyr::mutate( # total annual change PgC/yr
    annual_mean = delta_mean/yearspan,
    annual_median = delta_median/yearspan,
    annual_mode = delta_mode/yearspan,
    annual_sd = delta_sd/yearspan,
    annual_2.5 = delta_2.5/yearspan,
    annual_5 = delta_5/yearspan,
    annual_95 = delta_95/yearspan,
    annual_97.5 = delta_97.5/yearspan
  ) %>%
  dplyr::mutate( # percentiles on average SOC annual change Mg/ha/yr
    annual_mean_perha = annual_mean*1e9/type_area_ha,
    annual_median_perha = annual_median*1e9/type_area_ha,
    annual_mode_perha = annual_mode*1e9/type_area_ha,
    annual_sd_perha = annual_sd*1e9/type_area_ha,
    annual_2.5_perha = annual_2.5*1e9/type_area_ha,
    annual_5_perha = annual_5*1e9/type_area_ha,
    annual_95_perha = annual_95*1e9/type_area_ha,
    annual_97.5_perha = annual_97.5*1e9/type_area_ha
  )

toc(); print("done: find quantiles for each type")

#=================== sum by all global numbers ================================
sum_delta_global <- sum_delta_combined_type %>%
  dplyr::group_by(decade) %>%
  dplyr::summarize(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
                   across(where(is.character), first)) %>%
  ungroup() %>%
  dplyr::select(-type)

#================= find 2.5, 5, 95, 97.5 percentiles ===========================
tic(); print("start: find quantiles for global")
pan_global_CI <- sum_delta_global %>%
  dplyr::rowwise() %>%
  dplyr::mutate(delta_mean =  mean(c_across(contains("delta_total")), na.rm = TRUE),
                delta_median =  median(c_across(contains("delta_total")), na.rm = TRUE),
                delta_mode = getmode(c_across(contains("delta_total"))),
                delta_sd = sd(c_across(contains("delta_total")), na.rm = TRUE),
                delta_2.5 = quantile(c_across(contains("delta_total")), 0.025, na.rm = TRUE),
                delta_5 = quantile(c_across(contains("delta_total")), 0.05, na.rm = TRUE),
                delta_95 = quantile(c_across(contains("delta_total")), 0.95, na.rm = TRUE),
                delta_97.5 = quantile(c_across(contains("delta_total")), 0.975, na.rm = TRUE),
                percentage_above_zero = sum(c_across(contains("delta_total"))>0)/length(c_across(contains("delta_total")) )
  ) %>%
  dplyr::ungroup() %>% # stop rowwise
  dplyr::select(-contains("delta_total")) %>%
  dplyr::mutate(yearspan=case_when(
    grepl("1990", decade) ~ 8,
    grepl("2000", decade) ~ 10,
    grepl("2010", decade) ~ 10,
    grepl("1992-2020", decade) ~ 28
  )) %>%
  dplyr::mutate( # total annual change PgC/yr
    annual_mean = delta_mean/yearspan,
    annual_median = delta_median/yearspan,
    annual_mode = delta_mode/yearspan,
    annual_sd = delta_sd/yearspan,
    annual_2.5 = delta_2.5/yearspan,
    annual_5 = delta_5/yearspan,
    annual_95 = delta_95/yearspan,
    annual_97.5 = delta_97.5/yearspan
  ) %>%
  dplyr::mutate( # percentiles on average SOC annual change Mg/ha/yr
    annual_mean_perha = annual_mean*1e9/type_area_ha,
    annual_median_perha = annual_median*1e9/type_area_ha,
    annual_mode_perha = annual_mode*1e9/type_area_ha,
    annual_sd_perha = annual_sd*1e9/type_area_ha,
    annual_2.5_perha = annual_2.5*1e9/type_area_ha,
    annual_5_perha = annual_5*1e9/type_area_ha,
    annual_95_perha = annual_95*1e9/type_area_ha,
    annual_97.5_perha = annual_97.5*1e9/type_area_ha
  )

toc(); print("done: find quantiles for global")

#=================== save pan_biome_CI ===================================
# pan_type_CI_nonNA, pan_global_CI_nonNA
save(pan_region_CI,pan_zone_CI,pan_type_CI,pan_global_CI, 
     file = "./results/upscaled_regional_summary.RData")
print("pan_region_CI,pan_zone_CI,pan_type_CI,pan_global_CI saved to ./results/upscaled_regional_summary.RData")

# save distribution of regional, zone, type, global numbers
save(sum_delta_combined, sum_delta_combined_zone, sum_delta_combined_type, sum_delta_global,
     file = "./results/upscaled_regional_predictions.RData")
print("sum_delta_combined, sum_delta_combined_zone, sum_delta_combined_type, sum_delta_global saved to ./results/upscaled_regional_predictions.RData")


#=============== find statistics at pixel level ================================

# young forest
tic(); print("start: young forest pixel summary")
summary_youngforest1990 <- summarize_pixel_result(delta_youngforest1990_df_list, 
                                                  decade_str="1990", type_str="young forest")
summary_youngforest2000 <- summarize_pixel_result(delta_youngforest2000_df_list, 
                                                  decade_str="2000", type_str="young forest")
summary_youngforest2010 <- summarize_pixel_result(delta_youngforest2010_df_list, 
                                                  decade_str="2010", type_str="young forest")
summary_youngforest1992_2020 <- summarize_pixel_result(delta_youngforest_total_df_list, 
                                                       decade_str="1992-2020", type_str="young forest")

# calculate total SOC change per pixel in gigagram
summary_youngforest1990 <- calculate_SOC_change_pixel(summary_youngforest1990, proportion_str="youngforest1990")
summary_youngforest2000 <- calculate_SOC_change_pixel(summary_youngforest2000, proportion_str="youngforest2000")
summary_youngforest2010 <- calculate_SOC_change_pixel(summary_youngforest2010, proportion_str="youngforest2010")
summary_youngforest1992_2020 <- calculate_SOC_change_pixel(summary_youngforest1992_2020, proportion_str="avg_youngforest1992_2020")

toc(); print("done: young forest pixel summary")

# old forest
tic(); print("start: old forest pixel summary")
summary_oldforest1990 <- summarize_pixel_result(delta_oldforest1990_df_list, 
                                                decade_str="1990", type_str="old forest")
summary_oldforest2000 <- summarize_pixel_result(delta_oldforest2000_df_list, 
                                                decade_str="2000", type_str="old forest")
summary_oldforest2010 <- summarize_pixel_result(delta_oldforest2010_df_list, 
                                                decade_str="2010", type_str="old forest")
summary_oldforest1992_2020 <- summarize_pixel_result(delta_oldforest_total_df_list, 
                                                     decade_str="1992-2020", type_str="old forest")

# calculate total SOC change per pixel in gigagram
summary_oldforest1990 <- calculate_SOC_change_pixel(summary_oldforest1990, proportion_str="oldforest1990")
summary_oldforest2000 <- calculate_SOC_change_pixel(summary_oldforest2000, proportion_str="oldforest2000")
summary_oldforest2010 <- calculate_SOC_change_pixel(summary_oldforest2010, proportion_str="oldforest2010")
summary_oldforest1992_2020 <- calculate_SOC_change_pixel(summary_oldforest1992_2020, proportion_str="avg_oldforest1992_2020")

toc(); print("done: old forest pixel summary")

# grassland
tic(); print("start: grassland pixel summary")
summary_grassland1990 <- summarize_pixel_result(delta_grassland1990_df_list, 
                                                decade_str="1990", type_str="grassland")
summary_grassland2000 <- summarize_pixel_result(delta_grassland2000_df_list, 
                                                decade_str="2000", type_str="grassland")
summary_grassland2010 <- summarize_pixel_result(delta_grassland2010_df_list, 
                                                decade_str="2010", type_str="grassland")
summary_grassland1992_2020 <- summarize_pixel_result(delta_grassland_total_df_list, 
                                                     decade_str="1992-2020", type_str="grassland")

# calculate total SOC change per pixel in gigagram
summary_grassland1990 <- calculate_SOC_change_pixel(summary_grassland1990, proportion_str="grassland1990")
summary_grassland2000 <- calculate_SOC_change_pixel(summary_grassland2000, proportion_str="grassland2000")
summary_grassland2010 <- calculate_SOC_change_pixel(summary_grassland2010, proportion_str="grassland2010")
summary_grassland1992_2020 <- calculate_SOC_change_pixel(summary_grassland1992_2020, proportion_str="grassland1992_2020")

toc(); print("done: grassland pixel summary")

#=================== save pixel level summary ==============
tic()
save(summary_youngforest1990,summary_youngforest2000,summary_youngforest2010,summary_youngforest1992_2020,
     summary_oldforest1990,summary_oldforest2000,summary_oldforest2010,summary_oldforest1992_2020,
     summary_grassland1990,summary_grassland2000,summary_grassland2010,summary_grassland1992_2020,
     file = "./results/upscaled_pixel_summary.RData")
toc()
print("./results/upscaled_pixel_summary.RData saved")

#================== specific SOC sink estimates in main text =============
# # if haven't, load individual posterior predictions by region
# load("./results/upscaled_regional_predictions.RData")
# 
# # if haven't, load function getmode (reposted here: )
# getmode <- function(v) {
#   round_v <- round(v,1)
#   uniqv <- unique(round_v, 1)
#   uniqv[which.max(tabulate(match(round_v, uniqv)))]
# }

#### forest SOC sink (for Fig. 4B)
# aggregate numbers of global - tropical forests- boreal grasslands
sum_delta_global_forest <- sum_delta_combined_zone %>%
  dplyr::filter(type=="young forest" | type=="old forest") %>% # filter for only forest
  dplyr::group_by(decade) %>%
  dplyr::summarize(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
                   across(where(is.character), first)) %>%
  ungroup() %>%
  dplyr::select(-type,-zone)

# find descriptive statistics 
pan_global_forest_CI <- sum_delta_global_forest %>%
  dplyr::rowwise() %>%
  dplyr::mutate(delta_mean =  mean(c_across(contains("delta_total")), na.rm = TRUE),
                delta_median =  median(c_across(contains("delta_total")), na.rm = TRUE),
                delta_mode = getmode(c_across(contains("delta_total"))),
                delta_sd = sd(c_across(contains("delta_total")), na.rm = TRUE),
                delta_2.5 = quantile(c_across(contains("delta_total")), 0.025, na.rm = TRUE),
                delta_5 = quantile(c_across(contains("delta_total")), 0.05, na.rm = TRUE),
                delta_95 = quantile(c_across(contains("delta_total")), 0.95, na.rm = TRUE),
                delta_97.5 = quantile(c_across(contains("delta_total")), 0.975, na.rm = TRUE),
                percentage_above_zero = sum(c_across(contains("delta_total"))>0)/length(c_across(contains("delta_total")) )
  ) %>%
  dplyr::ungroup() %>% # stop rowwise
  dplyr::select(-contains("delta_total")) %>%
  dplyr::mutate(yearspan=case_when(
    grepl("1990", decade) ~ 8,
    grepl("2000", decade) ~ 10,
    grepl("2010", decade) ~ 10,
    grepl("1992-2020", decade) ~ 28
  )) %>%
  dplyr::mutate( # total annual change PgC/yr
    annual_mean = delta_mean/yearspan,
    annual_median = delta_median/yearspan,
    annual_mode = delta_mode/yearspan,
    annual_sd = delta_sd/yearspan,
    annual_2.5 = delta_2.5/yearspan,
    annual_5 = delta_5/yearspan,
    annual_95 = delta_95/yearspan,
    annual_97.5 = delta_97.5/yearspan
  ) %>%
  dplyr::mutate( # percentiles on average SOC annual change Mg/ha/yr
    annual_mean_perha = annual_mean*1e9/zone_area_ha,
    annual_median_perha = annual_median*1e9/zone_area_ha,
    annual_mode_perha = annual_mode*1e9/zone_area_ha,
    annual_sd_perha = annual_sd*1e9/zone_area_ha,
    annual_2.5_perha = annual_2.5*1e9/zone_area_ha,
    annual_5_perha = annual_5*1e9/zone_area_ha,
    annual_95_perha = annual_95*1e9/zone_area_ha,
    annual_97.5_perha = annual_97.5*1e9/zone_area_ha
  )

# 1992-2020, forest SOC sink is 1.58 ± 0.87 PgC/yr

#### SOC sink in old forests and grasslands (for SLAND_soil in Table 2)
# aggregate numbers of global - young forest
sum_delta_global_no_youngforest <- sum_delta_combined_zone %>%
  dplyr::filter(type!="young forest") %>% # filter for only forest
  dplyr::group_by(decade) %>%
  dplyr::summarize(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
                   across(where(is.character), first)) %>%
  ungroup() %>%
  dplyr::select(-type,-zone)

# find descriptive statistics 
pan_global_no_youngforest_CI <- sum_delta_global_no_youngforest %>%
  dplyr::rowwise() %>%
  dplyr::mutate(delta_mean =  mean(c_across(contains("delta_total")), na.rm = TRUE),
                delta_median =  median(c_across(contains("delta_total")), na.rm = TRUE),
                delta_mode = getmode(c_across(contains("delta_total"))),
                delta_sd = sd(c_across(contains("delta_total")), na.rm = TRUE),
                delta_2.5 = quantile(c_across(contains("delta_total")), 0.025, na.rm = TRUE),
                delta_5 = quantile(c_across(contains("delta_total")), 0.05, na.rm = TRUE),
                delta_95 = quantile(c_across(contains("delta_total")), 0.95, na.rm = TRUE),
                delta_97.5 = quantile(c_across(contains("delta_total")), 0.975, na.rm = TRUE),
                percentage_above_zero = sum(c_across(contains("delta_total"))>0)/length(c_across(contains("delta_total")) )
  ) %>%
  dplyr::ungroup() %>% # stop rowwise
  dplyr::select(-contains("delta_total")) %>%
  dplyr::mutate(yearspan=case_when(
    grepl("1990", decade) ~ 8,
    grepl("2000", decade) ~ 10,
    grepl("2010", decade) ~ 10,
    grepl("1992-2020", decade) ~ 28
  )) %>%
  dplyr::mutate( # total annual change PgC/yr
    annual_mean = delta_mean/yearspan,
    annual_median = delta_median/yearspan,
    annual_mode = delta_mode/yearspan,
    annual_sd = delta_sd/yearspan,
    annual_2.5 = delta_2.5/yearspan,
    annual_5 = delta_5/yearspan,
    annual_95 = delta_95/yearspan,
    annual_97.5 = delta_97.5/yearspan
  ) %>%
  dplyr::mutate( # percentiles on average SOC annual change Mg/ha/yr
    annual_mean_perha = annual_mean*1e9/zone_area_ha,
    annual_median_perha = annual_median*1e9/zone_area_ha,
    annual_mode_perha = annual_mode*1e9/zone_area_ha,
    annual_sd_perha = annual_sd*1e9/zone_area_ha,
    annual_2.5_perha = annual_2.5*1e9/zone_area_ha,
    annual_5_perha = annual_5*1e9/zone_area_ha,
    annual_95_perha = annual_95*1e9/zone_area_ha,
    annual_97.5_perha = annual_97.5*1e9/zone_area_ha
  )
# 1992-2020, SOC sink in old forest and grassland is 1.98 ± 0.67 PgC/yr


#### SOC sink excluding tropical forests and grasslands (in "Evaluating global estimates")
# aggregate numbers of global - tropical forests- boreal grasslands
sum_delta_global_exclude <- sum_delta_combined_zone %>%
  dplyr::filter( # filter out tropical forests and boreal grassland
    !(zone=="tropical" & grepl("forest",type)) & !(zone=="boreal" & type=="grassland")) %>%
  dplyr::group_by(decade) %>%
  dplyr::summarize(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
                   across(where(is.character), first)) %>%
  ungroup() %>%
  dplyr::select(-type,-zone)

# find descriptive statistics 
pan_global_exclude_CI <- sum_delta_global_exclude %>%
  dplyr::rowwise() %>%
  dplyr::mutate(delta_mean =  mean(c_across(contains("delta_total")), na.rm = TRUE),
                delta_median =  median(c_across(contains("delta_total")), na.rm = TRUE),
                delta_mode = getmode(c_across(contains("delta_total"))),
                delta_sd = sd(c_across(contains("delta_total")), na.rm = TRUE),
                delta_2.5 = quantile(c_across(contains("delta_total")), 0.025, na.rm = TRUE),
                delta_5 = quantile(c_across(contains("delta_total")), 0.05, na.rm = TRUE),
                delta_95 = quantile(c_across(contains("delta_total")), 0.95, na.rm = TRUE),
                delta_97.5 = quantile(c_across(contains("delta_total")), 0.975, na.rm = TRUE),
                percentage_above_zero = sum(c_across(contains("delta_total"))>0)/length(c_across(contains("delta_total")) )
  ) %>%
  dplyr::ungroup() %>% # stop rowwise
  dplyr::select(-contains("delta_total")) %>%
  dplyr::mutate(yearspan=case_when(
    grepl("1990", decade) ~ 8,
    grepl("2000", decade) ~ 10,
    grepl("2010", decade) ~ 10,
    grepl("1992-2020", decade) ~ 28
  )) %>%
  dplyr::mutate( # total annual change PgC/yr
    annual_mean = delta_mean/yearspan,
    annual_median = delta_median/yearspan,
    annual_mode = delta_mode/yearspan,
    annual_sd = delta_sd/yearspan,
    annual_2.5 = delta_2.5/yearspan,
    annual_5 = delta_5/yearspan,
    annual_95 = delta_95/yearspan,
    annual_97.5 = delta_97.5/yearspan
  ) %>%
  dplyr::mutate( # percentiles on average SOC annual change Mg/ha/yr
    annual_mean_perha = annual_mean*1e9/zone_area_ha,
    annual_median_perha = annual_median*1e9/zone_area_ha,
    annual_mode_perha = annual_mode*1e9/zone_area_ha,
    annual_sd_perha = annual_sd*1e9/zone_area_ha,
    annual_2.5_perha = annual_2.5*1e9/zone_area_ha,
    annual_5_perha = annual_5*1e9/zone_area_ha,
    annual_95_perha = annual_95*1e9/zone_area_ha,
    annual_97.5_perha = annual_97.5*1e9/zone_area_ha
  )
# 1992-2020, SOC sink excluding tropical forests and boreal grassland is 1.47 ± 0.47 PgC/yr
