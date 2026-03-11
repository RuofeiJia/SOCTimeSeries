# find regional numbers by decade by regions in Pan et al. from bootstrapped models
# modified from find_CI_bayesian_cap2_exclude.R
# this one excludes pixels that have extrapolation of predictors based on 
# data without outliers
# also it output prediction of decadal SOC change over the same extent:
# avg_youngforest1992_2020, avg_oldforest1992_2020, grassland1992_2020
# fuctions are different from the other find_CI scripts

rm(list = ls(all = TRUE))

# package and direcotry
library(dplyr)
library(raster)
library(terra)
library(tictoc)
library(sp)

# working directory should be set by default to the folder of entire repo (R project)

#============= load pixel-level prediction results =======================
tic(); print("start: loading results and inputs")

load("./results/post_processing_bayesian_cap2_new_result_brms1_37k12chain.rda")
load("./results/post_processing_bayesian_cap2_new_inputs_brms1_37k12chain.rda")

toc(); print("results loaded")
#========== load proportion_decade_df ===========
tic(); print("start: load propotion_df")
load("./data/proportion_decade_df.rda")

# define boreal, temperate, tropical region
boreal_regions <- c("Canada","Northern Europe","Asian Russia","European Russia", "Alaska")
temperate_regions <- c("Continental United States", "Europe", "China", "Japan and Korea", "Australia", 
                       "New Zealand", "Other temperate","Others")
tropical_regions <- c("Africa", "Mexico and Central America", "South America", "Southeast Asia", "South Asia")

# generate proportion_biome_df with column biome
proportion_zone_df <- proportion_decade_df %>%
  dplyr::mutate(zone = case_when(
    pan_region_new2 %in% boreal_regions ~ "boreal",
    pan_region_new2 %in% temperate_regions ~ "temperate",
    pan_region_new2 %in% tropical_regions ~ "tropical"
  ))

toc(); print("proportion_decade_df loaded") 
#================ load SOC data ============================
tic(); print("start: load SOC data")
# load data
load("./data/SOC_withNewData_for_analysis.rda")
df_std <- df_std_withNewData %>% filter(!is.na(siteID)); rm(df_std_withNewData)

toc(); print("done: load SOC data")
#================ load functions =====================
tic(); print("start: loading functions")

# function to get mode (https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
getmode <- function(v) {
  round_v <- round(v,1)
  uniqv <- unique(round_v, 1)
  uniqv[which.max(tabulate(match(round_v, uniqv)))]
}

# find min max of predictors without outliers (used in exclude_extrapolation)
find_min_wo_outliers <- function(numbers) {
  quantile <- quantile(numbers) # returns 0%, 25%, 50%, 75%, 100%
  iqr <- quantile[4] - quantile[2]
  min <- max(quantile[2] - 1.5*iqr, quantile[1])
  return(min)
}
find_max_wo_outliers <- function(numbers) {
  quantile <- quantile(numbers) # returns 0%, 25%, 50%, 75%, 100%
  iqr <- quantile[4] - quantile[2]
  max <- min(quantile[4] + 1.5*iqr, quantile[5])
  return(max)
}

# function that calculate extrapolation to exclude pixels based on predictor values and filter by land cover proportion
# return all pixels with that proportion>0, and extrapolation columns
calculate_input_extrapolation <- function(input_df, df=df_std, land_cover_str="young forest", 
                                   proportion_str="avg_youngforest1992_2020") {
    
    # find min max predictors for this land cover
    df_cond <- df %>% dplyr::filter(forest_cond_gf_besnard60==land_cover_str)
    
    max_MAT <- find_max_wo_outliers(df_cond$MAT_cru)
    min_MAT <- find_min_wo_outliers(df_cond$MAT_cru)
    max_MAP <- find_max_wo_outliers(df_cond$MAP_cru)
    min_MAP <- find_min_wo_outliers(df_cond$MAP_cru)
    max_n <- find_max_wo_outliers(df_cond$n_pct_plo)
    min_n <- find_min_wo_outliers(df_cond$n_pct_plo)
    max_clay <- find_max_wo_outliers(df_cond$clay_plo)
    min_clay <- find_min_wo_outliers(df_cond$clay_plo)
    max_ph <- find_max_wo_outliers(df_cond$ph_plo)
    min_ph <- find_min_wo_outliers(df_cond$ph_plo)
    
    # crop pixels that require extrapolation
    input_df_new1 <- input_df %>%
      dplyr::mutate(
        MAT_extrapolation = ifelse(MAT_cru>=max_MAT|MAT_cru<=min_MAT, T, F),
        MAP_extrapolation = ifelse(MAP_cru>=max_MAP|MAP_cru<=min_MAP, T, F),
        n_extrapolation = ifelse(n_pct_plo>=max_n|n_pct_plo<=min_n, T, F),
        clay_extrapolation = ifelse(clay_plo>=max_clay|clay_plo<=min_clay, T, F),
        ph_extrapolation = ifelse(ph_plo>=max_ph|ph_plo<=min_ph, T, F)
      ) %>%
      rowwise() %>%
      dplyr::mutate(
        extrapolation_fraction = sum(MAT_extrapolation,MAP_extrapolation,n_extrapolation,
                                     clay_extrapolation,ph_extrapolation) #,
        #extrapolation_variable = paste(c("MAT", "MAP", "N", "clay", "pH")[c_across(MAT_extrapolation:ph_extrapolation)], collapse=",")
      ) %>% ungroup()
    
    # filter pixels by land cover proportion
    
    # get temporary proportion_decade_df
    proportion_df_temp <- proportion_decade_df
    # rename  relevant column to "proportion"
    colnames(proportion_df_temp)[which(colnames(proportion_df_temp)==proportion_str)] <- "proportion"
    # filter by proportion
    input_df_new2 <- merge(input_df_new1, 
                           proportion_df_temp[,c("x","y","proportion","pan_region_new2")],
                           by=c("x","y"), all.x=T) %>% 
      filter(proportion>0)
    
    return(input_df_new2)
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
      dplyr::summarize(delta_total = sum(col_temp*cellsize_ha*proportion, na.rm=T)/1e9 ) # Mg into Pg/yr
    
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
    dplyr::summarize(region_area_ha= sum(cellsize_ha*proportion) )
  
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
      dplyr::summarize(delta_total = sum(col_temp*cellsize_ha*proportion, na.rm=T)/1e9 ) # Mg into Pg/yr
    
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
    dplyr::summarize(zone_area_ha= sum(cellsize_ha*proportion) )
  
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
      dplyr::summarize(delta_total = sum(col_temp*cellsize_ha*proportion, na.rm=T)/1e9 ) # Mg into Pg/yr
    
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
    dplyr::summarize(type_area_ha= sum(cellsize_ha*proportion) )
  
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

#-----------prepare input_dfs with only predicted pixels-----------------------------
print("start: prepare input_dfs with only predicted pixels"); tic()

input_youngforest_all <- calculate_input_extrapolation(input_youngforest1990_first, 
                                                       land_cover_str="young forest", 
                                                       proportion_str="avg_youngforest1992_2020")

input_oldforest_all <- calculate_input_extrapolation(input_oldforest1990_first, 
                                                     land_cover_str="old forest", 
                                                     proportion_str="avg_oldforest1992_2020")

input_grassland_all <- calculate_input_extrapolation(input_grassland1990_first, 
                                                     land_cover_str="grassland", 
                                                     proportion_str="grassland1992_2020")
# filter for pixels without extrapolation
input_youngforest_pred <- input_youngforest_all %>% filter(extrapolation_fraction==0)
input_oldforest_pred <- input_oldforest_all %>% filter(extrapolation_fraction==0)
input_grassland_pred <- input_grassland_all %>% filter(extrapolation_fraction==0)

print("done: prepare input_dfs with only predicted pixels"); toc()

# save predictor condition of predicted pixels for each land cover
# ..._all are all pixels that should be predicted if sufficient data
# ..._pred are pixels that ended up being predicted because they fall within environmental space supported by observations
save(input_youngforest_all, input_oldforest_all, input_grassland_all,
     input_youngforest_pred, input_oldforest_pred, input_grassland_pred,
     file="./results/predicted_inputs_brms1_37k12chain.rda"
     )
print("input_df_pred saved in ./results/predicted_inputs_brms1_37k12chain.rda")
#==================== compile results ==========================================
tic(); print("start: compile results")
# young forest
delta_youngforest1990_df_list <- merge(input_youngforest_pred[c("x", "y")], delta_youngforest1990_df_listb, by=c("x","y"), all.x=T); rm(delta_youngforest1990_df_listb)
delta_youngforest2000_df_list <- merge(input_youngforest_pred[c("x", "y")], delta_youngforest2000_df_listb, by=c("x","y"), all.x=T); rm(delta_youngforest2000_df_listb)
delta_youngforest2010_df_list <- merge(input_youngforest_pred[c("x", "y")], delta_youngforest2010_df_listb, by=c("x","y"), all.x=T); rm(delta_youngforest2010_df_listb)
delta_youngforest_total_df_list <- merge(input_youngforest_pred[c("x", "y")], delta_youngforest_total_df_listb, by=c("x","y"), all.x=T); rm(delta_youngforest_total_df_listb)

# old forest
delta_oldforest1990_df_list <- merge(input_oldforest_pred[c("x", "y")], delta_oldforest1990_df_listb, by=c("x","y"), all.x=T); rm(delta_oldforest1990_df_listb)
delta_oldforest2000_df_list <- merge(input_oldforest_pred[c("x", "y")], delta_oldforest2000_df_listb, by=c("x","y"), all.x=T); rm(delta_oldforest2000_df_listb)
delta_oldforest2010_df_list <- merge(input_oldforest_pred[c("x", "y")], delta_oldforest2010_df_listb, by=c("x","y"), all.x=T); rm(delta_oldforest2010_df_listb)
delta_oldforest_total_df_list <- merge(input_oldforest_pred[c("x", "y")], delta_oldforest_total_df_listb, by=c("x","y"), all.x=T); rm(delta_oldforest_total_df_listb)

# grassland
delta_grassland1990_df_list <- merge(input_grassland_pred[c("x", "y")], delta_grassland1990_df_listb, by=c("x","y"), all.x=T); rm(delta_grassland1990_df_listb)
delta_grassland2000_df_list <- merge(input_grassland_pred[c("x", "y")], delta_grassland2000_df_listb, by=c("x","y"), all.x=T); rm(delta_grassland2000_df_listb)
delta_grassland2010_df_list <- merge(input_grassland_pred[c("x", "y")], delta_grassland2010_df_listb, by=c("x","y"), all.x=T); rm(delta_grassland2010_df_listb)
delta_grassland_total_df_list <- merge(input_grassland_pred[c("x", "y")], delta_grassland_total_df_listb, by=c("x","y"), all.x=T); rm(delta_grassland_total_df_listb)

toc(); print("result lists compiled")

#================== sum regional numbers from bootstrapped models ==============
tic(); print("start: sum pixels by model by pan_region_new2")
# young forest
sum_youngforest1990_delta_all <- sum_by_region(delta_youngforest1990_df_list, proportion_str="avg_youngforest1992_2020",
                                               type_str="young forest", decade_str="1990")
sum_youngforest2000_delta_all <- sum_by_region(delta_youngforest2000_df_list, proportion_str="avg_youngforest1992_2020",
                                               type_str="young forest", decade_str="2000")
sum_youngforest2010_delta_all <- sum_by_region(delta_youngforest2010_df_list, proportion_str="avg_youngforest1992_2020",
                                               type_str="young forest", decade_str="2010")
sum_youngforest1992_2020_delta_all <- sum_by_region(delta_youngforest_total_df_list, proportion_str="avg_youngforest1992_2020",
                                                    type_str="young forest", decade_str="1992-2020")

# old forest
sum_oldforest1990_delta_all <- sum_by_region(delta_oldforest1990_df_list, proportion_str="avg_oldforest1992_2020",
                                             type_str="old forest", decade_str="1990")
sum_oldforest2000_delta_all <- sum_by_region(delta_oldforest2000_df_list, proportion_str="avg_oldforest1992_2020",
                                             type_str="old forest", decade_str="2000")
sum_oldforest2010_delta_all <- sum_by_region(delta_oldforest2010_df_list, proportion_str="avg_oldforest1992_2020",
                                             type_str="old forest", decade_str="2010")
sum_oldforest1992_2020_delta_all <- sum_by_region(delta_oldforest_total_df_list, proportion_str="avg_oldforest1992_2020",
                                                  type_str="old forest", decade_str="1992-2020")
# test
test <- sum_oldforest1992_2020_delta_all %>% 
  dplyr::filter(type=="old forest" & decade=="1992-2020" & pan_region_new2=="Asian Russia")
print(paste0("test: Asian Russia old forest 1992-2020 area is ", test$region_area_ha))

# grassland
sum_grassland1990_delta_all <- sum_by_region(delta_grassland1990_df_list, proportion_str="grassland1992_2020",
                                             type_str="grassland", decade_str="1990")
sum_grassland2000_delta_all <- sum_by_region(delta_grassland2000_df_list, proportion_str="grassland1992_2020",
                                             type_str="grassland", decade_str="2000")
sum_grassland2010_delta_all <- sum_by_region(delta_grassland2010_df_list, proportion_str="grassland1992_2020",
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

#================== sum zone numbers from bootstrapped models =================
tic(); print("start: sum pixels by model by zone")
# young forest
sum_youngforest1990_delta_zone_all <- sum_by_zone(delta_youngforest1990_df_list, proportion_str="avg_youngforest1992_2020",
                                                  type_str="young forest", decade_str="1990")
sum_youngforest2000_delta_zone_all <- sum_by_zone(delta_youngforest2000_df_list, proportion_str="avg_youngforest1992_2020",
                                                  type_str="young forest", decade_str="2000")
sum_youngforest2010_delta_zone_all <- sum_by_zone(delta_youngforest2010_df_list, proportion_str="avg_youngforest1992_2020",
                                                  type_str="young forest", decade_str="2010")
sum_youngforest1992_2020_delta_zone_all <- sum_by_zone(delta_youngforest_total_df_list, proportion_str="avg_youngforest1992_2020",
                                                       type_str="young forest", decade_str="1992-2020")

# old forest
sum_oldforest1990_delta_zone_all <- sum_by_zone(delta_oldforest1990_df_list, proportion_str="avg_oldforest1992_2020",
                                                type_str="old forest", decade_str="1990")
sum_oldforest2000_delta_zone_all <- sum_by_zone(delta_oldforest2000_df_list, proportion_str="avg_oldforest1992_2020",
                                                type_str="old forest", decade_str="2000")
sum_oldforest2010_delta_zone_all <- sum_by_zone(delta_oldforest2010_df_list, proportion_str="avg_oldforest1992_2020",
                                                type_str="old forest", decade_str="2010")
sum_oldforest1992_2020_delta_zone_all <- sum_by_zone(delta_oldforest_total_df_list, proportion_str="avg_oldforest1992_2020",
                                                     type_str="old forest", decade_str="1992-2020")

# grassland
sum_grassland1990_delta_zone_all <- sum_by_zone(delta_grassland1990_df_list, proportion_str="grassland1992_2020",
                                                type_str="grassland", decade_str="1990")
sum_grassland2000_delta_zone_all <- sum_by_zone(delta_grassland2000_df_list, proportion_str="grassland1992_2020",
                                                type_str="grassland", decade_str="2000")
sum_grassland2010_delta_zone_all <- sum_by_zone(delta_grassland2010_df_list, proportion_str="grassland1992_2020",
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
#================== sum type (global) number from bootstrapped models ==========
tic(); print("start: sum pixels by model by type")
# young forest
sum_youngforest1990_delta_type_all <- sum_by_type(delta_youngforest1990_df_list, proportion_str="avg_youngforest1992_2020",
                                                  type_str="young forest", decade_str="1990")
sum_youngforest2000_delta_type_all <- sum_by_type(delta_youngforest2000_df_list, proportion_str="avg_youngforest1992_2020",
                                                  type_str="young forest", decade_str="2000")
sum_youngforest2010_delta_type_all <- sum_by_type(delta_youngforest2010_df_list, proportion_str="avg_youngforest1992_2020",
                                                  type_str="young forest", decade_str="2010")
sum_youngforest1992_2020_delta_type_all <- sum_by_type(delta_youngforest_total_df_list, proportion_str="avg_youngforest1992_2020",
                                                       type_str="young forest", decade_str="1992-2020")

# old forest
sum_oldforest1990_delta_type_all <- sum_by_type(delta_oldforest1990_df_list, proportion_str="avg_oldforest1992_2020",
                                                type_str="old forest", decade_str="1990")
sum_oldforest2000_delta_type_all <- sum_by_type(delta_oldforest2000_df_list, proportion_str="avg_oldforest1992_2020",
                                                type_str="old forest", decade_str="2000")
sum_oldforest2010_delta_type_all <- sum_by_type(delta_oldforest2010_df_list, proportion_str="avg_oldforest1992_2020",
                                                type_str="old forest", decade_str="2010")
sum_oldforest1992_2020_delta_type_all <- sum_by_type(delta_oldforest_total_df_list, proportion_str="avg_oldforest1992_2020",
                                                     type_str="old forest", decade_str="1992-2020")

# grassland
sum_grassland1990_delta_type_all <- sum_by_type(delta_grassland1990_df_list, proportion_str="grassland1992_2020",
                                                type_str="grassland", decade_str="1990")
sum_grassland2000_delta_type_all <- sum_by_type(delta_grassland2000_df_list, proportion_str="grassland1992_2020",
                                                type_str="grassland", decade_str="2000")
sum_grassland2010_delta_type_all <- sum_by_type(delta_grassland2010_df_list, proportion_str="grassland1992_2020",
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
save(pan_region_CI,pan_zone_CI,pan_type_CI,pan_global_CI, file = "./results/find_CI_bayesian_cap2_exclude_same_extent.RData")
print("pan_region_CI,pan_zone_CI,pan_type_CI,pan_global_CI saved to ./results/find_CI_bayesian_cap2_exclude_same_extent.RData")
# in repo, saved as "upscaled_regional_summary.RData"

# save distribution of regional, zone, type, global numbers
save(sum_delta_combined, sum_delta_combined_zone, sum_delta_combined_type, sum_delta_global,
     file = "./results/find_CI_bayesian_cap2_exclude_same_extent_numbers.RData")
print("sum_delta_combined, sum_delta_combined_zone, sum_delta_combined_type, sum_delta_global saved to ./results/find_CI_bayesian_cap2_exclude_same_extent_numbers.RData")
# in repo, saved as "upscaled_regional_predictions.RData"

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
summary_youngforest1990 <- calculate_SOC_change_pixel(summary_youngforest1990, proportion_str="avg_youngforest1992_2020")
summary_youngforest2000 <- calculate_SOC_change_pixel(summary_youngforest2000, proportion_str="avg_youngforest1992_2020")
summary_youngforest2010 <- calculate_SOC_change_pixel(summary_youngforest2010, proportion_str="avg_youngforest1992_2020")
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
summary_oldforest1990 <- calculate_SOC_change_pixel(summary_oldforest1990, proportion_str="avg_oldforest1992_2020")
summary_oldforest2000 <- calculate_SOC_change_pixel(summary_oldforest2000, proportion_str="avg_oldforest1992_2020")
summary_oldforest2010 <- calculate_SOC_change_pixel(summary_oldforest2010, proportion_str="avg_oldforest1992_2020")
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
summary_grassland1990 <- calculate_SOC_change_pixel(summary_grassland1990, proportion_str="grassland1992_2020")
summary_grassland2000 <- calculate_SOC_change_pixel(summary_grassland2000, proportion_str="grassland1992_2020")
summary_grassland2010 <- calculate_SOC_change_pixel(summary_grassland2010, proportion_str="grassland1992_2020")
summary_grassland1992_2020 <- calculate_SOC_change_pixel(summary_grassland1992_2020, proportion_str="grassland1992_2020")

toc(); print("done: grassland pixel summary")

#=================== save pixel level summary ==============
tic()
save(summary_youngforest1990,summary_youngforest2000,summary_youngforest2010,summary_youngforest1992_2020,
     summary_oldforest1990,summary_oldforest2000,summary_oldforest2010,summary_oldforest1992_2020,
     summary_grassland1990,summary_grassland2000,summary_grassland2010,summary_grassland1992_2020,
     file = "./results/find_CI_bayesian_cap2_exclude_same_extent_pixel_summary.RData")
toc()
print("./results/find_CI_bayesian_cap2_exclude_same_extent_pixel_summary.RData saved")
# in repo, saved as "upscaled_pixel_summary.RData"


#================== specific SOC sink estimates in main text =============
load("./results/find_CI_bayesian_cap2_exclude_same_extent_numbers.RData")

#### forest SOC sink (for Fig. 3B)
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

# 1992-2020, forest SOC sink is 1.39 ± 0.26 PgC/yr

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
# 1992-2020, SOC sink in old forest and grassland is 1.96 ± 0.24 PgC/yr

