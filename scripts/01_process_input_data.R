# clean, gapfill, and process the global SOC time series dataset (<filename>)
# into a dataframe that is ready for analysis (<filename>)

# raw dataset compiled from other sources is saved in this repo in "data" folder as ""
# intermediate steps were saved during course of this project but not stored in this repo
# the final processed dataset is saved in the repo in "data" folder as ""

# working directory should be set by default to the folder of entire repo (R project)
getwd() # check

# libraries
library(tidyverse)
library (dplyr)
library(raster)
library(tictoc)

# working directory should be set by default to the folder of entire repo (R project)

# load raw dataset (object name for dataframe is df)
load("./data/SOC_time_series_database.rda") 

#------ 0. functions -----------------------------------------------------------
# all functions needed for 01_process_input_data.R
# in order of appearance in 01_process_input_data.R
# most functions are used to extract and gapfill covariates from spatial data products
# other functions are used to process/clean data surrounding bulk density

# gapfill existing BD data to same plot, other time
clean_gapfill_BD <- function(df) {
  
  #====================== exclude erroneous BD values ==========================
  # (1) for drained soil, exclude value beyond threshold
  # BD_soil: 0.01-2.86 (US FIA threshold)
  # BD_fine: 0.03-1.81 (Pacini et al. 2023, woodland+grassland+shrubland min max from LUCAS data)
  BDsoil_min <- 0.01; BDsoil_max <- 2.86
  BDfine_min <- 0.03; BDfine_max <- 2.6 # more leniently? assume fine earth can't be denser than coarse fragment?
  
  df_new <- df %>%
    dplyr::mutate( # assign whether a BD is beyond threshold
      BD_threshold = case_when(
        lower_depth > 21 ~ "No", # LUCAS threshold apply only to top 20cm, US FIA threshold apply only to top 8 inch
        grepl("marsh|bog|water", land_cover) ~ "No", # if water saturated soil, keep BD value
        BD_def=="fine earth" & (BD<BDfine_min | BD>BDfine_max) ~ "Yes", # else if BD fine beyond threshold, exclude
        BD_def=="whole soil" & (BD<BDsoil_min | BD>BDsoil_max) ~ "Yes", # else if BD soil beyond threshold, exclude
        TRUE ~ "No" # else, keep original BD values
      )) %>% group_by(plotID, observation_year) %>%
    dplyr::mutate( # based on threshold column, if a profile has suspicious value, exclude value from that profile
      BD_threshold_profile = ifelse("Yes" %in% unique(BD_threshold), "Yes", "No")
    ) %>% ungroup() %>%
    dplyr::mutate( 
      BD = ifelse(BD_threshold_profile=="Yes", NA, BD),
      BD_eff = ifelse(BD_threshold_profile=="Yes", NA, BD_eff)
    ) %>% arrange(plotID, observation_year, upper_depth)
  
  # (2) exclude BD in profiles that show too much change over time, keep the most recent record
  # because older data is more likely to have error during allocation
  
  # add column ratio_bottom_BD, ratio of max bottom comparable BD across time for each plot
  BD_changes <- df_new %>% filter(!grepl("stock", c_var)) %>% # take out stock rows
    group_by(plotID, observation_year) %>%
    dplyr::filter(sum(!is.na(BD))>0) %>% # take out plots with no BD at all
    arrange(upper_depth) %>%
    dplyr::mutate(
      lowest_bd_profile = max(upper_depth[!is.na(BD)], na.rm = TRUE) # profile level lower depth with a !is.na(BD)
    ) %>% ungroup() %>% group_by(plotID) %>% 
    dplyr::mutate( # get lowest comparable BD by upper_depth
      lowest_bd_plot = min(lowest_bd_profile)
    ) %>% # filter out rows that are deeper than plot-level comparable lowest_bd layer
    dplyr::filter(upper_depth<=lowest_bd_plot) %>% 
    group_by(plotID, observation_year) %>% arrange(upper_depth) %>%
    dplyr::mutate(
      bottom_BD = last(BD)
    ) %>%
    ungroup() %>% group_by(plotID) %>%
    dplyr::mutate(
      ratio_bottom_BD=max(bottom_BD)/min(bottom_BD)
    ) %>% # mutate most recent year to keep BD if there is extreme BD change
    dplyr::mutate(
      recent_BD_year = max(observation_year)
    ) %>%
    dplyr::select(plotID,ratio_bottom_BD,recent_BD_year) %>%
    distinct() %>% ungroup() # output a list of plotID with bottom BD changes and the year which BD to keep
  
  # merge BD_changes with df
  df_new2 <- merge(x=df_new, y=BD_changes, by="plotID", all.x=T)
  
  # exclude the BDs from plots in BD_extreme_change
  # (for the plots that have extreme BD change, keep the most recent year with BDs)
  bottom_BD_ratio_max <- 2 # set max bottom_BD_ratio threshold
  df_new3 <- df_new2 %>%
    dplyr::mutate(
      BD = case_when(
        ratio_bottom_BD>=bottom_BD_ratio_max & observation_year!=recent_BD_year ~ NA,
        TRUE ~ BD
      ),
      BD_eff = case_when(
        ratio_bottom_BD>=bottom_BD_ratio_max & observation_year!=recent_BD_year ~ NA,
        TRUE ~ BD_eff
      )
    )
  
  #====================== gapfill BD from existing data across time ============
  # first, gapfill BD from identical soil depths in each plotID
  # second, gapfill BD from non-identical soil depths (gapfill if avg depth of BD layer is within the interval of layer without BD)
  df_new4 <- data.frame()
  for (plot in unique(df_new3$plotID)) {
    plot_df <- df_new3[df_new3$plotID==plot,] %>% mutate(BD_from_data = NA, BD_eff_from_data=NA)
    
    if ( sum(is.na(plot_df$BD))>0 & sum(is.na(plot_df$BD))<nrow(plot_df) ) { # if there are some BD but some NA
      #print(plot)
      plot_bd_df <- distinct(plot_df[!is.na(plot_df$BD),], observation_year, upper_depth, lower_depth, BD, BD_eff)
      plot_bd_df$avg_depth <- (plot_bd_df$upper_depth + plot_bd_df$lower_depth)/2
      for ( i in which(is.na(plot_df$BD)) ) {# for each is.na(BD), find best value in plot_bd_df to fill in
        na_upper_depth <- plot_df$upper_depth[i]
        na_lower_depth <- plot_df$lower_depth[i]
        na_avg_depth <- (na_upper_depth + na_lower_depth)/2
        na_year <- plot_df$observation_year[i]
        closest_year <- plot_bd_df$observation_year[which.min(abs(plot_bd_df$observation_year - na_year))]
        closest_BD <- plot_bd_df %>% filter(observation_year==closest_year) %>%
          slice(which.min(abs(avg_depth - na_avg_depth)))
        
        if(closest_BD$avg_depth<=na_lower_depth & closest_BD$avg_depth>=na_upper_depth) {
          print(plot); print(i)
          # if average depth of BD_layer is within the depth range of na layer
          plot_df$BD_from_data[i] <- closest_BD$BD # assign closest BD
          plot_df$BD_eff_from_data[i] <- closest_BD$BD_eff # assign closest BD_eff
        }
      }
    }
    df_new4 <- rbind(df_new4, plot_df)
  }
  
  sum(is.na(df_new4$BD) & is.na(df_new4$BD_from_data)) # 5250
  sum(is.na(df_new4$BD_eff) & is.na(df_new4$BD_eff_from_data)) # 6137
  
  df_new4 <- df_new4 %>% arrange(siteID,plotID,observation_year,upper_depth)
  
  return(df_new4) # return the newest df name
}

# extract bulk density from soilgrids to gapfill dataset
extract_bulk_density <- function(df) {
  library(raster)
  
  # load raster !!!! rename after downloading
  bd05 <- raster("./data/fine_bulk_density_0-5cm_SoilGrids2.tif")
  bd515 <- raster("./data/fine_bulk_density_5-15cm_SoilGrids2.tif")
  bd1530 <- raster("./data/fine_bulk_density_15-30cm_SoilGrids2.tif")
  bd3060 <- raster("./data/fine_bulk_density_30-60cm_SoilGrids2.tif")
  bd60100 <- raster("./data/fine_bulk_density_60-100cm_SoilGrids2.tif")
  bd100200 <- raster("./data/fine_bulk_density_100-200cm_SoilGrids2.tif")
  
  
  # sample raster
  df[c("lat","long")] <- sapply(df[c("lat","long")], as.double)
  points <- dplyr::select(df, lat, long)  # add layer ID?
  coordinates(points) <- c("long","lat")
  
  bd.df <- data.frame(
    bd0_5=(raster::extract(bd05,points))/100, # original unit cg/cm³. cg/cm³/100=1 g/cm3
    bd5_15=(raster::extract(bd515,points))/100,
    bd15_30=(raster::extract(bd1530,points))/100,
    bd30_60=(raster::extract(bd3060,points))/100,
    bd60_100=(raster::extract(bd60100,points))/100,
    bd100_200=(raster::extract(bd100200,points))/100    
  )
  
  # assign bd to each row based on layer average depth
  bd.df <- bd.df %>% 
    mutate( depth_avg=(df$upper_depth + df$lower_depth)*0.5 ) %>%
    mutate(
      bd_sg = case_when(
        depth_avg<=5 ~ bd0_5,
        depth_avg>5 & depth_avg<=15 ~ bd5_15,
        depth_avg>15 & depth_avg<=30 ~ bd15_30,
        depth_avg>30 & depth_avg<=60 ~ bd30_60,
        depth_avg>60 & depth_avg<=100 ~ bd60_100,
        depth_avg>100 & depth_avg<=200 ~ bd100_200,
        TRUE ~ NA
      )
    )
  
  # compile column to df
  df <- df %>%
    mutate(bd_fine_sg = bd.df$bd_sg)
  
  return(df)
  
}

# function that add column "coarse_fragment_vol"
extract_coarse_fragment_vol <- function(df) {
  library(raster)
  
  # load coarse fragment 0-5cm from soilgrids
  cfvo05 <- raster("./data/coarse_fragment_vol_0-5cm_SoilGrids2.tif")
  cfvo515 <- raster("./data/coarse_fragment_vol_5-15cm_SoilGrids2.tif")
  cfvo1530 <- raster("./data/coarse_fragment_vol_15-30cm_SoilGrids2.tif")
  cfvo3060 <- raster("./data/coarse_fragment_vol_30-60cm_SoilGrids2.tif")
  cfvo60100 <- raster("./data/coarse_fragment_vol_60-100cm_SoilGrids2.tif")
  cfvo100200 <- raster("./data/coarse_fragment_vol_100-200cm_SoilGrids2.tif")
  
  # sample them into cf_df
  df[c("lat","long")] <- sapply(df[c("lat","long")], as.double)
  points <- dplyr::select(df, lat, long)  # add layer ID?
  coordinates(points) <- c("long","lat")
  cf_df <- data.frame(cfvo0_5=(raster::extract(cfvo05,points))/1000,
                      cfvo5_15=(raster::extract(cfvo515,points))/1000,
                      cfvo15_30=(raster::extract(cfvo1530,points))/1000,
                      cfvo30_60=(raster::extract(cfvo3060,points))/1000,
                      cfvo60_100=(raster::extract(cfvo60100,points))/1000,
                      cfvo100_200=(raster::extract(cfvo100200,points))/1000
  ) #soilgrids unit cm3/dm3 1/thousand. convert to fraction
  
  # assign coarse fragment (vol) to each row based on layer average depth
  cf_df <- cf_df %>% 
    mutate( depth_avg=(df$upper_depth + df$lower_depth)*0.5 ) %>%
    mutate(
      cf_vol = case_when(
        depth_avg<=5 ~ cfvo0_5,
        depth_avg>5 & depth_avg<=15 ~ cfvo5_15,
        depth_avg>15 & depth_avg<=30 ~ cfvo15_30,
        depth_avg>30 & depth_avg<=60 ~ cfvo30_60,
        depth_avg>60 & depth_avg<=100 ~ cfvo60_100,
        depth_avg>100 & depth_avg<=200 ~ cfvo100_200,
        TRUE ~ NA
      )
    )
  
  # compile coarse fragment column to df
  df <- df %>%
    mutate(cf_vol_sg = cf_df$cf_vol)
  
  return(df)
}

# extract topsoil clay percentage from soilgrids to gapfill dataset
extract_clay <- function(df) {
  # load clay05
  clay05 <- raster("./data/clay05cm_0p25_SoilGrids2.tif")
  
  # extract points
  df[c("lat","long")] <- sapply(df[c("lat","long")], as.double)
  points <- dplyr::select(df, lat, long)  # add layer ID?
  coordinates(points) <- c("long","lat")
  clay.df <- data.frame(clay=(raster::extract(clay05,points))/10) #soilgrids clay is in g/kg, convert to %
  
  # add column clay from soilgrids in df
  df$clay_sg <- clay.df$clay
  return(df)
  
}

# extract topsoil pH from soilgrids to gapfill dataset
extract_ph <- function(df) {
  # load topsoil ph
  ph05 <- raster("./data/ph05cm_SoilGrids2.tif")
  
  # extract points
  df[c("lat","long")] <- sapply(df[c("lat","long")], as.double)
  points <- dplyr::select(df, lat, long)  # add layer ID?
  coordinates(points) <- c("long","lat")
  ph.df <- data.frame(ph=(raster::extract(ph05,points))/10) # soilgrids phh2o unit is PH*10, convert to ph
  
  # add column clay from soilgrids in df
  df$ph_sg <- ph.df$ph
  return(df)
  
}

# extract topsoil n% from soilgrids to gapfill dataset
extract_n<- function(df) {
  
  # load n05
  n05 <- raster("./data/n05cm_SoilGrids2.tif")
  
  # extract points
  df[c("lat","long")] <- sapply(df[c("lat","long")], as.double)
  points <- dplyr::select(df, lat, long)  # add layer ID?
  coordinates(points) <- c("long","lat")
  n.df <- data.frame(n=(raster::extract(n05,points))/1000) #soilgrids clay is in cg/kg, convert to %
  
  # add column in df
  df$n_pct_sg <- n.df$n
  return(df)
  
}

# extract MAT and MAP of observation years from CRUTS datam, and mean MAT MAP during observed period
# (add df$MAT_cru, df$MAP_cru, df$mean_mat_site, df$mean_map_site)
extract_CRU <- function(df) {
  library(plyr)
  library(cruts)
  
  # import latest cru_df
  load("./data/cru_df.rda")
  
  # set cru_df points
  old_points <- distinct(cru_df[c("long","lat")])
  
  # set new points
  points <- dplyr::select(df, lat, long) %>% mutate(
    lat = as.double(lat),
    long = as.double(long)
  ) %>% distinct()
  
  new_points <- setdiff(points, old_points) # in points but not in old_points
  
  # if there is new points, update cru_df
  if (nrow(new_points)>0) {
    coordinates(new_points) <- c("long","lat")
    
    temp_file <- "./data/cru_ts4.07.1901.2022.tmp.dat.nc"
    prec_file <- "./data/cru_ts4.07.1901.2022.pre.dat.nc"
    
    # set new rows for cru_df (if additional points are added since cru_df was created)
    new_cru_df <- data.frame(new_points)
    for (year in 1901:2022) {
      time_range <- c(paste0(year,"-01-01"), paste0(year,"-12-31"))
      
      temp <- cruts2raster(temp_file, timeRange=time_range)
      mat <- mean(temp)
      mat.df <- data.frame(mat=(raster::extract(mat,new_points)))
      colnames(mat.df) <- paste0("mat_", year) 
      
      prec <- cruts2raster(prec_file, timeRange=time_range)
      map <- preSum <- sum (prec) # Sum precipitation
      map.df <- data.frame(map=(raster::extract(map,new_points)))
      colnames(map.df) <- paste0("map_", year) 
      
      new_cru_df <- cbind(new_cru_df, mat.df, map.df)
    }
    
    # make updated cru_df and save
    cru_df <- rbind(cru_df, new_cru_df) %>% 
      dplyr::select(lat,long,starts_with("mat_"), starts_with("map_"))
    save(cru_df, file="./data/cru_df.rda")
    write.csv(cru_df, "./data/cru_df.csv")
  }
  
  # add MAT_cru, MAP_cru (MAT, MAP of observed year) in df
  df$MAT_cru <- sapply(1:nrow(df), function(i) {
    temp_lat <- df$lat[i]
    temp_long <- df$long[i]
    obs_year <- df$observation_year[i]
    cru_row <- cru_df %>% filter(lat == temp_lat & long == temp_long)
    if (nrow(cru_row) > 0 & obs_year>=1901) {
      sample(cru_row[paste0("mat_",obs_year)], 1)[[1]]
    } else {
      NA
    }
  })
  
  df$MAP_cru <- sapply(1:nrow(df), function(i) {
    temp_lat <- df$lat[i]
    temp_long <- df$long[i]
    obs_year <- df$observation_year[i]
    cru_row <- cru_df %>% filter(lat == temp_lat & long == temp_long)
    if (nrow(cru_row) > 0 & obs_year>=1901) {
      sample(cru_row[paste0("map_",obs_year)], 1)[[1]]
    } else {
      NA
    }
  })
  
  # rename CRU columns to distinguish later
  colnames(cru_df)[3:ncol(cru_df)] <- paste0("cru", colnames(cru_df)[3:ncol(cru_df)])
  
  # join df and cru_df
  df <- left_join(df, cru_df, by=c("lat","long"))
  
  # get mat of important years
  for (row in 1:nrow(df)) {
    obs_year <- df$observation_year[row]
    init_year <- df$first_observation_year[row]
    prev_year <- df$observation_year[row] - df$time_interval[row]
    if (obs_year>=1901 & obs_year<=2022) { # mat of observed year
      mat_obs_row <- df[row,paste0("crumat_",obs_year)]
      mat_obs_col_row <- which(colnames(df)==paste0("crumat_",obs_year)) # column index of observed year mat
      map_obs_row <- df[row,paste0("crumap_",obs_year)]
      map_obs_col_row <- which(colnames(df)==paste0("crumap_",obs_year))
    } else {mat_obs_row <- NA; mat_obs_col_row<-NA;map_obs_row <- NA; map_obs_col_row<-NA}
    if (obs_year-5>=1901 & obs_year<=2022) { # mat of observed year-5
      mat_obs_lag5_row <- df[row,paste0("crumat_",obs_year-5)] 
      map_obs_lag5_row <- df[row,paste0("crumap_",obs_year-5)] 
    } else {mat_obs_lag5_row <- NA; map_obs_lag5_row <- NA}
    if (init_year>=1901 & init_year<=2022) { # mat of first observation_year
      mat_init_row <- df[row,paste0("crumat_",init_year)]
      mat_init_col_row <- which(colnames(df)==paste0("crumat_",init_year)) # column index of initial year mat
      map_init_row <- df[row,paste0("crumap_",init_year)]
      map_init_col_row <- which(colnames(df)==paste0("crumap_",init_year))
    } else {mat_init_row <- NA;mat_init_col_row<-NA;map_init_row <- NA;map_init_col_row<-NA}
    if (init_year-5>=2000 & init_year-5<=2022) { # mat of first observation_year-5
      mat_init_lag5_row <- df[row,paste0("crumat_",init_year-5)]
      map_init_lag5_row <- df[row,paste0("crumap_",init_year-5)]
    } else {mat_init_lag5_row <- NA;map_init_lag5_row <- NA}
    
    if (init_year>=1901 & (obs_year>=1901&obs_year<=2022) ) { # mean since inital year
      mean_mat_row <- mean( as.numeric(df[row,mat_init_col_row:mat_obs_col_row]), na.rm=T)
      mean_map_row <- mean( as.numeric(df[row,map_init_col_row:map_obs_col_row]), na.rm=T)
      sd_map_row <- sd(df[row,map_init_col_row:map_obs_col_row], na.rm=T)
    }
    
    # # type previous year column index
    # if (prev_year>=1901 & prev_year<=2022) {
    #   mat_prev_col_row <- which(colnames(df)==paste0("crumat_",prev_year))
    # } else {mat_prev_col_row<-NA}
    
    # # calculate linear slope between time interval
    # if (!is.na(mat_prev_col_row) & prev_year!=obs_year) {
    #   lm_df <- data.frame(
    #     y = unlist(df[row,mat_prev_col_row:mat_obs_col_row]),
    #     year = prev_year:obs_year)
    #   if (sum(is.na(lm_df$y))==0) {
    #     model <- lm(y~year, data=lm_df, na.action=na.omit)
    #     slope_mat_row <- model$coefficients[2]
    #   } else {slope_mat_row<-NA}
    # } else {slope_mat_row<-NA}
    
    # # calculate lagged linear slope between time interval
    # if ((prev_year-5>=1901 & obs_year-5>=1901) & prev_year!=obs_year) {
    #   lm_df_lag5 <- data.frame(
    #     y = unlist(df[row,mat_prev_col_row:mat_obs_col_row-5]),
    #     year = prev_year:obs_year-5)
    #   if(sum(is.na(lm_df_lag5$y))==0) {
    #     model_lag5 <- lm(y~year, data=lm_df_lag5, na.action=na.omit)
    #     slope_mat_lag5_row <- model_lag5$coefficients[2]
    #   } else {slope_mat_lag5_row<-NA}
    # } else {slope_mat_lag5_row<-NA}
    
    
    # put in df
    df$mean_mat[row] <- mean_mat_row
    df$mean_map[row] <- mean_map_row 
    # df$sd_map[row] <- sd_map_row
    # df$map_obs[row] <- unlist(as.double(map_obs_row))
    # df$mat_obs[row] <- unlist(as.double(mat_obs_row))
    # df$mat_obs_lag5[row] <- unlist(as.double(mat_obs_lag5_row))
    # df$mat_init[row] <- unlist(as.double(mat_init_row))
    # df$mat_init_lag5[row] <- unlist(as.double(mat_init_lag5_row))
    # df$slope_mat[row] <- unlist(as.double(slope_mat_row))
    # df$slope_mat_lag5[row] <- unlist(as.double(slope_mat_lag5_row))
  }
  
  # unlist new columns
  df <- df %>%
    mutate(mean_mat = as.numeric(unlist(mean_mat)),
           mean_map = as.numeric(unlist(mean_map)) #,
           # sd_map = as.numeric(unlist(sd_map)),
           # map_obs = as.numeric(unlist(map_obs)),
           # mat_obs = as.numeric(unlist(mat_obs)),
           # mat_obs_lag5 = as.numeric(unlist(mat_obs_lag5)),
           # mat_init = as.numeric(unlist(mat_init)),
           # mat_init_lag5 = as.numeric(unlist(mat_init_lag5)),
           # slope_mat = as.numeric(unlist(slope_mat)),
           # slope_mat_lag5 = as.numeric(unlist(slope_mat_lag5))
    )
  
  # calculate changes and rate in mat
  df <- df %>% dplyr::group_by(plotID) %>%
    dplyr::arrange(plotID,observation_year) %>%
    # dplyr::mutate(
    #   delta_mat = mat_obs-mat_init, # mat change since first observation
    #   delta_mat_lag5 = mat_obs_lag5-mat_init_lag5,
    #   diff_mat = mat_obs - lag(mat_obs, default = first(mat_obs)),
    #   diff_mat_lag5 = mat_obs_lag5 - lag(mat_obs_lag5, default = first(mat_obs_lag5))
    # ) %>% 
    # dplyr::mutate(
    #   rate_mat = ifelse(is.finite(diff_mat/time_interval), diff_mat/time_interval, NA),
    #   rate_mat_lag5 = ifelse(is.finite(diff_mat_lag5/time_interval),diff_mat_lag5/time_interval,NA)
    # ) %>% 
    dplyr::mutate(
      mean_mat_site = last(mean_mat),
      mean_map_site = last(mean_map),
    ) %>%
    dplyr::arrange(plotID, observation_year)
  
  # take out mat and map every year
  df <- df %>% dplyr::select(-starts_with("cru"))
  
  return(df)
}

# add estimated restoration year for forest (forest age) from Besnard et al. 2021 to gapfill dataset
extract_Besnard_forest_age <- function(df) {
  library(ncdf4)
  # extract estimated forest age circa 2010 from Besnard et al. (2021) 
  # https://essd.copernicus.org/articles/13/4881/2021/
  
  # load world_forest_age_df (created in script 00_download_prepare_spatial_data.R)
  load("./data/world_forest_age_df.rda")
  
  # set cru_df points
  old_points <- distinct(world_forest_age_df[c("long","lat")])
  
  # add new lat long to world_forest_age_df if detected
  # set new points
  points <- df %>% ungroup() %>% filter(land_cover=="forest") %>% 
    distinct(lat, long) %>% mutate(
      lat = as.double(lat),
      long = as.double(long)) %>% dplyr::select(long,lat)
  
  new_points <- setdiff(points, old_points) # in points but not in old_points
  
  # if there are new points 
  if (nrow(new_points)>0) {
    # prepare new points
    new_world_forest_age_df <- new_points
    coordinates(new_points) <- c("long","lat")
    # load product
    filename <- "./data/202432118501222_BGIForestAgeMPIBGC1.0.0.nc"
    world_forest_age <- stack(filename, varname="ForestAge_TC010")
    # use 10% tree cover correction (not consider pixels with <10% tree cover)
    crs(world_forest_age) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    # extract forest age for new points:
    new_world_forest_age_df$besnard_forest_age <- raster::extract(world_forest_age,new_points) 
    new_world_forest_age_df <- new_world_forest_age_df%>% mutate(
      besnard_forest_age=as.double(besnard_forest_age))
    
    # rbind old and new world_forest_age_df
    world_forest_age_df <- rbind(world_forest_age_df, new_world_forest_age_df)
    
    # save updated world_forest_age_df
    save(world_forest_age_df, file="./data/world_forest_age_df.rda")
    
  }
  
  # merge world_forest_age by lat long
  df <- merge(x=df,y=world_forest_age_df,by=c("lat","long"),all.x=TRUE)
  
  # take out nonforest forest age values
  df <- df %>% mutate(
    besnard_forest_age = ifelse(land_cover=="forest",as.double(besnard_forest_age),NA)
  )
  
  # convert forest age into "restoration_year" the age is circa 2010
  df <- df %>% 
    mutate(besnard_restoration_year_row = 2010-besnard_forest_age)
  
  # create unique restoration_year per plot (take latest restoration year)
  df <- df %>% group_by(plotID) %>%
    dplyr::mutate(
      besnard_restoration_year = ifelse(sum(!is.na(besnard_restoration_year_row))>0,
                                        max(besnard_restoration_year_row, na.rm=T), NA)
    ) %>% ungroup() %>%
    dplyr::mutate(besnard_restoration_year=round(besnard_restoration_year))
  
  # remove redundant columns
  df <- df %>% dplyr::select(-besnard_forest_age, -besnard_restoration_year_row)
  
  return(df)
}

#------ 1. convert all SOC units to SOC stock and adjust for depth (0-30cm) ----

## clean columns
sapply(df, typeof)

cols_num <- c("year", "lat", "long", "observation_year", "observation_month", "c_value", "c_SD",
              "c_SE", "sample_number", "upper_depth", "lower_depth", "restoration_year", "BD_eff", "BD",
              "BD_SD", "BD_SE", "clay", "silt", "sand", "pH", "n_value", "n_SE", "c_n", "NPP",
              "elevation", "MAT", "MAP")

df[cols_num] <- sapply(df[cols_num], as.double)
sapply(df[cols_num], typeof)

# format unit
unique(df$c_unit) # "mg/g"  "t/ha"  "%"     "g/kg"  "Mg/ha" "kg/m2" "g/m2" "g/cm2"
unique(df$BD_unit) # NA      "g/cm3" x "g/cm^3" ""
unique(df$n_unit) # "mg/g" "g/kg" "%"    "t/ha"  "Mg/ha" "g/m2"

df[df$n_unit %in% c("g/kg", "mg/g"),]$n_unit <- "g/kg"
df[df$c_unit %in% c("g/kg", "mg/g"),]$c_unit <- "g/kg"
df[df$c_unit %in% c("t/ha"),]$c_unit <- "Mg/ha"
df[df$BD_unit %in% c("g/cm^3"),]$BD_unit <- "g/cm3"

## gapfill bulk density and adjust for coarse fragment
sum(is.na(df$BD)) # 8135 missing BD data out of 18366 observations

# (1) clean erroneous BD and extreme BD change
# (2) gapfill BD and BD_eff of profiles within plot, across sampling time
tic()
df <- clean_gapfill_BD(df) # add column BD_from_data and BD_eff_from_data
toc() # 69 sec

# extract BD from soilgrids
tic()
df <- extract_bulk_density(df) # add column bd_fine_sg
toc()
# check BD failed to be extracted
sum(is.na(df$bd_fine_sg)) # 585 missing BD

# gapfill BD_gf with first BD_from_data and then bd_fine_sg
df <- df %>% mutate(
  BD_gf = case_when( # gapfill BD values
    !is.na(BD) ~ BD,
    !is.na(BD_from_data) ~ BD_from_data,
    !is.na(bd_fine_sg) ~ bd_fine_sg
  )
) %>%
  mutate( # fill BD_def for rows gapfilled with bd_fine_sg, the rest of BD_def predefined by source
    BD_def = ifelse(BD_gf==bd_fine_sg, "fine earth", BD_def) )

sum(is.na(df$BD_gf)) # 245

# add coarse fragment
df <- extract_coarse_fragment_vol(df) # add column cf_vol_sg
sum(is.na(df$cf_vol_sg)) # 524 missing

# calculate BD_eff_est from BD_fine, BD_soil and cf_vol_sg (gapfill priority 1)
rho_coarse <- 2.6 # assume coarse fragment density
df <- df %>% mutate(
  BD_eff_est = case_when(
    BD_def=="fine earth" ~ BD_gf*(1-cf_vol_sg),
    BD_def=="whole soil" ~ BD_gf - cf_vol_sg*rho_coarse,
    TRUE~NA
  )
) %>% mutate( # filter negative BD_eff_est where BD from data is too low for sg
  BD_eff_est = ifelse(BD_eff_est>0, BD_eff_est,NA)
)

# gapfill BD_eff first with BD_eff_est then with BD_eff_sg
df <- df %>% mutate(
  BD_eff_gf = case_when(
    !is.na(BD_eff) ~ BD_eff,
    !is.na(BD_eff_from_data) ~ BD_eff_from_data,
    !is.na(BD_eff_est) ~ BD_eff_est,
    #!is.na(BD_eff_sg) ~ BD_eff_sg,
    TRUE ~ NA
  )
)

sum(is.na(df$BD_eff_gf)) # 570
# above: gapfill bulk density finished

## format and convert c_unit
# ONLY DO THIS ONCE!!!!!! (don't run it more than once in R studio)
# BD unit
unique(df$BD_unit) # so far only "g/cm3" NA "g/cm^3"

# SOC unit
unique(df$c_unit) # "g/kg"  "Mg/ha" "%" "kg/m2" "g/m2" "g/cm2"

# make spelling consistent
df[df$c_unit %in% c("Mg ha-1", "Mg ha–1", "Mg/ha", "t/ha", "tons/ha", "t ha-1"),]$c_unit <- "Mg/ha"
df[df$c_unit %in% c("g/kg", "mg/g"),]$c_unit <- "g/kg"


# convert by calculation
# % = 10*g/kg; mg/kg = 0.001*g/kg
df[df$c_unit=="%", "c_value"] <- 10*df[df$c_unit=="%", "c_value"]
df[df$c_unit=="%", "c_unit"] <- "g/kg"
# C stock to Mg/ha: kg/m2 = 10*Mg/ha; g/m2 = 0.01*Mg/ha; kg/ha = 0.001*Mg/ha; g/cm2 = 100*Mg/ha
df[df$c_unit=="kg/m2", "c_value"] <- 10*df[df$c_unit=="kg/m2", "c_value"]
df[df$c_unit=="kg/m2", "c_unit"] <- "Mg/ha"

df[df$c_unit=="g/m2", "c_value"] <- 0.01*df[df$c_unit=="g/m2", "c_value"]
df[df$c_unit=="g/m2", "c_unit"] <- "Mg/ha"

df[df$c_unit=="g/cm2", "c_value"] <- 100*df[df$c_unit=="g/cm2", "c_value"]
df[df$c_unit=="g/cm2", "c_unit"] <- "Mg/ha"

# check all concentrations and stock units are consistent
unique(df[df$c_var %in% c("SOC", "SOM"), "c_unit"]) == "g/kg" # TRUE
unique(df[df$c_var %in% c("SOC stock", "SOM stock"), "c_unit"]) == "Mg/ha" # TRUE

## convert all SOC value to SOC stock
# Set SOM to SOC conversion factor
coeff <- 0.5 # or 0.58

# create column of layer thickness (D)
df <- mutate(df, 
             D = lower_depth - upper_depth, 
             depth_avg = (lower_depth + upper_depth)/2)

# calculate from concentration
unique(df$c_var)
df <- df %>%
  dplyr::mutate(SOC_stock = case_when(
    c_var %in% c("SOC stock", "TC stock", "TC stock ") ~ c_value, # paste SOC stock and TC stock to the new column
    c_var == "SOM stock" ~ coeff * c_value, # from SOM stock: SOM stock(Mg/ha) = coeff*SOC stock(Mg/ha)
    c_var %in% c("SOC", "TC") ~ c_value * BD_eff_gf * D / 10, # from SOC concentration: SOC stock(Mg/ha) = SOC(g/kg)*BD(g/cm3)*D(cm)/10
    c_var == "SOM" ~ coeff * c_value * BD_eff_gf * D / 10, # from SOM concentration: SOC stock(Mg/ha) = coeff*SOM(g/kg)*BD(g/cm3)*D(cm)/10
    TRUE ~ NA # if non of previous conditions are met, set SOC_stock=NA
  ))

# check missing SOC stock 
sum(is.na(df$SOC_stock)) # 179

# exclude missing SOC_stock from df
df <- df %>% filter(!is.na(SOC_stock))
min(df$SOC_stock) # 0.0097902

# # save this processed df without adjusting for depth (intermediate step)
# # (no adjusting depth for missing or overlapping layer, no calculating profile depth)
# # no fitting depth curve
# df_layer <- df
# save(df_layer, file="./data/df_layer_2024Jul09.rda") # 18179 rows
# write.csv(df_layer, "./data/df_layer_2024Jul09.csv", row.names = FALSE)

## adjust C stock to standardized depth
# set default standard depths (implement determine std depth in the loop?)
log_std_depth <- data.frame( log_lower_depth = c(log(30), log(60)))

# set fitted slope coefficient for soil profile (see Fitting_slope.R)
F_slope <- 0.4057259 # forest
S_slope <- 0.6069514 # shrubland
G_slope <- 0.4476003 # grassland
C_slope <- 0.4899359 # cropland
P_slope <- G_slope # pasture, use grassland for now
avg_slope <- 0.4872456 # other, use global average for now

# start two new dataset
df_processed <- data.frame() # dataset with original data rows
df_adjusted <- data.frame() # dataset with each profile per row
cols_adjusted <- c("source", "author", "year", "title", "url", "collector", "lat", "long",
                   "site_name", "siteID", "plotID", "profileID", "observation_year", "observation_month",
                   "land_cover", "restoration", "restoration_year", "successional_stage", "elevation", "MAT", "MAP", "SOC_stock_30",
                   "SOC_stock_60", "r_squared")


for (i in 1:nrow(distinct(df, plotID, profileID)) ) { # loop through each soil profile
  temp_val <- distinct(df, plotID, profileID)[i,]
  temp_df <- subset(df, plotID==temp_val$plotID & 
                      profileID==temp_val$profileID)
  
  # skip null profile
  if (nrow(temp_df) == 0) {next}
  # skip profile with only NA
  if ( sum(is.na(temp_df[, "SOC_stock"])) >= 1 ) {
    print(paste("missing measurement in some depth intervals: i", i,
                "profileID", temp_val$plotID, "plotID", temp_val$profileID))
    next
  }
  # skip profile where lower_depth < upper_depth
  if( length(which(temp_df$lower_depth <=temp_df$upper_depth))>0 ) {
    print(paste("lower_depth smaller than upper_depth","i",i,"profileID",temp_val$profileID))
    next
  }
  
  # sort each soil profile in increasing depth
  temp_df <- temp_df[order(temp_df$lower_depth),]
  
  # create vector of upper_depths of 2-last row, lower_depth of 1~(last-1) row 
  lower1 <- temp_df[1:(nrow(temp_df)-1), ]$lower_depth
  upper2 <- temp_df[2:nrow(temp_df), ]$upper_depth
  
  # adjust overlapping or missing layers
  if ( nrow(temp_df)>1 & !identical(lower1, upper2) ) {
    # identify indices of non-continuity in lower1 and upper2
    j_overlap <- which(upper2 < lower1)
    j_missing <- which(upper2 > lower1)
    # if two layers overlap
    if ( length(j_overlap)>0 ) {
      print(paste("overlapping", "i ",i,"profileID", temp_val$profileID))
      for (j in j_overlap) {
        # change upper_depth (lower layer) to be lower_depth (upper layer)
        temp_df$upper_depth[j+1] <- temp_df$lower_depth[j]
        new_D <- temp_df$lower_depth[j+1]-temp_df$upper_depth[j+1]
        # adjust stock number by new_D/old D
        temp_df$SOC_stock[j+1] <- temp_df$SOC_stock[j+1]*(new_D/temp_df$D[j+1])
        # adjust new D
        temp_df$D[j+1] <- new_D
      }
    }
    
    # if two layers have missing depth in between, calculate intermediate SOC_stock as average
    if ( length(j_missing)>0 ) {
      for (j in j_missing) {
        print(paste("missing layer", "i ",i,"profileID", temp_val$profileID))
        # calculate stock/cm depth for layer above and below missing layer
        stock_above <- temp_df$SOC_stock[j]/temp_df$D[j]
        stock_below <- temp_df$SOC_stock[j+1]/temp_df$D[j+1]
        # add new row for gapfilled missing layer
        new_row <- temp_df[j,] %>% # duplicate the row for layer above missing layer and replace depth columns
          dplyr::mutate(upper_depth = temp_df$lower_depth[j],
                        lower_depth = temp_df$upper_depth[j+1],
                        D = temp_df$upper_depth[j+1] - temp_df$lower_depth[j],
                        c_value = NA,
                        comment = paste0("SOC_stock of this layer is gapfilled. ",comment)) %>%
          dplyr::mutate(SOC_stock = 0.5*(stock_above+stock_below)*D)
        # add new row to existing temp_df (profile_level)
        temp_df <- temp_df %>% add_row(new_row)
      }
      # reorder temp_df by increasing upper_depth
      temp_df <- temp_df[order(temp_df$lower_depth),]
    }
  } # end adjusting non-continuous layers (overlapping and missing)
  
  # calculate cumulative stock
  temp_df <- mutate(temp_df, 
                    cum_SOC_stock = cumsum(SOC_stock) )
  
  # for profile with single layer, fit log-log curve with biome-specific slope
  if (nrow(temp_df) == 1) {
    
    # set biome-sppecific slope
    if (temp_df$land_cover[1]=="forest") { slope <- F_slope
    } else if (temp_df$land_cover[1]=="grassland") { slope <- G_slope
    } else if (temp_df$land_cover[1]=="shrubland") { slope <- S_slope 
    } else { slope <- avg_slope }
    
    # calculate intercept directly bc there's only one sample
    intercept <- log(temp_df$cum_SOC_stock) - log(temp_df$lower_depth)*slope
    
    # predict stock at standardized depths
    temp_df <-  mutate(temp_df, 
                       SOC_stock_30 = exp(log_std_depth[1,]*slope + intercept),
                       SOC_stock_60 = exp(log_std_depth[2,]*slope + intercept),
                       r_squared = NA )
    
  } else { # for continuous profile with multiple layers
    
    # fit log-log curve
    temp_df <-  mutate(temp_df, 
                       log_lower_depth = log(lower_depth) )
    
    curve <- lm(log(cum_SOC_stock) ~ log_lower_depth, data=temp_df)
    
    # # get curve summary and plot log(depth) vs. log(SOC)
    # summary(curve)
    # plot(log(temp_df$lower_depth), log(temp_df$cum_SOC_stock))
    # abline(curve, col="red")
    
    
    # predict stock at standardized septh
    temp_df <-  mutate(temp_df, 
                       SOC_stock_30 = exp(predict(curve, log_std_depth)[1]),
                       SOC_stock_60 = exp(predict(curve, log_std_depth)[2]),
                       r_squared = summary(curve)$adj.r.squared )
    temp_df <- subset(temp_df, select = -c(log_lower_depth))
  }
  
  # # plot SOC vs. detph and fitted line
  # ggplot(data=temp_df) + ylim(0,60) + expand_limits(y = c(0, 60), x=c(0,150)) +
  #   geom_point(aes(x=cum_SOC_stock, y=lower_depth)) +
  #   geom_function(fun = ~ exp( (log(.x) - summary(curve)$coefficient[1])/summary(curve)$coefficient[2] ) ) +
  #   scale_y_reverse() + scale_x_continuous(position = "top") +
  #   xlab("Cumulative SOC stock (Mg/ha)") + ylab("Lower depth (cm)") #+ theme_bw()
  
  # rbind temp_df with df_processed
  df_processed <- rbind(df_processed, temp_df)
  
  # rbind adjusted stock in each profile as a row with df_adjusted
  df_adjusted <- rbind(df_adjusted, temp_df[1, cols_adjusted])
}

# exclude time series that contain poor fit (R^2<0.8)
profiles_small_r2 <- df_adjusted %>% # list of profileIDs that don't match r2=0.8 threshold
  filter(r_squared<0.8) %>%
  dplyr::select(profileID)

df_processed <- df_processed %>%
  filter(!(profileID %in% profiles_small_r2$profileID)) %>%
  group_by(plotID) %>% dplyr::arrange(plotID,observation_year) %>%
  filter(length(unique(observation_year))>1) %>% ungroup()

df_adjusted <- df_adjusted %>%
  filter(!(profileID %in% profiles_small_r2$profileID)) %>%
  group_by(plotID) %>% dplyr::arrange(plotID,observation_year) %>%
  filter(length(unique(observation_year))>1) %>% ungroup()

# # save intermediate steps
# save(df_processed, file="./data/df_processed_2024Jul09.rda") # 695 entries
# save(df_adjusted, file="./data/df_adjusted_2024Jul09.rda") # 814 entries
# write.csv(df_processed, "./data/df_processed_2024Jul09.csv", row.names = FALSE)
# write.csv(df_adjusted, "./data/df_adjusted_2024Jul09.csv", row.names = FALSE)

#------ 2. add and format covariates -------------------------------------------
# this steps takes df_processed from above and process data further
cols_num <- c("lat", "long", "observation_year", "observation_month",
              "restoration_year", "elevation", "MAT", "MAP", "SOC_stock_30", 
              "SOC_stock_60", "r_squared", "clay", "silt", "sand", "c_n", "n_value")

df_processed[cols_num] <- sapply(df_processed[cols_num], as.double)
sapply(df_processed, typeof)

## convert unit and average existing covariates to plot level
df_adjusted_covariates <- df_processed %>%
  mutate(n_value = ifelse(n_unit=="g/kg", n_value/10, n_value),
         n_unit = ifelse(n_unit=="g/kg", "%", n_unit)
  ) %>% # convert N concentration unit
  group_by(profileID) %>% # generate deepest depth and true SOC stock columns
  dplyr::mutate(
    pro_depth = max(lower_depth), # deepest depth measured
    pro_C_stock = max(cum_SOC_stock) # stock to the depth measured
  ) %>%
  group_by(plotID) %>% dplyr::arrange(plotID,observation_year) %>%
  dplyr::mutate(
    plo_clay = mean(clay, na.rm=TRUE),
    plo_silt = mean(silt, na.rm=TRUE),
    plo_sand = mean(sand, na.rm=TRUE),
    plo_ph = mean(pH, na.rm=TRUE),
    plo_c_n = mean(c_n, na.rm=TRUE),
    plo_n_pct = ifelse("%" %in% unique(n_unit) & ("%" %in% unique(n_unit) | NA %in% unique(n_unit)), weighted.mean(n_value, D), NA),
    plo_elevation = mean(elevation, na.rm=TRUE)
  ) %>% 
  distinct(source, lat, long, site_name, siteID, 
           plotID, profileID, observation_year, observation_month, land_cover, restoration, 
           restoration_year, elevation, MAT, MAP, SOC_stock_30, SOC_stock_60, r_squared, 
           pro_depth, pro_C_stock, plo_clay, plo_silt, plo_sand, plo_ph, plo_n_pct, 
           plo_c_n, plo_elevation) %>%
  group_by(profileID) %>%
  filter(row_number()==1) %>% ungroup()

# check that only one line per unique profile
test <- df_adjusted_covariates %>% group_by(plotID) %>%
  filter(length(unique(observation_year))!=n())

# add first_observation_year and time_interval (years between obs. year and first obs. year)
df_adjusted_covariates <- df_adjusted_covariates %>%
  group_by(plotID) %>% dplyr::arrange(plotID,observation_year) %>%
  dplyr::mutate(
    first_observation_year = observation_year[1],
    time_interval = observation_year - lag(observation_year, default = first(observation_year))
  ) %>% ungroup()

## add covariate values to from external products (to use in gapfilling)
# add clay% from soilgrids (df$clay_sg)
df_adjusted_covariates <- extract_clay(df_adjusted_covariates)

# add ph from soilgrids (df$ph_sg)
df_adjusted_covariates <- extract_ph(df_adjusted_covariates)

# add n% from soilgrids (df$n_pct_sg)
df_adjusted_covariates <- extract_n(df_adjusted_covariates)

# Add MAT MAP of the year of observation (df$MAT_cru, df$MAP_cru, df$mean_mat_site, df$mean_map_site)
tic()
df_adjusted_covariates <- extract_CRU(df_adjusted_covariates)
toc()

# get estimated forest restoration year based on forest age circa 2010 (df$besnard_restoration_year)
df_adjusted_covariates <- extract_Besnard_forest_age(df_adjusted_covariates)

## gapfill covariates
df_lm_covariates <- df_adjusted_covariates %>%
  group_by(plotID) %>% dplyr::arrange(plotID,observation_year) %>%
  dplyr::mutate(
    MAT_gf = ifelse(is.na(MAT), MAT_cru, MAT), # gap fill MAT MAP with CRU 
    MAP_gf = ifelse(is.na(MAP), MAP_cru, MAP),
    clay_gf = ifelse(is.na(plo_clay), clay_sg, plo_clay),
    ph_gf = ifelse(is.na(plo_ph), ph_sg, plo_ph),
    n_pct_gf = ifelse(is.na(plo_n_pct), n_pct_sg, plo_n_pct),
    initial_stock30 = SOC_stock_30[1],
    initial_stock60 = SOC_stock_60[1],
    abs_diff_30 = SOC_stock_30 - lag(SOC_stock_30, default = first(SOC_stock_30))#, # change from last observation
    ) %>% dplyr::mutate(
    abs30_rate_mgha_yr = abs_diff_30/time_interval
  ) %>% dplyr::ungroup()

# change land_cover names
df_lm_covariates$land_cover <- ifelse(grepl("Bog|peatbogs", df_lm_covariates$land_cover), "bog", df_lm_covariates$land_cover)

# add factor to categorical columns
df_lm_covariates$siteID <- factor(df_lm_covariates$siteID)
df_lm_covariates$plotID <- factor(df_lm_covariates$plotID)
df_lm_covariates$profileID <- factor(df_lm_covariates$profileID)
df_lm_covariates$land_cover <- factor(df_lm_covariates$land_cover)
df_lm_covariates$restoration <- factor(df_lm_covariates$restoration)

# rename SOC_stock_30 and SOC_stock_60
df_lm_covariates <- dplyr::rename(df_lm_covariates,
                                  SOC_stock30 = SOC_stock_30#,
                                  #SOC_stock60 = SOC_stock_60
                                  )

sapply(df_lm_covariates, function(x) sum(is.na(x)))

# # save intermediate step
# save(df_lm_covariates, file="df_lm_covariates_2024Jul09.rda")
# write.csv(df_lm_covariates, "/Users/jiaruofei/Dropbox (MIT)/Global_SOC/df_lm_covariates_2024Jul09.csv", row.names = FALSE)

# filter to keep only columns needed for next steps
# create df with only columns are modeling/analysis #!!!!!!add EU forest age column
df_gf <- df_lm_covariates[c("source",
                            "lat", "long", "siteID", "plotID", "profileID", "observation_year",
                            "land_cover", "restoration", "restoration_year", "besnard_restoration_year", 
                            "MAT_cru", "MAP_cru", "clay_gf", "ph_gf", "n_pct_gf",
                           "initial_stock30", "mean_mat_site", "mean_map_site",
                            "SOC_stock30", "pro_depth", "pro_C_stock", 
                            "abs30_rate_mgha_yr"
)]
# # save intermediate steps
# save(df_gf, file="./data/df_gf_2024Jul09.rda")
# write.csv(df_gf, "./data/df_gf_2024Jul09.csv", row.names = FALSE)

# merge region, MAT, MAP, MAT to df_layer
df_layer_gf <- merge(x=df_layer, 
                     y=df_gf[,c("profileID","MAT_cru","MAP_cru","mean_mat_site","mean_map_site",
                                "besnard_restoration_year")], 
                     by="profileID",all.x=T)
# # save intermediate step
# save(df_layer_gf, file="./data/df_layer_gf_2024Jul09.rda")
# write.csv(df_layer_gf, "./data/df_layer_gf_2024Jul09.csv", row.names = FALSE)

#------ 3. clean dataset (df_gf) and get ready for analysis  ---------------------------
## scope dataset
# exclude the very ancient grassland obsin 1876
df_gf <- df_gf %>% filter(observation_year>1900)

# exclude water-saturated plots and shrubland plots
df_gf <- df_gf %>% filter(!grepl("water-logged|marsh|bog|shrubland", land_cover)) %>%
  mutate(land_cover=factor(land_cover))

unique(df_gf$land_cover)
unique(df_gf$biome3)

# exclude plots that restoration_year in between first and last observation
# make restoration year NA if restoration_year >=last observation year
# (fist implemented on 2024Mar28)
df_gf <- df_gf %>% group_by(plotID) %>% arrange(observation_year) %>%
  dplyr::filter(
    is.na(restoration_year) | unique(restoration_year)<=first(observation_year) | unique(restoration_year)>=last(observation_year)) %>%
  dplyr::mutate(
    restoration_year = ifelse(
      unique(restoration_year)>=last(observation_year), NA, restoration_year
    )
  )

## clean outliers in SOC temporal change
# filter by the ratio of repeated sampling and first sampling's profile depth SOC stock

# add percentage of profile SOC stock to first observation
df_gf <- df_gf %>% group_by(plotID) %>% arrange(plotID, observation_year) %>%
  dplyr::mutate(first_proSOC= first(pro_C_stock)) %>%
  ungroup() %>% 
  mutate(pct_1stproSOC = pro_C_stock/first_proSOC,
         pct_1stproSOC_log = log(pro_C_stock/first_proSOC) ) %>% # ratio of profile-depth SOC/first time measrurement
  dplyr::arrange(pct_1stproSOC, descending=F)

# visualize
ggplot(data=df_gf) + 
  geom_histogram(aes(x=pct_1stproSOC_log),bins = 100)

# clean beyond +2 and -2 sd (regardless of different pro_depth for now)
sd_pct_1stproSOC_log <- sd(df_gf$pct_1stproSOC_log)
mean_pct_1stproSOC_log <- mean(df_gf$pct_1stproSOC_log)

df_gf_sd <- df_gf %>% ungroup() %>%
  filter(pct_1stproSOC_log<=mean_pct_1stproSOC_log+2*sd_pct_1stproSOC_log & 
           pct_1stproSOC_log>=mean_pct_1stproSOC_log-2*sd_pct_1stproSOC_log ) %>% # delete the observations with extreme change from first obs
  group_by(plotID) %>%
  dplyr::filter(length(unique(observation_year))>1) 

plots_sd <- unique(df_gf_sd$plotID) # 3375 plots


## adjust dataframe for statistical analysis
# calculate log of SOC stock (0-30cm)
df_gf_sd$SOC_stock30_log <- log(df_gf_sd$SOC_stock30)

# adjust year
baseYear=min(df_gf_sd$observation_year)
df_gf_sd$cYear <- df_gf_sd$observation_year - baseYear

# add forest_cond (land cover with forest age 60 yrs cutoff in initial year, without gapfilling with Besnard forest age map)
df_gf_sd <- df_gf_sd %>% 
  group_by(plotID) %>%
  arrange(observation_year) %>%
  dplyr::mutate(
    stand_age_first = first(observation_year) - restoration_year) %>%
  ungroup() %>% 
  dplyr::mutate( # less than and after 50
    forest_cond = case_when( # land cover and divided by condition
      land_cover=="grassland" ~ "grassland",
      land_cover=="forest" & (stand_age_first<60 | grepl("early|mid|late", successional_stage) ) ~ "young forest",
      land_cover=="forest" & (stand_age_first>=60 | grepl("mature|old growth", successional_stage) ) ~ "old forest",
      land_cover=="forest" & (is.na(successional_stage) & is.na(restoration_year) ) ~ "unknown forest"
    ))

 # forest age grouping from literature/source that supersede gapfilled stand age
df_gf_sd <- df_gf_sd %>% 
  dplyr::mutate(
    forest_cond = case_when(
      siteID %in% c("L-AH-ECN", "L-DR-BF", "cnern-DHF", "cnern-ALF", "NEON-GUAN",
                    "NEON-JERC") ~ "old forest",
      siteID=="NEON-OSBS"|siteID=="NEON-SCBI" ~ "young forest",
      TRUE ~ as.character(forest_cond) )
  ) %>%
  mutate(
    forest_cond = factor(forest_cond, 
                         levels=c("young forest","old forest","unknown forest", "grassland"))
  )


# add forest_cond_gf_besnard60 (cutoff 60 years, gapfilled by besnard forest age map)
df_gf_sd <- df_gf_sd %>%
  dplyr::mutate(restoration_year_gf_besnard = case_when(
    !is.na(restoration_year) ~ restoration_year,
    !is.na(besnard_restoration_year) ~ besnard_restoration_year,
    TRUE ~ NA
  )) %>%
  group_by(plotID) %>%
  arrange(observation_year) %>%
  dplyr::mutate(
    stand_age_first_gf_besnard = first_observation_year - restoration_year_gf_besnard ) %>%
  ungroup() %>%
  dplyr::mutate(forest_cond_gf_besnard60 = case_when( # land cover and divided by condition
    land_cover=="grassland" ~ "grassland",
    land_cover=="forest" & (stand_age_first_gf_besnard<60 | grepl("early|mid|late", successional_stage) ) ~ "young forest",
    land_cover=="forest" & (stand_age_first_gf_besnard>=60 | grepl("mature|old growth", successional_stage) ) ~ "old forest",
    land_cover=="forest" & (is.na(successional_stage) & is.na(restoration_year_gf_besnard) ) ~ "unknown forest"
  ))
  # forest age grouping from literature/source that supersede gapfilled stand age
df_gf_sd <- df_gf_sd %>% 
  dplyr::mutate(
    forest_cond_gf_besnard60 = case_when(
      siteID %in% c("L-AH-ECN", "L-DR-BF", "cnern-DHF", "cnern-ALF", "NEON-GUAN",
                    "NEON-JERC") ~ "old forest",
      siteID=="NEON-OSBS"|siteID=="NEON-SCBI" ~ "young forest",
      TRUE ~ as.character(forest_cond_gf_besnard60)
    )
  ) %>%
  mutate(
    forest_cond_gf_besnard60 = factor(forest_cond_gf_besnard60, 
                                      levels=c("young forest","old forest","grassland"))
  )

# check
unique(df_gf_sd$forest_cond)
unique(df_gf_sd$forest_cond_gf_besnard60)

# # save intermediate steps
# save(df_gf_sd, file="./data/df_gf_sd_2024Jul09.rda")
# write.csv(df_gf_sd, "./data/df_gf_sd_2024Jul09.csv", row.names = FALSE)

# filter out "unknown forest" category, add numeric column for forest age group
df_known_age <- df_new %>% 
  filter(forest_cond_gf_besnard60!="unknown forest")

df_known_age %>% group_by(forest_cond_gf_besnard60_num) %>%
  summarize(length(unique(siteID)),length(unique(plotID)))

# # save intermediate step
# save(df_known_age, file="./data/df_known_age_2024Jul09.rda")

# prepare dataset for regression (plot level covariates and standardize)
df_model <- df_known_age %>%
  group_by(plotID) %>%
  dplyr::mutate(
    clay_plo = mean(clay_gf, na.rm=T),
    ph_plo = mean(ph_gf, na.rm=T),
    n_pct_plo = mean(n_pct_gf, na.rm=T)
  ) %>% ungroup() %>%
  filter(
    !is.na(clay_plo) & !is.na(ph_plo) & !is.na(n_pct_plo)  )

# # save intermediate step
# save(df_model, file="./data/df_model_2024Jul09.rda")

# add column of whether forest plot age is gapfilled
df_std <- df_model %>%
  mutate(forest_age_gapfill = case_when(
    grepl("grassland|shrubland", forest_cond_gf_besnard60) ~ NA, # if not forest, NA
    siteID %in% c("L-AH-ECN", "L-DR-BF", "cnern-DHF", "cnern-ALF", "NEON-GUAN",
                  "NEON-JERC","NEON-OSBS","NEON-SCBI") ~ FALSE, # if assigned from srouce info, F
    forest_cond=="young forest" ~ FALSE,
    forest_cond=="old forest" ~ FALSE,
    forest_cond=="unknown forest" ~ TRUE
  )) %>%
  mutate( # standardize
    std_MAT_cru = (MAT_cru - mean(MAT_cru)) / sd(MAT_cru),
    std_MAP_cru = (MAP_cru - mean(MAP_cru)) / sd(MAP_cru),
    std_year = (observation_year - mean(observation_year)) / sd(observation_year),
    std_n_pct_plo = (n_pct_plo - mean(n_pct_plo)) / sd(n_pct_plo),
    std_clay_plo = (clay_plo - mean(clay_plo)) / sd(clay_plo),
    std_ph_plo = (ph_plo - mean(ph_plo)) / sd(ph_plo)
  )

# keep only relevant columns
df_std <- df_std %>%
  dplyr::select(siteID, plotID, profileID, lat, long, observation_year, 
              SOC_stock30, SOC_stock30_log, abs30_rate_mgha_yr,
              MAT_cru,MAP_cru, forest_cond_gf_besnard60, 
              n_pct_plo,clay_plo,ph_plo,
              mean_mat_site,mean_map_site,forest_age_gapfill,
              std_MAT_cru,std_MAP_cru,std_year,
              std_n_pct_plo,std_clay_plo,std_ph_plo
              ) %>%
  ungroup()
  
# save final dataset ready for analysis
save(df_std, file="./data/SOC_time_series_for_analysis.rda")
write.csv(df_std, "./data/SOC_time_series_for_analysis.rda", row.names = FALSE)

