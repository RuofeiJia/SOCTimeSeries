# predict SOC change from gamm model
# modified from post_processing_bayesian_cap.R to add soil pH as predictor, standardize predictor values here
# compared to post_processing_bayesian_cap.R, defined max and min values of MAT and MAP 
# not temper with any predictor. leave the excluding job in find_CI
# SAVE ORIGINAL N, CLAY, PH value in input_dfs for more convenient post analysis

rm(list = ls(all = TRUE))

# package and directory
library(dplyr)
library(raster)
library(terra)
library(tictoc)
library(brms)

# working directory should be set by default to the folder of entire repo (R project)

#=========== load functions ===========================
# function that convert raster to dataframe 
raster2df <- function(layer) {
  layer_spdf <- as(layer, "SpatialPixelsDataFrame")
  layer_df <- as.data.frame(layer_spdf)
  colnames(layer_df) <- c("value", "x", "y")
  return(layer_df)
}

print("functions loaded")

#================== load data and calculate mean and sd ========================
# load data
load("./data/SOC_withNewData_for_analysis.rda")
df_std <- df_std_withNewData %>% filter(!is.na(siteID))

rm(df_std_withNewData)

# calculate mean and sd
mean_year <- mean(df_std$observation_year)
sd_year <- sd(df_std$observation_year)
mean_MAT <- mean(df_std$MAT_cru)
sd_MAT <- sd(df_std$MAT_cru)
mean_MAP <- mean(df_std$MAP_cru)
sd_MAP <- sd(df_std$MAP_cru)

mean_n_pct <- mean(df_std$n_pct_plo)
sd_n_pct <- sd(df_std$n_pct_plo)
mean_clay <- mean(df_std$clay_plo)
sd_clay <- sd(df_std$clay_plo)
mean_ph <- mean(df_std$ph_plo)
sd_ph <- sd(df_std$ph_plo)

#================== load predictor rasters (MAT,MAP,forest_cond,land_cover) ======================
# MAT,MAP,forest_cond,land_cover layers generated from script prepare_layers.R
# clay content, soil n%, and elevation layers generated from resample_clay_n_elev.R

# map directory
map_dir <- "./data/"

# MAT_year
mat_1992 <- raster(paste0(map_dir, "mat_1992.tif"))
mat_2000 <- raster(paste0(map_dir, "mat_2000.tif"))
mat_2010 <- raster(paste0(map_dir, "mat_2010.tif"))
mat_2020 <- raster(paste0(map_dir, "mat_2020.tif"))

# MAP_year
map_1992 <- raster(paste0(map_dir, "map_1992.tif"))
map_2000 <- raster(paste0(map_dir, "map_2000.tif"))
map_2010 <- raster(paste0(map_dir, "map_2010.tif"))
map_2020 <- raster(paste0(map_dir, "map_2020.tif"))

# n_pct
n_pct <- raster(paste0(map_dir, "n_sg_halfdegree.tif"))

# clay
clay <- raster(paste0(map_dir, "clay_sg_halfdegree.tif"))

# soil ph
ph <- raster(paste0(map_dir, "ph_sg_halfdegree.tif"))

print("predictor rasters loaded")

#================= convert input layers into dfs =================
# create empty dataframe with lat long of full half-degree grid
all_coords <- raster2df(mat_1992) %>%
  dplyr::select(x,y) %>%
  arrange(x,y)

# MAT_year
mat_1992_df <- merge(x=all_coords, y=raster2df(mat_1992), by=c("x","y"), all.x=T) %>% arrange(x,y)
mat_2000_df <- merge(x=all_coords, y=raster2df(mat_2000), by=c("x","y"), all.x=T) %>% arrange(x,y)
mat_2010_df <- merge(x=all_coords, y=raster2df(mat_2010), by=c("x","y"), all.x=T) %>% arrange(x,y)
#mat_2018_df <- merge(x=all_coords, y=raster2df(mat_2018), by=c("x","y"), all.x=T) %>% arrange(x,y)
mat_2020_df <- merge(x=all_coords, y=raster2df(mat_2020), by=c("x","y"), all.x=T) %>% arrange(x,y)

# MAP_year (do above onward)
map_1992_df <- merge(x=all_coords, y=raster2df(map_1992), by=c("x","y"), all.x=T) %>% arrange(x,y)
map_2000_df <- merge(x=all_coords, y=raster2df(map_2000), by=c("x","y"), all.x=T) %>% arrange(x,y)
map_2010_df <- merge(x=all_coords, y=raster2df(map_2010), by=c("x","y"), all.x=T) %>% arrange(x,y)
#map_2018_df <- merge(x=all_coords, y=raster2df(map_2018), by=c("x","y"), all.x=T) %>% arrange(x,y)
map_2020_df <- merge(x=all_coords, y=raster2df(map_2020), by=c("x","y"), all.x=T) %>% arrange(x,y)

# n_pct
n_pct_df <- merge(x=all_coords, y=raster2df(n_pct), by=c("x","y"), all.x=T) %>% 
  arrange(x,y)

# clay
clay_df <- merge(x=all_coords, y=raster2df(clay), by=c("x","y"), all.x=T) %>% 
  arrange(x,y)

# ph
ph_df <- merge(x=all_coords, y=raster2df(ph), by=c("x","y"), all.x=T) %>% 
  arrange(x,y)

print("input rasters converted into dfs")
#======== compile input predictors in df with column x,y  =====
# merge all input data with all_coords and na.omit
# young forest
input_youngforest1990_first <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 1992,
  MAT_cru = mat_1992_df$value,
  MAP_cru = map_1992_df$value,
  forest_cond_gf_besnard60 = "young forest",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)

input_youngforest1990_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2000,
  MAT_cru = mat_2000_df$value,
  MAP_cru = map_2000_df$value,
  forest_cond_gf_besnard60 = "young forest",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)

input_youngforest2000_first <- input_youngforest1990_last # 2000
input_youngforest2000_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2010,
  MAT_cru = mat_2010_df$value,
  MAP_cru = map_2010_df$value,
  forest_cond_gf_besnard60 = "young forest",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)


input_youngforest2010_first <- input_youngforest2000_last # 2010
input_youngforest2010_last <-  data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2020,
  MAT_cru = mat_2020_df$value,
  MAP_cru = map_2020_df$value,
  forest_cond_gf_besnard60 = "young forest",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)

# old forest  
input_oldforest1990_first <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 1992,
  MAT_cru = mat_1992_df$value,
  MAP_cru = map_1992_df$value,
  forest_cond_gf_besnard60 = "old forest",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)

input_oldforest1990_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2000,
  MAT_cru = mat_2000_df$value,
  MAP_cru = map_2000_df$value,
  forest_cond_gf_besnard60 = "old forest",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)


input_oldforest2000_first <- input_oldforest1990_last # 2000
input_oldforest2000_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2010,
  MAT_cru = mat_2010_df$value,
  MAP_cru = map_2010_df$value,
  forest_cond_gf_besnard60 = "old forest",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)


input_oldforest2010_first <- input_oldforest2000_last # 2010
input_oldforest2010_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2020,
  MAT_cru = mat_2020_df$value,
  MAP_cru = map_2020_df$value,
  forest_cond_gf_besnard60 = "old forest",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)

# grassland
input_grassland1990_first <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 1992,
  MAT_cru = mat_1992_df$value,
  MAP_cru = map_1992_df$value,
  forest_cond_gf_besnard60 = "grassland",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)

input_grassland1990_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2000,
  MAT_cru = mat_2000_df$value,
  MAP_cru = map_2000_df$value,
  forest_cond_gf_besnard60 = "grassland",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)

input_grassland2000_first <- input_grassland1990_last # 2000
input_grassland2000_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2010,
  MAT_cru = mat_2010_df$value,
  MAP_cru = map_2010_df$value,
  forest_cond_gf_besnard60 = "grassland",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)

input_grassland2010_first <- input_grassland2000_last # 2010
input_grassland2010_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2020,
  MAT_cru = mat_2020_df$value,
  MAP_cru = map_2020_df$value,
  forest_cond_gf_besnard60 = "grassland",
  n_pct_plo = n_pct_df$value,
  clay_plo = clay_df$value,
  ph_plo = ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP,
    std_n_pct_plo = (n_pct_plo - mean_n_pct) / sd_n_pct,
    std_clay_plo = (clay_plo - mean_clay) / sd_clay,
    std_ph_plo = (ph_plo - mean_ph) / sd_ph)


# establish x,y vectors fro nonNA pixels
xvec_omit <- input_youngforest1990_first$x
yvec_omit <- input_youngforest1990_first$y

omit_coords <- data.frame(x=xvec_omit, y=yvec_omit)

# check if all input dfs have x,y order of the vectors (should be all TRUE)
identical(input_youngforest1990_last$x, xvec_omit); identical(input_youngforest1990_last$y, yvec_omit)
identical(input_youngforest2000_last$x, xvec_omit); identical(input_youngforest2000_last$y, yvec_omit)
identical(input_youngforest2010_last$x, xvec_omit); identical(input_youngforest2010_last$y, yvec_omit)
identical(input_oldforest1990_first$x, xvec_omit); identical(input_oldforest1990_first$y, yvec_omit)
identical(input_oldforest2000_first$x, xvec_omit); identical(input_oldforest2000_first$y, yvec_omit)
identical(input_oldforest2010_first$x, xvec_omit); identical(input_oldforest2010_first$y, yvec_omit)
identical(input_oldforest2010_last$x, xvec_omit); identical(input_oldforest2010_last$y, yvec_omit)
identical(input_grassland1990_first$x, xvec_omit); identical(input_grassland1990_first$y, yvec_omit)
identical(input_grassland2000_first$x, xvec_omit); identical(input_grassland2000_first$y, yvec_omit)
identical(input_grassland2010_first$x, xvec_omit); identical(input_grassland2010_first$y, yvec_omit)
identical(input_grassland2010_last$x, xvec_omit); identical(input_grassland2010_last$y, yvec_omit)

print("input dfs built")

# save inputs
tic()
save(
  # young forest
  input_youngforest1990_first,input_youngforest1990_last,input_youngforest2000_first,
  input_youngforest2000_last,input_youngforest2010_first,input_youngforest2010_last,
  # old forest
  input_oldforest1990_first,input_oldforest1990_last,input_oldforest2000_first,
  input_oldforest2000_last,input_oldforest2010_first,input_oldforest2010_last,
  # grassland
  input_grassland1990_first,input_grassland1990_last,input_grassland2000_first,
  input_grassland2000_last,input_grassland2010_first,input_grassland2010_last,
  file = "./results/post_processing_bayesian_cap2_new_inputs_brms1_37k12chain.rda")
toc()
print("inputs data saved to post_processing_bayesian_cap2_new_inputs_brms1_37k12chain.rda")
#======================= load model ===========================
load("./results/brms1_withOneTime_37k12chain.rda")

# assign name to model
brms_mod <- brms1 #gamm_brms12 # withOneTime_brms1
rm(brms1)
print("input data and models loaded")
#============= set up empty listbs to store results ================
# young forest
delta_youngforest1990_df_listb <- omit_coords # df of delta_youngforest, calculate 1992-2000
delta_youngforest2000_df_listb <- omit_coords # df of delta_youngforest 2000-2010
delta_youngforest2010_df_listb <- omit_coords # df of delta_youngforest 2010-2020
delta_youngforest_total_df_listb <- omit_coords # df of delta_youngforest 1992-2020

# old forest
delta_oldforest1990_df_listb <- omit_coords # df of delta_oldforest, calculate 1992-2000
delta_oldforest2000_df_listb <- omit_coords # df of delta_oldforest 2000-2010
delta_oldforest2010_df_listb <- omit_coords # df of delta_oldforest 2010-2020
delta_oldforest_total_df_listb <- omit_coords # df of delta_oldforest 1992-2020

# grassland
delta_grassland1990_df_listb <- omit_coords # df of delta_grassland, calculate 1992-2000
delta_grassland2000_df_listb <- omit_coords # df of delta_grassland 2000-2010
delta_grassland2010_df_listb <- omit_coords # df of delta_grassland 2010-2020
delta_grassland_total_df_listb <- omit_coords # df of delta_grassland 1992-2020

print("empty lists set-up finished")
#===================== predict ====================
#--------------------- young forest --------------------------------------------
tic(); print("start: predict young forest")
#=============== predict young forest SOC from layers====================
# predict 1990_first
output_youngforest1990_first <- posterior_epred(brms_mod, newdata = input_youngforest1990_first, re_formula = NA)
# predict 1990_last
output_youngforest1990_last <- posterior_epred(brms_mod, newdata = input_youngforest1990_last, re_formula = NA)
# predict 2000_first
output_youngforest2000_first <- posterior_epred(brms_mod, newdata = input_youngforest2000_first, re_formula = NA)
# predict 2000_last
output_youngforest2000_last <- posterior_epred(brms_mod, newdata = input_youngforest2000_last, re_formula = NA)
# predict 2010_first
output_youngforest2010_first <- posterior_epred(brms_mod, newdata = input_youngforest2010_first, re_formula = NA)
# predict 2010_last
output_youngforest2010_last <- posterior_epred(brms_mod, newdata = input_youngforest2010_last, re_formula = NA)

#=============== calculate change between predicted layers =====================
# exp(1990_last) - exp(1990_first) (delta), then transform to each row representing a pixel
delta_youngforest1990 <- exp(output_youngforest1990_last) - exp(output_youngforest1990_first) # rows: posterior sample #, col: pixel #
delta_youngforest1990 <- data.frame(t(delta_youngforest1990))
colnames(delta_youngforest1990) <- paste0("value", 1:ncol(delta_youngforest1990))

# exp(2000_last) - exp(2000_first) (delta)
delta_youngforest2000 <- exp(output_youngforest2000_last) - exp(output_youngforest2000_first)
delta_youngforest2000 <- data.frame(t(delta_youngforest2000))
colnames(delta_youngforest2000) <- paste0("value", 1:ncol(delta_youngforest2000))

# exp(2010_last) - exp(2010_first) (delta)
delta_youngforest2010 <- exp(output_youngforest2010_last) - exp(output_youngforest2010_first)
delta_youngforest2010 <- data.frame(t(delta_youngforest2010))
colnames(delta_youngforest2010) <- paste0("value", 1:ncol(delta_youngforest2000))

# exp(2010_last) - exp(1990_first) (delta)
delta_youngforest_total <- exp(output_youngforest2010_last) - exp(output_youngforest1990_first)
delta_youngforest_total <- data.frame(t(delta_youngforest_total))
colnames(delta_youngforest_total) <- paste0("value",1:ncol(delta_youngforest_total))

#=============== save all results ====================
delta_youngforest1990_df_listb <- cbind(delta_youngforest1990_df_listb, delta_youngforest1990)
delta_youngforest2000_df_listb <- cbind(delta_youngforest2000_df_listb, delta_youngforest2000)
delta_youngforest2010_df_listb <- cbind(delta_youngforest2010_df_listb, delta_youngforest2010)
delta_youngforest_total_df_listb <- cbind(delta_youngforest_total_df_listb, delta_youngforest_total)

toc(); print("done: predict young forest")

#--------------------- old forest --------------------------------------------
tic(); print("start: predict old forest")
#=============== predict old forest SOC from layers====================
# predict 1990_first
output_oldforest1990_first <- posterior_epred(brms_mod, newdata = input_oldforest1990_first, re_formula = NA)
# predict 1990_last
output_oldforest1990_last <- posterior_epred(brms_mod, newdata = input_oldforest1990_last, re_formula = NA)
# predict 2000_first
output_oldforest2000_first <- posterior_epred(brms_mod, newdata = input_oldforest2000_first, re_formula = NA)
# predict 2000_last
output_oldforest2000_last <- posterior_epred(brms_mod, newdata = input_oldforest2000_last, re_formula = NA)
# predict 2010_first
output_oldforest2010_first <- posterior_epred(brms_mod, newdata = input_oldforest2010_first, re_formula = NA)
# predict 2010_last
output_oldforest2010_last <- posterior_epred(brms_mod, newdata = input_oldforest2010_last, re_formula = NA)

#=============== calculate change between predicted layers =====================
# exp(1990_last) - exp(1990_first) (delta), then transform to each row representing a pixel
delta_oldforest1990 <- exp(output_oldforest1990_last) - exp(output_oldforest1990_first) # rows: posterior sample #, col: pixel #
delta_oldforest1990 <- data.frame(t(delta_oldforest1990))
colnames(delta_oldforest1990) <- paste0("value", 1:ncol(delta_oldforest1990))

# exp(2000_last) - exp(2000_first) (delta)
delta_oldforest2000 <- exp(output_oldforest2000_last) - exp(output_oldforest2000_first)
delta_oldforest2000 <- data.frame(t(delta_oldforest2000))
colnames(delta_oldforest2000) <- paste0("value", 1:ncol(delta_oldforest2000))

# exp(2010_last) - exp(2010_first) (delta)
delta_oldforest2010 <- exp(output_oldforest2010_last) - exp(output_oldforest2010_first)
delta_oldforest2010 <- data.frame(t(delta_oldforest2010))
colnames(delta_oldforest2010) <- paste0("value", 1:ncol(delta_oldforest2000))

# exp(2010_last) - exp(1990_first) (delta)
delta_oldforest_total <- exp(output_oldforest2010_last) - exp(output_oldforest1990_first)
delta_oldforest_total <- data.frame(t(delta_oldforest_total))
colnames(delta_oldforest_total) <- paste0("value",1:ncol(delta_oldforest_total))

#=============== save all results ====================
delta_oldforest1990_df_listb <- cbind(delta_oldforest1990_df_listb, delta_oldforest1990)
delta_oldforest2000_df_listb <- cbind(delta_oldforest2000_df_listb, delta_oldforest2000)
delta_oldforest2010_df_listb <- cbind(delta_oldforest2010_df_listb, delta_oldforest2010)
delta_oldforest_total_df_listb <- cbind(delta_oldforest_total_df_listb, delta_oldforest_total)

toc(); print("done: predict old forest")

#--------------------- grassland --------------------------------------------
tic(); print("start: predict grassland")
#=============== predict grassland SOC from layers====================
# predict 1990_first
output_grassland1990_first <- posterior_epred(brms_mod, newdata = input_grassland1990_first, re_formula = NA)
# predict 1990_last
output_grassland1990_last <- posterior_epred(brms_mod, newdata = input_grassland1990_last, re_formula = NA)
# predict 2000_first
output_grassland2000_first <- posterior_epred(brms_mod, newdata = input_grassland2000_first, re_formula = NA)
# predict 2000_last
output_grassland2000_last <- posterior_epred(brms_mod, newdata = input_grassland2000_last, re_formula = NA)
# predict 2010_first
output_grassland2010_first <- posterior_epred(brms_mod, newdata = input_grassland2010_first, re_formula = NA)
# predict 2010_last
output_grassland2010_last <- posterior_epred(brms_mod, newdata = input_grassland2010_last, re_formula = NA)

#=============== calculate change between predicted layers =====================
# exp(1990_last) - exp(1990_first) (delta), then transform to each row representing a pixel
delta_grassland1990 <- exp(output_grassland1990_last) - exp(output_grassland1990_first) # rows: posterior sample #, col: pixel #
delta_grassland1990 <- data.frame(t(delta_grassland1990))
colnames(delta_grassland1990) <- paste0("value", 1:ncol(delta_grassland1990))

# exp(2000_last) - exp(2000_first) (delta)
delta_grassland2000 <- exp(output_grassland2000_last) - exp(output_grassland2000_first)
delta_grassland2000 <- data.frame(t(delta_grassland2000))
colnames(delta_grassland2000) <- paste0("value", 1:ncol(delta_grassland2000))

# exp(2010_last) - exp(2010_first) (delta)
delta_grassland2010 <- exp(output_grassland2010_last) - exp(output_grassland2010_first)
delta_grassland2010 <- data.frame(t(delta_grassland2010))
colnames(delta_grassland2010) <- paste0("value", 1:ncol(delta_grassland2000))

# exp(2010_last) - exp(1990_first) (delta)
delta_grassland_total <- exp(output_grassland2010_last) - exp(output_grassland1990_first)
delta_grassland_total <- data.frame(t(delta_grassland_total))
colnames(delta_grassland_total) <- paste0("value",1:ncol(delta_grassland_total))

#=============== save all results ====================
delta_grassland1990_df_listb <- cbind(delta_grassland1990_df_listb, delta_grassland1990)
delta_grassland2000_df_listb <- cbind(delta_grassland2000_df_listb, delta_grassland2000)
delta_grassland2010_df_listb <- cbind(delta_grassland2010_df_listb, delta_grassland2010)
delta_grassland_total_df_listb <- cbind(delta_grassland_total_df_listb, delta_grassland_total)

toc(); print("done: predict grassland")

# save
tic()
save(
  # young forest
  delta_youngforest1990_df_listb,delta_youngforest2000_df_listb,delta_youngforest2010_df_listb,delta_youngforest_total_df_listb,
  # old forest
  delta_oldforest1990_df_listb,delta_oldforest2000_df_listb,delta_oldforest2010_df_listb,delta_oldforest_total_df_listb,
  # grassland
  delta_grassland1990_df_listb,delta_grassland2000_df_listb,delta_grassland2010_df_listb,delta_grassland_total_df_listb,
  file = "./results/post_processing_bayesian_cap2_new_result_brms1_37k12chain.rda")
toc()
print("prediction data saved to post_processing_bayesian_cap2_new_result_brms1_37k12chain.rda")


