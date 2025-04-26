# predict SOC change from gamm model
# calculate SOC change by decade
# only produce SOC change

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
# load data (object name: df_std)
load("./data/SOC_time_series_for_analysis.rda")

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
mean_elev <- mean(df_std$elev_plo)
sd_elev <- sd(df_std$elev_plo)

# define range of MAT, MAP for each land cover type
# young forest
minMAT_yf <- min(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$MAT_cru)
maxMAT_yf <- max(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$MAT_cru)
minMAP_yf <- min(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$MAP_cru)
maxMAP_yf <- max(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$MAP_cru)

# old forest
minMAT_of <- min(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$MAT_cru)
maxMAT_of <- max(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$MAT_cru)
minMAP_of <- min(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$MAP_cru)
maxMAP_of <- max(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$MAP_cru)

# grassland
minMAT_g <- min(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$MAT_cru)
maxMAT_g <- max(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$MAT_cru)
minMAP_g <- min(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$MAP_cru)
maxMAP_g <- max(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$MAP_cru)

print("done: set input data range")
#================== load predictor rasters (MAT,MAP,forest_cond,land_cover) ======================
# MAT,MAP,forest_cond,land_cover layers generated from script prepare_layers.R
# clay content, soil n%, and elevation layers generated from resample_clay_n_elev.R

# MAT_year
mat_1992 <- raster("./data/mat_1992.tif")
mat_2000 <- raster("./data/mat_2000.tif")
mat_2010 <- raster("./data/mat_2010.tif")
mat_2018 <- raster("./data/mat_2018.tif")
mat_2020 <- raster("./data/mat_2020.tif")

# MAP_year
map_1992 <- raster("./data/map_1992.tif")
map_2000 <- raster("./data/map_2000.tif")
map_2010 <- raster("./data/map_2010.tif")
map_2018 <- raster("./data/map_2018.tif")
map_2020 <- raster("./data/map_2020.tif")

# n_pct
n_pct <- raster("./data/n_sg_halfdegree.tif")

# clay
clay <- raster("./data/clay_sg_halfdegree.tif")

# soil ph
ph <- raster("./data/ph_sg_halfdegree.tif")

print("predictor rasters loaded")

#================= convert input layers into dfs =================
# create empty dataframe with lat long of full half-degree grid
all_coords <- raster2df(mat_1992_df) %>%
  dplyr::select(x,y) %>%
  arrange(x,y)

# MAT_year
mat_1992_df <- merge(x=all_coords, y=raster2df(mat_1992), by=c("x","y"), all.x=T) %>% arrange(x,y)
mat_2000_df <- merge(x=all_coords, y=raster2df(mat_2000), by=c("x","y"), all.x=T) %>% arrange(x,y)
mat_2010_df <- merge(x=all_coords, y=raster2df(mat_2010), by=c("x","y"), all.x=T) %>% arrange(x,y)
mat_2018_df <- merge(x=all_coords, y=raster2df(mat_2018), by=c("x","y"), all.x=T) %>% arrange(x,y)
mat_2020_df <- merge(x=all_coords, y=raster2df(mat_2020), by=c("x","y"), all.x=T) %>% arrange(x,y)

# MAP_year (do above onward)
map_1992_df <- merge(x=all_coords, y=raster2df(map_1992), by=c("x","y"), all.x=T) %>% arrange(x,y)
map_2000_df <- merge(x=all_coords, y=raster2df(map_2000), by=c("x","y"), all.x=T) %>% arrange(x,y)
map_2010_df <- merge(x=all_coords, y=raster2df(map_2010), by=c("x","y"), all.x=T) %>% arrange(x,y)
map_2018_df <- merge(x=all_coords, y=raster2df(map_2018), by=c("x","y"), all.x=T) %>% arrange(x,y)
map_2020_df <- merge(x=all_coords, y=raster2df(map_2020), by=c("x","y"), all.x=T) %>% arrange(x,y)

# std_n_pct
std_n_pct_df <- merge(x=all_coords, y=raster2df(n_pct), by=c("x","y"), all.x=T) %>% 
  arrange(x,y) %>%
  mutate(value = (value - mean_n_pct)/sd_n_pct)

# std_clay
std_clay_df <- merge(x=all_coords, y=raster2df(clay), by=c("x","y"), all.x=T) %>% 
  arrange(x,y) %>%
  mutate(value = (value - mean_clay)/sd_clay)

# std_ph
std_ph_df <- merge(x=all_coords, y=raster2df(ph), by=c("x","y"), all.x=T) %>% 
  arrange(x,y) %>%
  mutate(value = (value - mean_ph)/sd_ph)

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
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_yf), minMAT_yf),
    MAP_cru = max(min(MAP_cru, maxMAP_yf), minMAP_yf)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)

input_youngforest1990_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2000,
  MAT_cru = mat_2000_df$value,
  MAP_cru = map_2000_df$value,
  forest_cond_gf_besnard60 = "young forest",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_yf), minMAT_yf),
    MAP_cru = max(min(MAP_cru, maxMAP_yf), minMAP_yf)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)

input_youngforest2000_first <- input_youngforest1990_last # 2000
input_youngforest2000_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2010,
  MAT_cru = mat_2010_df$value,
  MAP_cru = map_2010_df$value,
  forest_cond_gf_besnard60 = "young forest",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_yf), minMAT_yf),
    MAP_cru = max(min(MAP_cru, maxMAP_yf), minMAP_yf)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)


input_youngforest2010_first <- input_youngforest2000_last # 2010
input_youngforest2010_last <-  data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2020,
  MAT_cru = mat_2020_df$value,
  MAP_cru = map_2020_df$value,
  forest_cond_gf_besnard60 = "young forest",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_yf), minMAT_yf),
    MAP_cru = max(min(MAP_cru, maxMAP_yf), minMAP_yf)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)

# old forest  
input_oldforest1990_first <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 1992,
  MAT_cru = mat_1992_df$value,
  MAP_cru = map_1992_df$value,
  forest_cond_gf_besnard60 = "old forest",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_of), minMAT_of),
    MAP_cru = max(min(MAP_cru, maxMAP_of), minMAP_of)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)

input_oldforest1990_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2000,
  MAT_cru = mat_2000_df$value,
  MAP_cru = map_2000_df$value,
  forest_cond_gf_besnard60 = "old forest",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_of), minMAT_of),
    MAP_cru = max(min(MAP_cru, maxMAP_of), minMAP_of)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)


input_oldforest2000_first <- input_oldforest1990_last # 2000
input_oldforest2000_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2010,
  MAT_cru = mat_2010_df$value,
  MAP_cru = map_2010_df$value,
  forest_cond_gf_besnard60 = "old forest",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_of), minMAT_of),
    MAP_cru = max(min(MAP_cru, maxMAP_of), minMAP_of)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)


input_oldforest2010_first <- input_oldforest2000_last # 2010
input_oldforest2010_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2020,
  MAT_cru = mat_2020_df$value,
  MAP_cru = map_2020_df$value,
  forest_cond_gf_besnard60 = "old forest",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_of), minMAT_of),
    MAP_cru = max(min(MAP_cru, maxMAP_of), minMAP_of)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)

# grassland
input_grassland1990_first <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 1992,
  MAT_cru = mat_1992_df$value,
  MAP_cru = map_1992_df$value,
  forest_cond_gf_besnard60 = "grassland",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_g), minMAT_g),
    MAP_cru = max(min(MAP_cru, maxMAP_g), minMAP_g)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)

input_grassland1990_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2000,
  MAT_cru = mat_2000_df$value,
  MAP_cru = map_2000_df$value,
  forest_cond_gf_besnard60 = "grassland",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_g), minMAT_g),
    MAP_cru = max(min(MAP_cru, maxMAP_g), minMAP_g)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)

input_grassland2000_first <- input_grassland1990_last # 2000
input_grassland2000_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2010,
  MAT_cru = mat_2010_df$value,
  MAP_cru = map_2010_df$value,
  forest_cond_gf_besnard60 = "grassland",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_g), minMAT_g),
    MAP_cru = max(min(MAP_cru, maxMAP_g), minMAP_g)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)

input_grassland2010_first <- input_grassland2000_last # 2010
input_grassland2010_last <- data.frame(
  x=all_coords$x,
  y=all_coords$y,
  observation_year = 2020,
  MAT_cru = mat_2020_df$value,
  MAP_cru = map_2020_df$value,
  forest_cond_gf_besnard60 = "grassland",
  std_n_pct_plo = std_n_pct_df$value,
  std_clay_plo = std_clay_df$value,
  std_ph_plo = std_ph_df$value,
  siteID = 0,
  plotID = 0
) %>% na.omit() %>%
  rowwise() %>% 
  dplyr::mutate(
    MAT_cru = max(min(MAT_cru, maxMAT_g), minMAT_g),
    MAP_cru = max(min(MAP_cru, maxMAP_g), minMAP_g)
  ) %>% ungroup() %>%
  mutate(
    std_year = (observation_year - mean_year) / sd_year,
    std_MAT_cru = (MAT_cru - mean_MAT) / sd_MAT,
    std_MAP_cru = (MAP_cru - mean_MAP) / sd_MAP)


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

#======================= load model ===========================
load("./results/gamm_brms12.rda")
print("input data and models loaded")

# assign name to model
brms_mod <- gamm_brms12

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

# save (this is not saved in the repo, but can be generated from the script)
tic()
save(
  # young forest
  delta_youngforest1990_df_listb,delta_youngforest2000_df_listb,delta_youngforest2010_df_listb,delta_youngforest_total_df_listb,
  # old forest
  delta_oldforest1990_df_listb,delta_oldforest2000_df_listb,delta_oldforest2010_df_listb,delta_oldforest_total_df_listb,
  # grassland
  delta_grassland1990_df_listb,delta_grassland2000_df_listb,delta_grassland2010_df_listb,delta_grassland_total_df_listb,
  file = "./results/prediction_result.rda")
toc()
print("prediction data saved to prediction_result.rda")


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
  file = "./results/prediction_inputs.rda")
toc()
print("inputs data saved to prediction_inputs.rda")
