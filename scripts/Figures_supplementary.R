# supplementary figures 
# replot data availability after adding singel-time data
rm(list = ls(all = TRUE))

# package and direcotry
library(dplyr)
library(brms)
library(raster)
library(terra)
library(rasterVis)
library(tictoc)
library(RColorBrewer)
library(ggplot2)
library(ggnewscale)
library(plotbiomes)
library(cowplot)
library(sf)
library(grid)
library(gridExtra)

# working directory should be set by default to the folder of entire repo (R project)

map_dir <- "./data/"
fig_dir <- "./figures"
#---------- load functions --------------------
# function that convert raster to dataframe 
raster2df <- function(layer) {
  layer_spdf <- as(layer, "SpatialPixelsDataFrame")
  layer_df <- as.data.frame(layer_spdf)
  colnames(layer_df) <- c("value", "x", "y")
  return(layer_df)
}

# calculate number of sites at observation year for young forest
count_nsite <- function(df=df_bootstrap, type_str="young forest") {
  distinct_site_year <- df %>% # truncate years by biome (df_biome)
    filter(forest_cond_gf_besnard60==type_str) %>%
    distinct(siteID,observation_year) %>% 
    group_by(siteID) %>% arrange(observation_year) %>%
    dplyr::mutate(first_observation_year = first(observation_year),
                  last_observation_year = last(observation_year)) %>%
    ungroup() %>% 
    distinct(siteID,first_observation_year,last_observation_year)
  
  n_site_df <- data.frame(year=seq(1950,2022,1)) %>%
    dplyr::mutate(
      n_site = sapply(year, function(y) sum(distinct_site_year$first_observation_year <= y & distinct_site_year$last_observation_year >= y)),
      forest_cond_gf_besnard60=type_str)
  
  return(n_site_df)
}

# plot number of sites vs. year
plot_nsite <- function(nsite_df,type_str="young forest") {
  p <- ggplot(nsite_df) + geom_line(aes(x=year,y=n_site,color=forest_cond_gf_besnard60)) +
    scale_color_manual(values=type_palette) +
    labs(x=NULL, y=NULL) + # , title=type_str
    theme_classic() +
    theme(plot.title = element_text(size=10),
          legend.position = "none")
  return(p)
}

# plot map of single value in scico color scheme
plot_single_value <- function(delta_df, var_name="value", shrubland=F) {
  # delta_df must have column x(lat), y(long), and a value column
  colnames(delta_df)[which(colnames(delta_df)==var_name)] <- "var"
  
  # specify min max numbers in scale
  min_val <- min(delta_df$var, na.rm=T)
  max_val <- max(delta_df$var, na.rm=T)
  
  # # define legend
  # if (shrubland) {
  #   labels <- c(paste0("≤", scales::number_format(accuracy = 1)(min_val)), 
  #               paste0("≥", scales::number_format(accuracy = 1)(max_val)))
  # } else {
  #   labels <- scales::number_format(accuracy = 1, scale = 1)(c(min_val, max_val))
  # }
  
  ggplot(countries_sf) +
    geom_sf(fill = "lightgray", col="gray", alpha=0.5) +
    geom_tile(data=delta_df, aes(x=x, y=y, fill=var, color=var, width=1,height=1), alpha=1) +
    scico::scale_fill_scico(palette = "vik",
                            breaks=c(min_val, max_val),
                            labels = labels) + 
    scico::scale_color_scico(palette = "vik",
                             breaks=c(min_val, max_val),
                             labels = labels, guide = "none") + 
    labs(x=NULL, y=NULL, fill=NULL) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          plot.title = element_blank(),
          axis.title.x = element_blank(),  # Remove x-axis title
          axis.title.y = element_blank(),  # Remove y-axis title
          axis.text.x = element_blank(),    # Remove x-axis text
          axis.text.y = element_blank(),    # Remove y-axis text
          axis.ticks.x = element_blank(),   # Remove x-axis ticks
          axis.ticks.y = element_blank())   # Remove y-axis ticks))
  # plot.margin = margin(1, 1, 1, 1) makes margin too small
}
# plot single value in viridis
plot_single_value_viridis <- function(delta_df, var_name="value", shrubland=F) {
  # delta_df must have column x(lat), y(long), and a value column
  colnames(delta_df)[which(colnames(delta_df)==var_name)] <- "var"
  
  # specify min max numbers in scale
  min_val <- min(delta_df$var)
  max_val <- max(delta_df$var)
  # define legend
  if (shrubland) {
    labels <- c(paste0("≤", scales::number_format(accuracy = 1)(min_val)), 
                paste0("≥", scales::number_format(accuracy = 1)(max_val)))
  } else {
    labels <- scales::number_format(accuracy = 1, scale = 1)(c(min_val, max_val))
  }
  
  ggplot(countries_sf) +
    geom_sf(fill = "lightgray", col="gray", alpha=0.5) +
    geom_tile(data=delta_df, aes(x=x, y=y, fill=var, color=var), alpha=1) +
    scale_fill_viridis_c(option = "viridis",
                         breaks=c(min_val, max_val),
                         labels = labels) +
    scale_color_viridis_c(option = "viridis",
                          breaks=c(min_val, max_val),
                          labels = labels, guide = "none") +
    labs(x=NULL, y=NULL, fill=NULL) + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  # Remove x-axis title
          axis.title.y = element_blank(),  # Remove y-axis title
          axis.text.x = element_blank(),    # Remove x-axis text
          axis.text.y = element_blank(),    # Remove y-axis text
          axis.ticks.x = element_blank(),   # Remove x-axis ticks
          axis.ticks.y = element_blank(),   # Remove y-axis ticks))
          legend.key.size = unit(0.4, "cm")
    )   
  # plot.margin = margin(1, 1, 1, 1) makes margin too small
}
# plot map of estimated change, negative=red, positive=blue
plot_single_delta <- function(delta_df, var_name="fit_delta", shrubland=F) {
  
  # delta_df must have column x(lat), y(long), and a value column
  colnames(delta_df)[which(colnames(delta_df)==var_name)] <- "var"
  
  # specify min max numbers in scale
  min_val <- min(delta_df$var, na.rm=T)
  max_val <- max(delta_df$var, na.rm=T)
  
  # define legend
  if (shrubland) {
    labels <- c(paste0("≤", scales::number_format(accuracy = 1)(min_val)), 
                "0", 
                paste0("≥", scales::number_format(accuracy = 1)(max_val)))
  } else {
    labels <- scales::number_format(accuracy = 1, scale = 1)(c(min_val, 0, max_val))
  }
  
  # plot
  p<-ggplot(countries_sf) +
    geom_sf(fill = "lightgray", col="gray", alpha=0.5) +
    #geom_sf(data=forest_biomes, fill = "darkgreen", col=NA, pwd=0.5, alpha=0.5) +
    #geom_tile(data=ideal_layer_contour_df,aes(x=x, y=y), fill="darkgreen", alpha=0.5) +
    geom_tile(data=delta_df, aes(x=x, y=y, fill=var, color=var, width=1, height=1), alpha=1) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint =0,
                         breaks=c(min_val, 0, max_val),
                         labels = labels) +
    scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint =0,
                          breaks=c(min_val, 0, max_val),
                          labels = labels, guide = "none") +
    labs(x=NULL, y=NULL, fill=NULL) + 
    #ggtitle(expression(paste(Delta, "SOC (Mg/ha) 0-30cm in young forests, 1992-2015"))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  # Remove x-axis title
          axis.title.y = element_blank(),  # Remove y-axis title
          axis.text.x = element_blank(),    # Remove x-axis text
          axis.text.y = element_blank(),    # Remove y-axis text
          axis.ticks.x = element_blank(),   # Remove x-axis ticks
          axis.ticks.y = element_blank(),  # Remove y-axis ticks))
          plot.margin = margin(2, 2, 2, 2), # 1,1,1,1 makes margin too small
          legend.key.size = unit(0.4, "cm")
    )   
  
  return(p)
}

#---------- load and prepare data -------------------------
# new dataset (final object name: df_std)
load("./data/SOC_withNewData_for_analysis.rda")
df_std <- df_std_withNewData; rm(df_std_withNewData)

# time series data
df_std_ts <- df_std %>% filter(!grepl("OLM-", siteID)) %>%
  mutate(data_type="Time series") %>%
  mutate(data_type=factor(data_type, levels=c("Time series", "Single time")))

# one time data
df_onetime <- df_std %>% filter(grepl("OLM-", siteID)) %>%
  mutate(data_type="Single time") %>%
  mutate(data_type=factor(data_type, levels=c("Time series", "Single time")))

# pixel level results
load("./results/find_CI_bayesian_cap2_exclude_same_extent_pixel_summary.RData")
# load input predictor data
load("./results/predicted_inputs_brms1_37k12chain.rda") # all applicable and predicted pixels in separate dataframes, with land use proportion >0
# load land cover extent
load(paste0(map_dir, "proportion_decade_df.rda"))

# load model
# load("./results/gamm_brms12.rda") # change this
# mod <- gamm_brms12
load("./results/brms1_withOneTime_37k12chain.rda")
mod <- brms1; rm(brms1)

# input layers (code to generate these files is in 00_download_prepare_spatial_data.R)
# mat_1992
mat_1992 <- raster(paste0(map_dir, "mat_1992.tif"))
# map_1992
map_1992 <- raster(paste0(map_dir, "map_1992.tif"))
# n_pct
n_pct <- raster(paste0(map_dir, "n_sg_halfdegree.tif"))
# clay
clay <- raster(paste0(map_dir, "clay_sg_halfdegree.tif"))
# ph
ph <- raster(paste0(map_dir, "ph_sg_halfdegree.tif"))

# convert tifs to df
mat_1992_df <- raster2df(mat_1992)
map_1992_df <- raster2df(map_1992)
n_pct_df <- raster2df(n_pct) 
clay_df <- raster2df(clay)
ph_df <- raster2df(ph)

# # load world administrative boundaries
# # https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/?flg=en-us
basemap_dir <- "./data"
countries_sf <- read_sf(paste0(basemap_dir, "world-administrative-boundaries/world-administrative-boundaries.shp"))
countries_sf <- st_make_valid(countries_sf)
countries_sf <- st_transform(countries_sf, 4326)

# OR define min and max of MAT, MAP without outliers (within 1.5*IQR from Q1 and Q3)
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

# set min and max values without outliers
# young forest
maxMAT_yf <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$MAT_cru)
minMAT_yf <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$MAT_cru)
maxMAP_yf <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$MAP_cru)
minMAP_yf <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$MAP_cru)
maxn_yf <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$n_pct_plo)
minn_yf <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$n_pct_plo)
maxclay_yf <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$clay_plo)
minclay_yf <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$clay_plo)
maxph_yf <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$ph_plo)
minph_yf <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="young forest",]$ph_plo)
# old forest
maxMAT_of <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$MAT_cru)
minMAT_of <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$MAT_cru)
maxMAP_of <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$MAP_cru)
minMAP_of <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$MAP_cru)
maxn_of <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$n_pct_plo)
minn_of <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$n_pct_plo)
maxclay_of <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$clay_plo)
minclay_of <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$clay_plo)
maxph_of <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$ph_plo)
minph_of <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="old forest",]$ph_plo)
# grassland
maxMAT_g <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$MAT_cru)
minMAT_g <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$MAT_cru)
maxMAP_g <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$MAP_cru)
minMAP_g <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$MAP_cru)
maxn_g <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$n_pct_plo)
minn_g <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$n_pct_plo)
maxclay_g <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$clay_plo)
minclay_g <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$clay_plo)
maxph_g <- find_max_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$ph_plo)
minph_g <- find_min_wo_outliers(df_std[df_std$forest_cond_gf_besnard60=="grassland",]$ph_plo)

#---------- define palettes ----------------------------------------------------
type_palette <- c(
  "young forest" = "#c8445b",
  "old forest" = "#4d5ca1",
  "grassland" = "#66aa8b"
)

# use this website to lighten 30% https://mdigi.tools/lighten-color/#c8445b
type_palette_light <- c(
  "young forest" = "#de8f9d", 
  "old forest" = "#919bca",
  "grassland" = "#a3ccb9"
)

shape_palette <- c(
  "Single time" = 21,
  "Time series" = 22)

extrapolation_variable_palette <- c(
  #"none"= "gray", #"gray", # "#f5e8df" or "#d7e0ca"
  "MAT"="#CC79A7",
  "MAP"="#56B4E9",
  "soil N"="#994F00",
  "soil clay"="#E69F00",
  "soil pH"="#F0E442"
)

theme_set(theme_minimal(base_family = "Helvetica", base_size = 14))

#-----------------Figure S1 data distribution-----------------------------------
# (a) soil data on world map

df_std_plot <- df_std %>%
  mutate(data_type=ifelse(grepl("OLM-", siteID), "Single time", "Time series")) %>%
  mutate(data_type=factor(data_type, levels=c( "Single time", "Time series"))) %>%
  arrange(data_type)

# try to show time series and onetime data on one map
p_data_map <- 
  ggplot(countries_sf) +
  geom_sf(fill = "lightgray", col="gray", alpha=0.5) +
  geom_point(data=df_onetime, aes(x=long, y=lat, color = forest_cond_gf_besnard60, fill = forest_cond_gf_besnard60, shape=data_type), 
             alpha=1, size=1.2, stroke=0.1) +
  scale_shape_manual(values = shape_palette) +
  scale_color_manual(values = type_palette_light, guide="none") +
  scale_fill_manual(values = type_palette_light, guide="none") +
  new_scale_fill() +
  geom_point(data=df_std_ts, aes(x=long, y=lat, fill = forest_cond_gf_besnard60, shape=data_type), 
             color="black", alpha=1, size=1.2, stroke = 0.1) +
  scale_fill_manual(values = type_palette,
                    labels = c("Young forest", "Old forest", "Grassland")) +
  labs(tag="a", x=NULL, y=NULL) +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        legend.position = "bottom") + 
  guides(shape = guide_legend(order=1, override.aes = list(
    shape=c(16,22), color=c("gray70","black"), fill=c("gray70", "gray40"), stroke=0.5, size = 3)),
    fill = guide_legend(order=2, override.aes = list(shape=21, color=NA, size = 3)) )

p_data_map

# (b) number of sites vs. year

# plot
p_data_year <- ggplot(df_std_plot) +
  geom_histogram(aes(x = observation_year, alpha=data_type, fill=forest_cond_gf_besnard60),
                 binwidth = 1, position = "stack") +
  scale_alpha_manual(values = c("Time series" = 1, "Single time" = 0.7), name=NULL) + # guide="none"
  scale_fill_manual(values = type_palette, guide="none") +
  facet_wrap(~forest_cond_gf_besnard60, nrow = 1, 
             labeller = labeller(
               forest_cond_gf_besnard60 = 
                 c("young forest"="Young forest",
                   "old forest"="Old forest",
                   "grassland"="Grassland"))
  ) +
  labs(tag="b",x = "Year",y = "Number of observation") +
  guides(alpha = guide_legend(override.aes = list(alpha=c(0.4,1), size = 3))
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

p_data_year

# assemble
fig_S1_data_distribution <- grid.arrange(p_data_map, p_data_year, 
                                         ncol=1, heights=c(1,1))

# save
ggsave(paste0(fig_dir, "/figureS1_data_distribution.png"), 
       plot = fig_S1_data_distribution, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 160 / 25.4, units = "in")

#-----------------Figure S2 data coverage compared to biome and prediction-----

# data against whittaker plot (with rate)
df_change <- df_std_ts %>% group_by(plotID) %>%
  arrange(plotID, observation_year) %>%
  filter(observation_year!=first(observation_year)) %>% # delete first observation
  #dplyr::slice_tail(n=1) %>% # keep only rate from first to last observation
  ungroup() %>%
  dplyr::mutate( # calculate rate
    SOC_rate = abs30_rate_mgha_yr
  ) %>%
  mutate( # truncate outliers
    SOC_rate_less10 = case_when(
      SOC_rate > 10 ~ 10,
      SOC_rate < -10 ~ -10,
      TRUE ~ SOC_rate
    ),
    SOC_rate_sqrt = case_when(
      SOC_rate>0 ~ sqrt(SOC_rate_less10),
      SOC_rate<=0 ~ -1*sqrt(abs(SOC_rate_less10)))
  )

# create labels for color scale
min_rate <- min(df_change$SOC_rate_sqrt)
max_rate <- max(df_change$SOC_rate_sqrt)
labels <- c(paste0("≤", -10),
            "0", 
            paste0("≥", 10))

p_whittaker_rate <- whittaker_base_plot() + 
  new_scale_fill() +
  facet_wrap(~forest_cond_gf_besnard60,
             nrow = 1, 
             labeller = labeller(
               forest_cond_gf_besnard60 = 
                 c("young forest"="Young forest",
                   "old forest"="Old forest",
                   "grassland"="Grassland")) ) +
  geom_point(data=df_onetime, 
             aes(x=mean_mat_site,y=mean_map_site/10,
                 shape=data_type), # color=forest_cond_gf_besnard60, fill=forest_cond_gf_besnard60, 
             color="gray", fill="gray", size=1, alpha=1, stroke=0.1) +
  scale_shape_manual(values = shape_palette, guide="none") +
  scale_color_manual(values = type_palette_light, guide="none") +
  scale_fill_manual(values = type_palette_light, guide="none") +
  new_scale_fill() +
  geom_point(data=df_change, 
             aes(x=mean_mat_site,y=mean_map_site/10, 
                 fill = SOC_rate_sqrt, shape=data_type), 
             color="black", alpha=1, size=1, stroke = 0.1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint =0, # low read, high blue
                       breaks=c(min_rate,  0, max_rate),
                       labels = labels, name="SOC change from time series \n (Mg C ha-1 yr-1)",
                       guide = guide_colorbar(order = 1)) +
  labs() +
  theme_bw() + 
  theme(lgend.position="right")

p_whittaker_rate

# save
ggsave(paste0(fig_dir, "/figureS2_whittaker_rate.png"), 
       plot = p_whittaker_rate, bg = "white", dpi = 600, width = 183 / 25.4, height = 130 / 25.4, units = "in")


# # data against whittaker plot (without rate)
# p_whittaker <- whittaker_base_plot() + 
#   new_scale_fill() +
#   facet_wrap(~forest_cond_gf_besnard60,
#              nrow = 1, 
#              labeller = labeller(
#                forest_cond_gf_besnard60 = 
#                  c("young forest"="Young forest",
#                    "old forest"="Old forest",
#                    "grassland"="Grassland")) ) +
#   geom_point(data=df_onetime, 
#              aes(x=mean_mat_site,y=mean_map_site/10,
#                  color=forest_cond_gf_besnard60, fill=forest_cond_gf_besnard60, shape=data_type), 
#              size=1, alpha=1, stroke=0.1) +
#   scale_shape_manual(values = shape_palette, guide="none") +
#   scale_color_manual(values = type_palette_light, guide="none") +
#   scale_fill_manual(values = type_palette_light, guide="none") +
#   new_scale_fill() +
#   geom_point(data=df_std_ts, 
#              aes(x=mean_mat_site,y=mean_map_site/10, 
#                  fill = forest_cond_gf_besnard60, shape=data_type), 
#              color="black", alpha=1, size=1, stroke = 0.1) +
#   scale_fill_manual(values = type_palette, guide="none") +
#   labs(tag="b") +
#   theme_bw() + 
#   theme(lgend.position="right")
# 
# p_whittaker

#-----------------Figure S3 heatmaps of MAT, MAP, year -------------------------

# generate 3D dataframe based on MAT, MAP
heatmap_MAT_pred <- function(gmod, df, cond_str="young forest", yr_span=c(1957,2020), MAT_span=c(-30,30)) {
  
  df_cond <- df %>% filter(forest_cond_gf_besnard60 == cond_str)
  
  MAT_ngrid <- MAT_span[2] - MAT_span[1] + 1
  yr_ngrid <- yr_span[2] - yr_span[1] + 1
  cond_name <- case_when(cond_str=="young forest" ~ "young.forest",
                         cond_str=="old forest" ~ "old.forest", 
                         cond_str=="grassland" ~ "grassland",
                         cond_str=="shrubland" ~ "shrubland")
  
  # construct grid manually 
  mod_pred_of_mat <- data.frame(
    year = rep(seq(yr_span[1], yr_span[2], 1), MAT_ngrid),
    MAT_cru = rep(MAT_span[1]:MAT_span[2], each = yr_ngrid),
    MAP_cru = mean(df_cond$MAP_cru),
    forest_cond_gf_besnard60 = cond_str,
    std_clay_plo = 0,
    std_n_pct_plo = 0,
    std_ph_plo = 0,
    siteID = 0,
    plotID = 0
  ) %>% 
    mutate(std_MAT_cru = (MAT_cru - mean(df$MAT_cru)) / sd(df$MAT_cru),
           std_MAP_cru = (MAP_cru - mean(df$MAP_cru)) / sd(df$MAP_cru),
           std_year = (year - mean(df$observation_year)) / sd(df$observation_year)
    )
  
  pred <- posterior_epred(gmod, newdata = mod_pred_of_mat, re_formula = NA)
  pred_summary <- data.frame(
    fit_mean = apply(pred, 2, mean),
    fit_sd = apply(pred, 2, sd),
    fit_2.5 = apply(pred, 2, quantile, probs = 0.025),
    fit_97.5 = apply(pred, 2, quantile, probs = 0.975)
  ) %>%
    mutate(
      exp_fit_mean = exp(fit_mean),
      exp_fit_sd = exp(fit_sd),
      exp_fit_2.5 = exp(fit_2.5),
      exp_fit_97.5 = exp(fit_97.5)
    )
  
  mod_pred_of_mat <- cbind(mod_pred_of_mat, pred_summary)
  
  # plot
  plot <- ggplot(mod_pred_of_mat) + 
    geom_raster(aes(year, MAT_cru, fill =exp_fit_mean)) +
    scale_fill_viridis_c(option = "viridis") +
    geom_point(data=df_std[df_std$forest_cond_gf_besnard60==cond_str,],
               aes(x=observation_year, y=MAT_cru), size=0.05,alpha=0.2,color="white") + 
    xlim(yr_span) +  ylim(MAT_span) +
    labs(fill="SOC (Mg/ha)", x=NULL, y="MAT (°C)") +
    theme(#panel.grid.major = element_blank(), 
      #panel.grid.minor = element_blank(),
      legend.margin = margin(t= -2, l=-2, r=-2, unit = "pt"),
      legend.title = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      title = element_text(size = 10))
  
  return(plot)
}
heatmap_MAP_pred <- function(gmod, df, cond_str="young forest", yr_span=c(1957,2020), MAP_span=c(0,6000)) {
  
  df_cond <- df %>% filter(forest_cond_gf_besnard60 == cond_str)
  
  MAP_ngrid <- (MAP_span[2] - MAP_span[1])/100 + 1
  yr_ngrid <- yr_span[2] - yr_span[1] + 1
  cond_name <- case_when(cond_str=="young forest" ~ "young.forest",
                         cond_str=="old forest" ~ "old.forest", 
                         cond_str=="grassland" ~ "grassland",
                         cond_str=="shrubland" ~ "shrubland")
  
  mod_pred_of_mat <- data.frame(
    year = rep(seq(yr_span[1], yr_span[2], 1), MAP_ngrid),
    MAT_cru = mean(df_cond$MAT_cru),
    MAP_cru = rep(seq(MAP_span[1], MAP_span[2], by = 100), each = yr_ngrid),
    forest_cond_gf_besnard60 = cond_str,
    std_clay_plo = 0,
    std_n_pct_plo = 0,
    std_ph_plo = 0,
    siteID = 0,
    plotID = 0
  ) %>% 
    mutate(std_MAT_cru = (MAT_cru - mean(df$MAT_cru)) / sd(df$MAT_cru),
           std_MAP_cru = (MAP_cru - mean(df$MAP_cru)) / sd(df$MAP_cru),
           std_year = (year - mean(df$observation_year)) / sd(df$observation_year)
    )
  
  pred <- posterior_epred(gmod, newdata = mod_pred_of_mat, re_formula = NA)
  
  pred_summary <- data.frame(
    fit_mean = apply(pred, 2, mean),
    fit_sd = apply(pred, 2, sd),
    fit_2.5 = apply(pred, 2, quantile, probs = 0.025),
    fit_97.5 = apply(pred, 2, quantile, probs = 0.975)
  ) %>%
    mutate(
      exp_fit_mean = exp(fit_mean),
      exp_fit_sd = exp(fit_sd),
      exp_fit_2.5 = exp(fit_2.5),
      exp_fit_97.5 = exp(fit_97.5)
    )
  
  mod_pred_of_mat <- cbind(mod_pred_of_mat, pred_summary)
  
  plot <- ggplot(mod_pred_of_mat) + 
    geom_raster(aes(year, MAP_cru, fill = exp_fit_mean)) +
    scale_fill_viridis_c(option = "viridis") +
    geom_point(data=df_std[df_std$forest_cond_gf_besnard60==cond_str,],
               aes(x=observation_year, y=MAP_cru), size=0.05,alpha=0.2, color="white") + 
    xlim(yr_span) + ylim(MAP_span) +
    labs(fill= "SOC (Mg/ha)", x=NULL, y="MAP (mm)") +
    theme(#panel.grid.major = element_blank(), 
      #panel.grid.minor = element_blank(),
      legend.margin = margin(t= -2, l=-2, r=-2, unit = "pt"),
      legend.title = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      title = element_text(size = 10))
  
  return(plot)
}

# apply functions to gmod
plot_yf_MAT_pred <- heatmap_MAT_pred(mod, df_std, cond_str="young forest",
                                     yr_span=c(1992,2020), MAT_span=c(minMAT_yf,maxMAT_yf)) + ggtitle("Young Forest")
plot_of_MAT_pred <- heatmap_MAT_pred(mod, df_std, cond_str="old forest",
                                     yr_span=c(1992,2020), MAT_span=c(minMAT_of,maxMAT_of)) + ggtitle("Old Forest")
plot_g_MAT_pred <- heatmap_MAT_pred(mod, df_std, cond_str="grassland",
                                    yr_span=c(1992,2020), MAT_span=c(minMAT_g,maxMAT_g)) + ggtitle("Grassland")

plot_yf_MAP_pred <- heatmap_MAP_pred(mod, df_std, cond_str="young forest",
                                     yr_span=c(1992,2020), MAP_span=c(minMAP_yf,maxMAP_yf)) + ggtitle("Young Forest")
plot_of_MAP_pred <- heatmap_MAP_pred(mod, df_std, cond_str="old forest",
                                     yr_span=c(1992,2020), MAP_span=c(minMAP_yf,maxMAP_yf)) + ggtitle("Old Forest")
plot_g_MAP_pred <- heatmap_MAP_pred(mod, df_std, cond_str="grassland",
                                    yr_span=c(1992,2020), MAP_span=c(minMAP_g,maxMAP_g)) + ggtitle("Grassland")

# assemble
p_heatmap <- grid.arrange(plot_yf_MAT_pred,plot_yf_MAP_pred,
                          plot_of_MAT_pred,plot_of_MAP_pred,
                          plot_g_MAT_pred,plot_g_MAP_pred,
                          ncol=2)

# save
ggsave(paste0(fig_dir, "/figuresS?_heatmap.png"),# "./figureS3_heatmap.png", 
       plot = p_heatmap, bg = "white", dpi = 600, width = 183 / 25.4, height = 183 / 25.4, units = "in")


#-----------------Figure S4 extrapolation extent ------------------------------

# alpha for variable that needs extrapolation:
alpha_extrapolation <- 0.3

# plot with explicit which variable is out of range
plot_extrapolation_variable <- function(input_df,type_str="young forest") {
  
  # define legend label
  if (type_str=="young forest") {
    landcover_label <- "Young forest"
  } else if (type_str=="old forest") {
    landcover_label <- "Old forest"
  } else if (type_str=="grassland") {
    landcover_label <- "Grassland"
  }
  
  # pivot long input_df
  predicted_df <- input_df %>%
    filter(extrapolation_fraction == 0) %>%
    mutate(extrapolation_type = "none",
           land_cover = type_str)
  
  extrapolate_df <- bind_rows(
    input_df %>%
      filter(MAT_extrapolation) %>%
      mutate(extrapolation_type = "MAT"),
    input_df %>%
      filter(MAP_extrapolation) %>%
      mutate(extrapolation_type = "MAP"),
    input_df %>%
      filter(n_extrapolation) %>%
      mutate(extrapolation_type = "soil N"),
    input_df %>%
      filter(clay_extrapolation) %>%
      mutate(extrapolation_type = "soil clay"),
    input_df %>%
      filter(ph_extrapolation) %>%
      mutate(extrapolation_type = "soil pH")
  )
  
  plot <- ggplot(countries_sf) +
    geom_sf(fill = "gray100", col="gray", alpha=0.5) +
    geom_tile(data=extrapolate_df, aes(x=x,y=y,fill=extrapolation_type), alpha=alpha_extrapolation) +
    scale_fill_manual(values = extrapolation_variable_palette, 
                      breaks = c("MAT", "MAP", "soil N", "soil clay", "soil pH"),
                      name = "Excluded pixels due to \nout-of-range predictor(s)",) +
    new_scale_fill() +
    geom_tile(data=predicted_df, aes(x=x,y=y, fill=land_cover, color=land_cover) ) +
    scale_fill_manual(values = type_palette, 
                      labels = landcover_label,
                      name = "Predicted pixels") +
    scale_color_manual(values = type_palette, guide = "none") +
    guides(fill = guide_legend(order = 1),
           `fill_newscale` = guide_legend(order = 2)) +
    labs(x=NULL, y=NULL) + 
    #ggtitle(type_str) +
    #ggtitle(expression(paste(Delta, "SOC (Mg/ha) 0-30cm in young forests, 1992-2015"))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  # Remove x-axis title
          axis.title.y = element_blank(),  # Remove y-axis title
          axis.text.x = element_blank(),    # Remove x-axis text
          axis.text.y = element_blank(),    # Remove y-axis text
          axis.ticks.x = element_blank(),   # Remove x-axis ticks
          axis.ticks.y = element_blank(),  # Remove y-axis ticks
          legend.position = "right",
          # legend.position = "inside",
          # legend.position.inside=c(0.25,0.2),
          # legend.background = element_rect(fill = NA, color = NA),
          # legend.key.size = unit(0.4, "cm"),
          # legend.text = element_text(size = 6),
          # legend.title = element_text(size = 6) 
    )
  # plot.margin = margin(1, 1, 1, 1) makes margin too small
  return(plot)
}

extrapolation_variable_yf <- plot_extrapolation_variable(input_youngforest_all,"young forest")+ggtitle("Young forest")
extrapolation_variable_of <- plot_extrapolation_variable(input_oldforest_all,"old forest")+ggtitle("Old forest")
extrapolation_variable_g <- plot_extrapolation_variable(input_grassland_all,"grassland")+ggtitle("Grassland")

prediction_extent <- grid.arrange(
  extrapolation_variable_yf,
  extrapolation_variable_of,
  extrapolation_variable_g,
  ncol=1)

# save
ggsave(paste0(fig_dir, "/figureS?_prediction_extent.png"), 
       plot = prediction_extent, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 210 / 25.4, units = "in")

#-----------------Figure S5a representativeness by PCA -----------------------------------
# label pixels that are predicted

predicted_youngforest <- summary_youngforest1992_2020 %>% mutate(yf_predicted=T) %>% dplyr::select(x,y,yf_predicted)
predicted_oldforest <- summary_oldforest1992_2020 %>% mutate(of_predicted=T) %>% dplyr::select(x,y,of_predicted)
predicted_grassland <- summary_grassland1992_2020 %>% mutate(g_predicted=T) %>% dplyr::select(x,y,g_predicted)

predicted_pixels <- predicted_youngforest %>%
  full_join(predicted_oldforest, by = c("x", "y")) %>%
  full_join(predicted_grassland, by = c("x", "y"))

# predictor space of all predicted 
input_first <- input_youngforest1990_first %>%
  left_join(proportion_decade_df, by = c("x", "y")) %>%
  left_join(predicted_pixels, by = c("x", "y")) %>%
  mutate(across(c(yf_predicted, of_predicted, g_predicted),
                ~replace_na(as.logical(.x), FALSE)))

input_last <- input_youngforest2010_last %>%
  left_join(proportion_decade_df, by = c("x", "y")) %>%
  left_join(predicted_pixels, by = c("x", "y")) %>%
  mutate(across(c(yf_predicted, of_predicted, g_predicted),
                ~replace_na(as.logical(.x), FALSE)))

predictors <- rbind(input_first,input_last) %>%
  left_join(n_pct_df, by = c("x", "y")) %>% rename(n_pct_plo = value) %>%
  left_join(clay_df, by = c("x", "y")) %>% rename(clay_plo = value) %>%
  left_join(ph_df, by = c("x", "y")) %>% rename(ph_plo = value)

# define function that plot all pixels, all data, and time series data in 
visualize_PCA <- function(predictors, onetime_data, ts_data, 
                          forest_cond_str="young forest", 
                          extent_str="avg_youngforest1992_2020",
                          predict_col = "yf_predicted") {
  
  # rename variables for visualization
  variable_names <- c("observation_year","MAT","MAP","soil_N","soil_clay","soil_pH")
  
  # onetime data with this land cover
  data_values <- onetime_data %>%
    filter(forest_cond_gf_besnard60==forest_cond_str) %>%
    dplyr::select(observation_year,MAT_cru,MAP_cru,n_pct_plo,clay_plo,ph_plo)
  colnames(data_values) <- variable_names
  
  # time series data 
  ts_data_values <- ts_data %>%
    filter(forest_cond_gf_besnard60==forest_cond_str) %>%
    dplyr::select(observation_year,MAT_cru,MAP_cru,n_pct_plo,clay_plo,ph_plo)
  colnames(ts_data_values) <- variable_names
  
  # predictors pixels in this land cover
  # all pixels to predict SOC change of young forest 1992 2020
  predictors <- predictors %>%
    dplyr::filter(!!sym(extent_str)>0) %>%
    dplyr::filter(!!sym(predict_col))
  
  predictor_values <- predictors %>%
    dplyr::select(observation_year,MAT_cru,MAP_cru,n_pct_plo,clay_plo,ph_plo)
  colnames(predictor_values) <- variable_names
  
  # PCA for the predictors (pixels)
  pca_result <- prcomp(predictor_values, scale. = TRUE)
  
  # add PCA score to predictor_values
  predictor_pca_scores <- cbind(predictors, pca_result$x) %>%
    mutate(type="Predicted pixels") %>%
    mutate(type = factor(type, levels = c("Predicted pixels", "Single-time data", "Time series data")))
  # rename predicted column
  # colnames(predictor_pca_scores)[which(colnames(predictor_pca_scores)==predict_col)] <- "predict"
  
  # project input data to this space
  # scale data (only run once in this order)
  data_scaled <- sweep(data_values, 2, pca_result$center, "-")
  data_scaled <- sweep(data_scaled, 2, pca_result$scale, "/")
  # project data to PCA space
  data_pca_scores <- as.matrix(data_scaled) %*% pca_result$rotation
  data_pca_scores <- as.data.frame(data_pca_scores) %>% 
    mutate(type="Single-time data") %>%
    mutate(type = factor(type, levels = c("Predicted pixels", "Single-time data", "Time series data")))
  
  # project time series data to this space
  # scale data (only run once in this order)
  ts_data_scaled <- sweep(ts_data_values, 2, pca_result$center, "-")
  ts_data_scaled <- sweep(ts_data_scaled, 2, pca_result$scale, "/")
  # project data to PCA space
  ts_data_pca_scores <- as.matrix(ts_data_scaled) %*% pca_result$rotation
  ts_data_pca_scores <- as.data.frame(ts_data_pca_scores) %>% 
    mutate(type="Time series data") %>%
    mutate(type = factor(type, levels = c("Predicted pixels", "Single-time data", "Time series data")))
  
  # extract directions of variables on PC1 and PC2 space
  arrow_scale <- 5
  variable_directions <- as.data.frame(pca_result$rotation[,1:2]) %>%
    tibble::rownames_to_column(var = "variable") %>%
    mutate(PC1 = PC1 * arrow_scale,
           PC2 = PC2 * arrow_scale)
  
  
  p <- ggplot() + 
    geom_point(data=predictor_pca_scores, aes(x=PC1, y=PC2, color=type), shape=1,alpha=1) +
    geom_point(data=data_pca_scores, aes(x=PC1, y=PC2, color=type), shape=1,alpha=1) + 
    geom_point(data=ts_data_pca_scores, aes(x=PC1, y=PC2, color=type), shape=1,alpha=1) + 
    scale_color_manual(values=c("Predicted pixels"="gray", "Single-time data"="pink", "Time series data"="skyblue"),
                       name=NULL) +
    geom_segment(data = variable_directions,
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color = "black") +
    geom_text(data = variable_directions,
              aes(x = PC1, y = PC2, label = variable),
              color = "black", vjust = 1.5, size = 3)
  
  
  return(p)
}

p_pca_yf <- visualize_PCA(predictors, df_onetime, df_std_ts,"young forest","avg_youngforest1992_2020", "yf_predicted") + 
  ggtitle("Young forest") + theme(legend.position = "none")
p_pca_of <- visualize_PCA(predictors, df_onetime, df_std_ts,"old forest","avg_oldforest1992_2020", "of_predicted") + 
  ggtitle("Old forest") + theme(legend.position = "none")
p_pca_g <- visualize_PCA(predictors, df_onetime, df_std_ts,"grassland","grassland1992_2020", "g_predicted") + 
  ggtitle("Grassland") + theme(legend.position = "none")
pca_legend <- get_legend(
  visualize_PCA(predictors, df_onetime, df_std_ts, "young forest",
                "avg_youngforest1992_2020", "yf_predicted") + theme(legend.position = "right"))

pca_plot <- grid.arrange(p_pca_yf,p_pca_of,p_pca_g,pca_legend, ncol=2)

ggsave(paste0(fig_dir, "/figureS?_PCA.png"), 
       plot = pca_plot, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 140 / 25.4, units = "in")

#-----------------Figure S5b distribution of data input variables vs. predicted pixels (cap2_exclude) ---------
# filter one row for each plot
df_unique <- df_std %>%
  distinct(siteID, plotID, n_pct_plo, clay_plo, ph_plo, mean_mat_site, mean_map_site)

# function to plot density of data vs. prediction layer
plot_density <- function(var_name="mean_mat_site", layer_df=mat_1992_df, title_str="MAT") {
  p <- ggplot() + 
    #geom_density(data=layer_df, aes(x=value, ..scaled..), fill="lightblue", alpha = 0.5) + 
    geom_histogram(data=layer_df, aes(x=value, y=..density..), fill="darkgray", color="white") +
    geom_density(data=df_unique, aes(x=!!sym(var_name)), fill="pink", alpha = 0.5) + 
    labs(x=NULL, y=NULL, title=title_str) + 
    theme_classic()
  return(p) 
}

# filter predictor dfs to have only predicted pixels
# get x, y of all predicted pixels
predicted_xy <- rbind(
  summary_youngforest1992_2020, summary_oldforest1992_2020, summary_grassland1992_2020
) %>%
  dplyr::select(x,y) %>%
  distinct()

# # for all pixels that should be predicted
# predicted_xy <- proportion_decade_df %>%
#   filter(avg_youngforest1992_2020>0 | avg_oldforest1992_2020>0 | grassland1992_2020>0) %>%
#   dplyr::select(x,y) %>%
#   distinct()


# filter predictor dfs
mat_1992_pred <- merge(predicted_xy, mat_1992_df, by=c("x","y"), all.x=T)
map_1992_pred <- merge(predicted_xy, map_1992_df, by=c("x","y"), all.x=T)
n_pct_pred <- merge(predicted_xy, n_pct_df, by=c("x","y"), all.x=T)
clay_pred <- merge(predicted_xy, clay_df, by=c("x","y"), all.x=T)
ph_pred <- merge(predicted_xy, ph_df, by=c("x","y"), all.x=T)


# density plot input mean_mat_site and pixels mat_1992
p_mat <- plot_density("mean_mat_site", mat_1992_pred, paste0("MAT (","\u00B0","C)"))
p_map <- plot_density("mean_map_site", map_1992_pred, "MAP (mm)")
p_n_pct <- plot_density("n_pct_plo", n_pct_pred, "Soil N%")
p_clay <- plot_density("clay_plo", clay_pred, "Soil clay %")
p_ph <- plot_density("ph_plo", ph_pred, "Soil pH")

legend <- legendGrob(
  labels = c("Prediction", "Input"),
  pch = 15,  # Square symbols
  gp = gpar(col = c("darkgray", "pink"), fill = c("darkgray", "pink"))
)

p_distribution <- grid.arrange(p_mat,p_map,p_n_pct,p_clay,p_ph,legend, ncol=3)

# ggsave("./figureS11_predictor_distribution.png", plot = p_distribution, 
#        bg = "white", dpi = 600, width = 183 / 25.4, height = 180 / 25.4, units = "in")
ggsave(paste0(fig_dir, "/figureS?_predictor_data_distribution.png"), plot = p_distribution, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 120 / 25.4, units = "in")



#-----------------Figure S6. Est. map and SE 1992-2000 --------------------------
# generate panels
avg_youngforest <- plot_single_delta(summary_youngforest1990[summary_youngforest1990$proportion>0,], "annual_mean")+ggtitle("young forest") # &abs(summary_youngforest1990$annual_mean)<3
sd_youngforest <- plot_single_value(summary_youngforest1990[summary_youngforest1990$proportion>0,], "annual_sd")+ggtitle(" ")
total_youngforest <- plot_single_delta(summary_youngforest1990[summary_youngforest1990$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")
avg_oldforest <- plot_single_delta(summary_oldforest1990[summary_oldforest1990$proportion>0,], "annual_mean")+ggtitle("old forest")
sd_oldforest <- plot_single_value(summary_oldforest1990[summary_oldforest1990$proportion>0,], "annual_sd")+ggtitle(" ")
total_oldforest <- plot_single_delta(summary_oldforest1990[summary_oldforest1990$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")
avg_grassland <- plot_single_delta(summary_grassland1990[summary_grassland1990$proportion>0,], "annual_mean")+ggtitle("grassland")
sd_grassland <-  plot_single_value(summary_grassland1990[summary_grassland1990$proportion>0,], "annual_sd")+ggtitle(" ")
total_grassland <- plot_single_delta(summary_grassland1990[summary_grassland1990$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")

gavg_yf <- ggplotGrob(avg_youngforest+ theme(legend.position = "none"))
gsd_yf  <- ggplotGrob(sd_youngforest+ theme(legend.position = "none"))
gt_yf <- ggplotGrob(total_youngforest+ theme(legend.position = "none"))
gavg_of <- ggplotGrob(avg_oldforest+ theme(legend.position = "none"))
gsd_of  <- ggplotGrob(sd_oldforest + theme(legend.position = "none"))
gt_of <- ggplotGrob(total_oldforest+ theme(legend.position = "none"))
gavg_g <- ggplotGrob(avg_grassland+ theme(legend.position = "none"))
gsd_g  <- ggplotGrob(sd_grassland+ theme(legend.position = "none"))
gt_g <- ggplotGrob(total_grassland+ theme(legend.position = "none"))

legend_avg_yf <- get_legend(ggplotGrob(avg_youngforest))
legend_sd_yf  <- get_legend(ggplotGrob(sd_youngforest))
legend_t_yf <- get_legend(ggplotGrob(total_youngforest))
legend_avg_of <- get_legend(ggplotGrob(avg_oldforest))
legend_sd_of  <- get_legend(ggplotGrob(sd_oldforest))
legend_t_of <- get_legend(ggplotGrob(total_oldforest))
legend_avg_g <- get_legend(ggplotGrob(avg_grassland))
legend_sd_g  <- get_legend(ggplotGrob(sd_grassland))
legend_t_g <- get_legend(ggplotGrob(total_grassland))

# plot 
p_1992_2000_cap <-
  plot_grid(
    plot_grid(ggpubr::text_grob("Average SOC Change\n(MgC/ha yr-1)"),
              gavg_yf,gavg_of,gavg_g, ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(), 
              legend_avg_yf,legend_avg_of, legend_avg_g,ncol =1, heights=c(0.1,1,1,1), align="hv"),
    plot_grid(ggpubr::text_grob("Standard Error\n(MgC/ha yr-1)"),
              gsd_yf,gsd_of,gsd_g,ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(), 
              legend_sd_yf,legend_sd_of,legend_sd_g, heights=c(0.1,1,1,1), ncol =1, align="hv"),
    plot_grid(ggpubr::text_grob("Total SOC change\nper 0.5° pixel (Gg/yr)"),
              gt_yf,gt_of,gt_g,ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(),
              legend_t_yf,legend_t_of,legend_t_g, ncol =1, heights=c(0.1,1,1,1), align="hv"),
    ncol=6, align = "hv",rel_widths = c(7,2.5,7,2.5,7,2.5)
  )

ggsave("./figures/figureS6_1992_2000_SOCchange.png", 
       plot = p_1992_2000_cap, bg = "white", dpi = 600, width = 200 / 25.4, height = 180 / 25.4, units = "in")


#-----------------Figure S7. Est. map and SE 2000-2010 --------------------------

# generate panels
avg_youngforest <- plot_single_delta(summary_youngforest2000[summary_youngforest2000$proportion>0,], "annual_mean")+ggtitle("young forest")
sd_youngforest <- plot_single_value(summary_youngforest2000[summary_youngforest2000$proportion>0,], "annual_sd")+ggtitle(" ")
total_youngforest <- plot_single_delta(summary_youngforest2000[summary_youngforest2000$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")
avg_oldforest <- plot_single_delta(summary_oldforest2000[summary_oldforest2000$proportion>0,], "annual_mean")+ggtitle("old forest")
sd_oldforest <- plot_single_value(summary_oldforest2000[summary_oldforest2000$proportion>0,], "annual_sd")+ggtitle(" ")
total_oldforest <- plot_single_delta(summary_oldforest2000[summary_oldforest2000$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")
avg_grassland <- plot_single_delta(summary_grassland2000[summary_grassland2000$proportion>0,], "annual_mean")+ggtitle("grassland")
sd_grassland <-  plot_single_value(summary_grassland2000[summary_grassland2000$proportion>0,], "annual_sd")+ggtitle(" ")
total_grassland <- plot_single_delta(summary_grassland2000[summary_grassland2000$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")

gavg_yf <- ggplotGrob(avg_youngforest+ theme(legend.position = "none"))
gsd_yf  <- ggplotGrob(sd_youngforest+ theme(legend.position = "none"))
gt_yf <- ggplotGrob(total_youngforest+ theme(legend.position = "none"))
gavg_of <- ggplotGrob(avg_oldforest+ theme(legend.position = "none"))
gsd_of  <- ggplotGrob(sd_oldforest + theme(legend.position = "none"))
gt_of <- ggplotGrob(total_oldforest+ theme(legend.position = "none"))
gavg_g <- ggplotGrob(avg_grassland+ theme(legend.position = "none"))
gsd_g  <- ggplotGrob(sd_grassland+ theme(legend.position = "none"))
gt_g <- ggplotGrob(total_grassland+ theme(legend.position = "none"))

legend_avg_yf <- get_legend(ggplotGrob(avg_youngforest))
legend_sd_yf  <- get_legend(ggplotGrob(sd_youngforest))
legend_t_yf <- get_legend(ggplotGrob(total_youngforest))
legend_avg_of <- get_legend(ggplotGrob(avg_oldforest))
legend_sd_of  <- get_legend(ggplotGrob(sd_oldforest))
legend_t_of <- get_legend(ggplotGrob(total_oldforest))
legend_avg_g <- get_legend(ggplotGrob(avg_grassland))
legend_sd_g  <- get_legend(ggplotGrob(sd_grassland))
legend_t_g <- get_legend(ggplotGrob(total_grassland))

# plot 
p_2000_2010_cap <- 
  plot_grid(
    plot_grid(ggpubr::text_grob("Average SOC Change\n(MgC/ha yr-1)"),
              gavg_yf,gavg_of,gavg_g, ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(), 
              legend_avg_yf,legend_avg_of, legend_avg_g,ncol =1, heights=c(0.1,1,1,1), align="hv"),
    plot_grid(ggpubr::text_grob("Standard Error\n(MgC/ha yr-1)"),
              gsd_yf,gsd_of,gsd_g,ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(), 
              legend_sd_yf,legend_sd_of,legend_sd_g, heights=c(0.1,1,1,1), ncol =1, align="hv"),
    plot_grid(ggpubr::text_grob("Total SOC change\nper 0.5° pixel (Gg/yr)"),
              gt_yf,gt_of,gt_g,ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(),
              legend_t_yf,legend_t_of,legend_t_g, ncol =1, heights=c(0.1,1,1,1), align="hv"),
    ncol=6, align = "hv",rel_widths = c(7,2.5,7,2.5,7,2.5)
  )

ggsave("./figures/figureS7_2000_2010_SOCchange.png", plot = p_2000_2010_cap, bg = "white", dpi = 600, width = 200 / 25.4, height = 180 / 25.4, units = "in")

#-----------------Figure S8. Est. map and SE 2010-2020 --------------------------

# generate panels
avg_youngforest <- plot_single_delta(summary_youngforest2010[summary_youngforest2010$proportion>0,], "annual_mean")+ggtitle("young forest")
sd_youngforest <- plot_single_value(summary_youngforest2010[summary_youngforest2010$proportion>0,], "annual_sd")+ggtitle(" ")
total_youngforest <- plot_single_delta(summary_youngforest2010[summary_youngforest2010$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")
avg_oldforest <- plot_single_delta(summary_oldforest2010[summary_oldforest2010$proportion>0,], "annual_mean")+ggtitle("old forest")
sd_oldforest <- plot_single_value(summary_oldforest2010[summary_oldforest2010$proportion>0,], "annual_sd")+ggtitle(" ")
total_oldforest <- plot_single_delta(summary_oldforest2010[summary_oldforest2010$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")
avg_grassland <- plot_single_delta(summary_grassland2010[summary_grassland2010$proportion>0,], "annual_mean")+ggtitle("grassland")
sd_grassland <-  plot_single_value(summary_grassland2010[summary_grassland2010$proportion>0,], "annual_sd")+ggtitle(" ")
total_grassland <- plot_single_delta(summary_grassland2010[summary_grassland2010$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")


gavg_yf <- ggplotGrob(avg_youngforest+ theme(legend.position = "none"))
gsd_yf  <- ggplotGrob(sd_youngforest+ theme(legend.position = "none"))
gt_yf <- ggplotGrob(total_youngforest+ theme(legend.position = "none"))
gavg_of <- ggplotGrob(avg_oldforest+ theme(legend.position = "none"))
gsd_of  <- ggplotGrob(sd_oldforest + theme(legend.position = "none"))
gt_of <- ggplotGrob(total_oldforest+ theme(legend.position = "none"))
gavg_g <- ggplotGrob(avg_grassland+ theme(legend.position = "none"))
gsd_g  <- ggplotGrob(sd_grassland+ theme(legend.position = "none"))
gt_g <- ggplotGrob(total_grassland+ theme(legend.position = "none"))

legend_avg_yf <- get_legend(ggplotGrob(avg_youngforest))
legend_sd_yf  <- get_legend(ggplotGrob(sd_youngforest))
legend_t_yf <- get_legend(ggplotGrob(total_youngforest))
legend_avg_of <- get_legend(ggplotGrob(avg_oldforest))
legend_sd_of  <- get_legend(ggplotGrob(sd_oldforest))
legend_t_of <- get_legend(ggplotGrob(total_oldforest))
legend_avg_g <- get_legend(ggplotGrob(avg_grassland))
legend_sd_g  <- get_legend(ggplotGrob(sd_grassland))
legend_t_g <- get_legend(ggplotGrob(total_grassland))


# plot 
p_2010_2020_cap <-
  plot_grid(
    plot_grid(ggpubr::text_grob("Average SOC Change\n(MgC/ha yr-1)"),
              gavg_yf,gavg_of,gavg_g, ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(), 
              legend_avg_yf,legend_avg_of, legend_avg_g,ncol =1, heights=c(0.1,1,1,1), align="hv"),
    plot_grid(ggpubr::text_grob("Standard Error\n(MgC/ha yr-1)"),
              gsd_yf,gsd_of,gsd_g,ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(), 
              legend_sd_yf,legend_sd_of,legend_sd_g, heights=c(0.1,1,1,1), ncol =1, align="hv"),
    plot_grid(ggpubr::text_grob("Total SOC change\nper 0.5° pixel (Gg/yr)"),
              gt_yf,gt_of,gt_g,ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(),
              legend_t_yf,legend_t_of,legend_t_g, ncol =1, heights=c(0.1,1,1,1), align="hv"),
    ncol=6, align = "hv",rel_widths = c(7,2.5,7,2.5,7,2.5)
  )

ggsave("./figures/figureS8_2010_2020_SOCchange.png", plot = p_2010_2020_cap, bg = "white", dpi = 600, width = 200 / 25.4, height = 180 / 25.4, units = "in")

#-----------------Figure S9. Est. map and SE 1992-2020 (main approach)--------------------------

# generate panels
avg_youngforest <- plot_single_delta(summary_youngforest1992_2020[summary_youngforest1992_2020$proportion>0,], "annual_mean")+ggtitle("young forest")
sd_youngforest <- plot_single_value(summary_youngforest1992_2020[summary_youngforest1992_2020$proportion>0,], "annual_sd")+ggtitle(" ")
total_youngforest <- plot_single_delta(summary_youngforest1992_2020[summary_youngforest1992_2020$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")
avg_oldforest <- plot_single_delta(summary_oldforest1992_2020[summary_oldforest1992_2020$proportion>0,], "annual_mean")+ggtitle("old forest")
sd_oldforest <- plot_single_value(summary_oldforest1992_2020[summary_oldforest1992_2020$proportion>0,], "annual_sd")+ggtitle(" ")
total_oldforest <- plot_single_delta(summary_oldforest1992_2020[summary_oldforest1992_2020$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")
avg_grassland <- plot_single_delta(summary_grassland1992_2020[summary_grassland1992_2020$proportion>0,], "annual_mean")+ggtitle("grassland")
sd_grassland <-  plot_single_value(summary_grassland1992_2020[summary_grassland1992_2020$proportion>0,], "annual_sd")+ggtitle(" ")
total_grassland <- plot_single_delta(summary_grassland1992_2020[summary_grassland1992_2020$proportion>0,], "annual_mean_GgperPixel")+ggtitle(" ")

gavg_yf <- ggplotGrob(avg_youngforest+ theme(legend.position = "none"))
gsd_yf  <- ggplotGrob(sd_youngforest+ theme(legend.position = "none"))
gt_yf <- ggplotGrob(total_youngforest+ theme(legend.position = "none"))
gavg_of <- ggplotGrob(avg_oldforest+ theme(legend.position = "none"))
gsd_of  <- ggplotGrob(sd_oldforest + theme(legend.position = "none"))
gt_of <- ggplotGrob(total_oldforest+ theme(legend.position = "none"))
gavg_g <- ggplotGrob(avg_grassland+ theme(legend.position = "none"))
gsd_g  <- ggplotGrob(sd_grassland+ theme(legend.position = "none"))
gt_g <- ggplotGrob(total_grassland+ theme(legend.position = "none"))

legend_avg_yf <- get_legend(ggplotGrob(avg_youngforest))
legend_sd_yf  <- get_legend(ggplotGrob(sd_youngforest))
legend_t_yf <- get_legend(ggplotGrob(total_youngforest))
legend_avg_of <- get_legend(ggplotGrob(avg_oldforest))
legend_sd_of  <- get_legend(ggplotGrob(sd_oldforest))
legend_t_of <- get_legend(ggplotGrob(total_oldforest))
legend_avg_g <- get_legend(ggplotGrob(avg_grassland))
legend_sd_g  <- get_legend(ggplotGrob(sd_grassland))
legend_t_g <- get_legend(ggplotGrob(total_grassland))

# plot 
p_1992_2020_cap <- 
  plot_grid(
    plot_grid(ggpubr::text_grob("Average SOC Change\n(MgC/ha yr-1)"),
              gavg_yf,gavg_of,gavg_g, ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(), 
              legend_avg_yf,legend_avg_of, legend_avg_g,ncol =1, heights=c(0.1,1,1,1), align="hv"),
    plot_grid(ggpubr::text_grob("Standard Error\n(MgC/ha yr-1)"),
              gsd_yf,gsd_of,gsd_g,ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(), 
              legend_sd_yf,legend_sd_of,legend_sd_g, heights=c(0.1,1,1,1), ncol =1, align="hv"),
    plot_grid(ggpubr::text_grob("Total SOC change\nper 0.5° pixel (Gg/yr)"),
              gt_yf,gt_of,gt_g,ncol = 1, heights=c(0.1,1,1,1), align = "hv"), 
    plot_grid(grid::nullGrob(),
              legend_t_yf,legend_t_of,legend_t_g, ncol =1, heights=c(0.1,1,1,1), align="hv"),
    ncol=6, align = "hv",rel_widths = c(7,2.5,7,2.5,7,2.5)
  )

ggsave("./figures/figureS9_1992_2020_SOCchange.png", plot = p_1992_2020_cap, bg = "white", dpi = 600, width = 200 / 25.4, height = 180 / 25.4, units = "in")

#-----------------Figure S10. Significant pixels -------------------------------

# filter delta dataframes for significant pixels
#1990-2000
summary_youngforest1990_sig <- summary_youngforest1990 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_oldforest1990_sig <- summary_oldforest1990 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_grassland1990_sig <- summary_grassland1990 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

# 2000-2010
summary_youngforest2000_sig <- summary_youngforest2000 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_oldforest2000_sig <- summary_oldforest2000 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_grassland2000_sig <- summary_grassland2000 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

# 2010-2020
summary_youngforest2010_sig <- summary_youngforest2010 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_oldforest2010_sig <- summary_oldforest2010 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_grassland2010_sig <- summary_grassland2010 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

# 1992-2020
summary_youngforest1992_2020_sig <- summary_youngforest1992_2020 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_oldforest1992_2020_sig <- summary_oldforest1992_2020 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_grassland1992_2020_sig <- summary_grassland1992_2020 %>%
  dplyr::filter(proportion>0) %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

# plot
yf_1990_sig <- ggplotGrob(plot_single_delta(summary_youngforest1990_sig, "annual_mean")+ggtitle("1990-2000"))
yf_2000_sig <- ggplotGrob(plot_single_delta(summary_youngforest2000_sig, "annual_mean")+ggtitle("2000-2010"))
yf_2010_sig <- ggplotGrob(plot_single_delta(summary_youngforest2010_sig, "annual_mean")+ggtitle("2010-2020"))
yf_total_sig <- ggplotGrob(plot_single_delta(summary_youngforest1992_2020_sig, "annual_mean")+ggtitle("1992-2020"))

of_1990_sig <- ggplotGrob(plot_single_delta(summary_oldforest1990_sig, "annual_mean")+ggtitle("1990-2000"))
of_2000_sig <- ggplotGrob(plot_single_delta(summary_oldforest2000_sig, "annual_mean")+ggtitle("2000-2010"))
of_2010_sig <- ggplotGrob(plot_single_delta(summary_oldforest2010_sig, "annual_mean")+ggtitle("2010-2020"))
of_total_sig <- ggplotGrob(plot_single_delta(summary_oldforest1992_2020_sig, "annual_mean")+ggtitle("1992-2020"))

g_1990_sig <- ggplotGrob(plot_single_delta(summary_grassland1990_sig, "annual_mean")+ggtitle("1990-2000"))
g_2000_sig <- ggplotGrob(plot_single_delta(summary_grassland2000_sig, "annual_mean")+ggtitle("2000-2010"))
g_2010_sig <- ggplotGrob(plot_single_delta(summary_grassland2010_sig, "annual_mean")+ggtitle("2010-2020"))
g_total_sig <- ggplotGrob(plot_single_delta(summary_grassland1992_2020_sig, "annual_mean")+ggtitle("1992-2020"))

# plot
p_sig_cap <- 
  plot_grid(
    plot_grid(ggpubr::text_grob("Young forest (MgC/ha yr-1)"),
              yf_1990_sig,yf_2000_sig,yf_2010_sig, yf_total_sig, ncol = 1, heights=c(0.1,1,1,1,1), align = "hv"), 
    plot_grid(ggpubr::text_grob("Old forest (MgC/ha yr-1)"),
              of_1990_sig,of_2000_sig,of_2010_sig, of_total_sig, ncol = 1, heights=c(0.1,1,1,1,1), align = "hv"), 
    plot_grid(ggpubr::text_grob("Grassland (MgC/ha yr-1)"),
              g_1990_sig,g_2000_sig,g_2010_sig, g_total_sig, ncol = 1, heights=c(0.1,1,1,1,1), align = "hv"), 
    ncol=3, align = "hv"#,rel_widths = c(7,2,7,2,7,2)
  )

ggsave("./figures/figureS10_sig_SOCchange.png", plot = p_sig_cap, bg = "white", dpi = 600, width = 183 / 25.4, height = 180 / 25.4, units = "in")

#-----------------Figure S11 predictor layer maps ------------
# omit NA values
clay_df <- clay_df %>%  na.omit()
n_pct_df <- n_pct_df %>% na.omit()
ph_df <- ph_df %>% na.omit()

mat_1992_df_omit <- mat_1992_df %>% na.omit()
map_1992_df_omit <- map_1992_df %>% na.omit()

p_input_layers <-
  grid.arrange(plot_single_value_viridis(mat_1992_df_omit, var_name="value") + ggtitle("MAT, year 1992 (°C)"), 
               plot_single_value_viridis(map_1992_df_omit, var_name="value") + ggtitle("MAP, year 1992 (mm)"),
               plot_single_value_viridis(n_pct_df, var_name="value") + ggtitle("soil Nitrogen (%)"),
               plot_single_value_viridis(clay_df, var_name="value") + ggtitle("soil clay (%)"),
               plot_single_value_viridis(ph_df, var_name="value") + ggtitle("soil pH"),
               ncol=2)

#save
ggsave("./figures/figureS11_predictor_layers.png", plot = p_input_layers, bg = "white", dpi = 600, width = 183 / 25.4, height = 180 / 25.4, units = "in")

#-----------------Figure S12 land use proportions ----------------------
# make into one map per land use (cuz every decade is the same extent now)
# use truncated or not tunrcate version?
# use input_lc_pred, or input_lc_all # maybe do _pred for now see what people say
# or do one _pred one the difference between _all and _pred (to show the uncaptured pixels?)
# that seems redundant with the truncation extent layer

# plot 
p_fraction <- 
  grid.arrange(
    ggplotGrob(plot_single_value_viridis(input_youngforest_pred, "proportion")+ggtitle("Young forest")),
    ggplotGrob(plot_single_value_viridis(input_oldforest_pred, "proportion")+ggtitle("Old forest")),
    ggplotGrob(plot_single_value_viridis(input_grassland_pred, "proportion")+ggtitle("Grassland")),
    ncol=1, nrow=3, heights=c(1,1,1)#, widths=c(1,1,1)
  )

#save
ggsave("./figures/figureS12_fraction.png", plot = p_fraction, bg = "white", dpi = 600, width = 183 / 25.4, height = 180 / 25.4, units = "in")

#-----------------Figure S13 GCB terms illustration ---------------------------
# this figure is manually made in powerpoint

#-----------------Figure S14 SOC temporal trends fit by only time series ------
# this is plotted by loading "brms1_timeseries_37k12chain.rda" into script 
# Figure1.R