# supplementary figures
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
library(cowplot)
library(sf)
library(grid)
library(gridExtra)

# working directory should be set by default to the folder of entire repo (R project)

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
  
  # define legend
  if (shrubland) {
    labels <- c(paste0("≤", scales::number_format(accuracy = 1)(min_val)), 
                paste0("≥", scales::number_format(accuracy = 1)(max_val)))
  } else {
    labels <- scales::number_format(accuracy = 1, scale = 1)(c(min_val, max_val))
  }
  
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
# dataset (object name: df_std)
load("./data/SOC_time_series_for_analysis.rda")
# pixel level results
load("./results/upscaled_pixel_summary.RData") # update this for r2>0.8
# load input predictor data (capped to within input MAT MAP range)
load("./results/prediction_inputs.rda")
# load land cover extent
load("./data/proportion_decade_df.rda")

# load model
load("./results/gamm_brms12.rda") # change this
mod <- gamm_brms12

# input layers (code to generate these files is in 00_download_prepare_spatial_data.R)
# mat_1992
mat_1992 <- raster("./data/mat_1992.tif")
# map_1992
map_1992 <- raster("./data/map_1992.tif")
# n_pct
n_pct <- raster("./data/n_sg_halfdegree.tif")
# clay
clay <- raster("./data/clay_sg_halfdegree.tif")
# ph
ph <- raster("./data/ph_sg_halfdegree.tif")

# convert tifs to df
mat_1992_df <- raster2df(mat_1992)
map_1992_df <- raster2df(map_1992)
n_pct_df <- raster2df(n_pct) 
clay_df <- raster2df(clay)
ph_df <- raster2df(ph)

# # load world administrative boundaries
# # https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/?flg=en-us
countries_sf <- read_sf("./data/world-administrative-boundaries/world-administrative-boundaries.shp")
countries_sf <- st_make_valid(countries_sf)
countries_sf <- st_transform(countries_sf, 4326)

# define min and max MAT and MAP conditions in input data
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

#---------- define palettes ----------------------------------------------------
type_palette <- c(
  "young forest" = "#c8445b",
  "old forest" = "#4d5ca1",
  "grassland" = "#66aa8b"
)

truncation_palette <- c(
  "None" = "gray",
  "MAT" = "darkgreen",
  "MAP" = "skyblue",
  "Both" = "darkred"
)

theme_set(theme_minimal(base_family = "Helvetica", base_size = 14))

#-----------------Figure S1 data distribution-----------------------------------
# (a) data on world map
p_data_map <- 
  ggplot(countries_sf) +
  geom_sf(fill = "lightgray", col="gray", alpha=0.5) +
  geom_point(data=df_std, aes(x=long, y=lat, color = forest_cond_gf_besnard60), alpha=1, size=0.5) +
  scale_color_manual(values = type_palette) +
  labs(tag="a", x=NULL, y=NULL) +
  theme_bw() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        legend.position = "bottom") + 
  guides(color = guide_legend(override.aes = list(size = 3))) 

p_data_map

# (b) number of sites vs. year
# time series availability over years
nsite_yf <- count_nsite(df_std, "young forest")
nsite_of <- count_nsite(df_std, "old forest")
nsite_g <- count_nsite(df_std, "grassland")

# plot
p_nsite_year <- ggplot(nsite_yf) + 
  geom_line(data=nsite_yf, aes(x=year,y=n_site,color=forest_cond_gf_besnard60)) +
  geom_line(data=nsite_of, aes(x=year,y=n_site,color=forest_cond_gf_besnard60)) +
  geom_line(data=nsite_g, aes(x=year,y=n_site,color=forest_cond_gf_besnard60)) +
  scale_color_manual(values=type_palette) +
  labs(x="year", y="Number of Sites", tag="b") + # , title=type_str
  theme_classic() +
  theme(legend.position = "none")

p_nsite_year

# assemble
fig_S1_data_distribution <- grid.arrange(p_data_map, p_nsite_year, 
                                         ncol=1, heights=c(2,1))
fig_S1_data_distribution

# save
ggsave("figureS1_data_distribution.png", 
       plot = fig_S1_data_distribution, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 160 / 25.4, units = "in")


#-----------------Figure S2 Whittaker plot with points colored by rates --------
library(plotbiomes)

df_change <- df_std %>% group_by(plotID) %>%
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

# plot 
p_whittaker_rates <- whittaker_base_plot() + 
  #facet_wrap(~forest_cond_gf_besnard60) +
  geom_point(data=df_change, aes(mean_mat_site,mean_map_site/10, color=SOC_rate_sqrt,
                                 shape=forest_cond_gf_besnard60), 
             size=0.7, alpha=1) + # alpha=0.5
  scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint =0,
                        breaks=c(min_rate,  0, max_rate),
                        labels = labels)  + 
  labs(color = "SOC Rate Change (Mg/ha)",shape = "Land Cover") + 
  guides(shape = guide_legend(override.aes = list(size = 3))) +
  theme(legend.margin = margin(t = -2, unit = "pt"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))

p_whittaker_rates 

# save
ggsave("./figureS2_whittaker_rate.png", plot = p_whittaker_rates, bg = "white", dpi = 600, width = 183 / 25.4, height = 150 / 25.4, units = "in")

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
               aes(x=observation_year, y=MAT_cru), size=0.1,alpha=0.2,color="white") + 
    xlim(yr_span) + labs(fill="SOC (Mg/ha)", x=NULL, y="MAT (°C)") +
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
               aes(x=observation_year, y=MAP_cru), size=0.1,alpha=0.2, color="white") + 
    xlim(yr_span) + labs(fill= "SOC (Mg/ha)", x=NULL, y="MAP (mm)") +
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
ggsave("./figureS3_heatmap.png", 
       plot = p_heatmap, bg = "white", dpi = 600, width = 183 / 25.4, height = 183 / 25.4, units = "in")


#-----------------Figure S4. Est. map and SE 1992-2000 (main approach)--------------------------
# generate panels
avg_youngforest <- plot_single_delta(summary_youngforest1990[summary_youngforest1990$proportion>0,], "annual_median")+ggtitle("young forest") # &abs(summary_youngforest1990$annual_mean)<3
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

ggsave("./figureS4_1992_2000_SOCchange.png", 
       plot = p_1992_2000_cap, bg = "white", dpi = 600, width = 200 / 25.4, height = 180 / 25.4, units = "in")


#-----------------Figure S5. Est. map and SE 2000-2010 (main approach)--------------------------

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

ggsave("./figureS5_2000_2010_SOCchange.png", plot = p_2000_2010_cap, bg = "white", dpi = 600, width = 200 / 25.4, height = 180 / 25.4, units = "in")


#-----------------Figure S6. Est. map and SE 2010-2020 (main approach)--------------------------

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

ggsave("./figureS6_2010_2020_SOCchange.png", plot = p_2010_2020_cap, bg = "white", dpi = 600, width = 200 / 25.4, height = 180 / 25.4, units = "in")

#-----------------Figure S7. Est. map and SE 1992-2020 (main approach)--------------------------

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

ggsave("./figureS7_1992_2020_SOCchange.png", plot = p_1992_2020_cap, bg = "white", dpi = 600, width = 200 / 25.4, height = 180 / 25.4, units = "in")


#-----------------Figure S8. Significant pixels (main approach)-------

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

ggsave("./figureS8_sig_SOCchange.png", plot = p_sig_cap, bg = "white", dpi = 600, width = 183 / 25.4, height = 180 / 25.4, units = "in")


#-----------------Figure S9 predictor layer maps (uncapped) ------------
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
ggsave("./figureS9_predictor_layers.png", plot = p_input_layers, bg = "white", dpi = 600, width = 183 / 25.4, height = 180 / 25.4, units = "in")

#-----------------Figure S10 truncation extent of MAT and MAP -------------------

# use the min max MAT MAP values from "Figure S3"
extent_yf <- merge(x=input_youngforest1990_first,y=proportion_decade_df, by=c("x","y"), all.x=T) %>%
  filter(avg_youngforest1992_2020>0) %>% # leave only pixels that have young forest
  mutate(
    truncation_MAT = ifelse(MAT_cru==maxMAT_yf|MAT_cru==minMAT_yf, T, F),
    truncation_MAP = ifelse(MAP_cru==maxMAP_yf|MAP_cru==minMAP_yf, T, F),
  ) %>%
  mutate(truncation = case_when(
    truncation_MAT & truncation_MAP ~ "Both",
    truncation_MAT & truncation_MAP==F ~ "MAT",
    truncation_MAT==F & truncation_MAP ~ "MAP",
    truncation_MAT==F & truncation_MAP==F ~ "None"
  ))

extent_of <- merge(x=input_oldforest1990_first,y=proportion_decade_df, by=c("x","y"), all.x=T) %>%
  filter(avg_oldforest1992_2020>0) %>% # leave only pixels that have old forest
  mutate(
    truncation_MAT = ifelse(MAT_cru==maxMAT_of|MAT_cru==minMAT_of, T, F),
    truncation_MAP = ifelse(MAP_cru==maxMAP_of|MAP_cru==minMAP_of, T, F),
  ) %>%
  mutate(truncation = case_when(
    truncation_MAT & truncation_MAP ~ "Both",
    truncation_MAT & truncation_MAP==F ~ "MAT",
    truncation_MAT==F & truncation_MAP ~ "MAP",
    truncation_MAT==F & truncation_MAP==F ~ "None"
  ))

extent_g <- merge(x=input_grassland1990_first,y=proportion_decade_df, by=c("x","y"), all.x=T) %>%
  filter(grassland1990>0|grassland2000>0|grassland2010>0) %>% # leave only pixels that have grassland
  mutate(
    truncation_MAT = ifelse(MAT_cru==maxMAT_g|MAT_cru==minMAT_g, T, F),
    truncation_MAP = ifelse(MAP_cru==maxMAP_g|MAP_cru==minMAP_g, T, F),
  ) %>%
  mutate(truncation = case_when(
    truncation_MAT & truncation_MAP ~ "Both",
    truncation_MAT & truncation_MAP==F ~ "MAT",
    truncation_MAT==F & truncation_MAP ~ "MAP",
    truncation_MAT==F & truncation_MAP==F ~ "None"
  ))

# define plot function
plot_extrapolation_extent <- function(input_df) {
  
  input_df <- input_df %>%
    mutate(truncation = factor(truncation,levels=c("None","MAT","MAP","Both")))
  
  plot <- ggplot(countries_sf) +
    geom_sf(fill = "gray100", col="gray", alpha=0.5) +
    geom_tile(data=input_df, aes(x=x, y=y,fill=truncation),alpha=0.5) +
    scale_fill_manual(values=truncation_palette) +
    labs(x=NULL, y=NULL, fill="fraction of predictors\nextrapolated") + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),  # Remove x-axis title
          axis.title.y = element_blank(),  # Remove y-axis title
          axis.text.x = element_blank(),    # Remove x-axis text
          axis.text.y = element_blank(),    # Remove y-axis text
          axis.ticks.x = element_blank(),   # Remove x-axis ticks
          axis.ticks.y = element_blank(),  # Remove y-axis ticks
          legend.position = "right",
          legend.title = element_blank() 
    )
  # plot.margin = margin(1, 1, 1, 1) makes margin too small
  return(plot)
}

# plot
p_extent_yf <- plot_extrapolation_extent(extent_yf) + ggtitle("Young Forest")
p_extent_of <- plot_extrapolation_extent(extent_of) + ggtitle("Old Forest")
p_extent_g <- plot_extrapolation_extent(extent_g) + ggtitle("Grassland")

# assemble
p_truncation_extent <- grid.arrange(
  p_extent_yf,p_extent_of,
  p_extent_g,
  ncol=1)

# save 
ggsave("./figureS10_truncation.png", plot = p_truncation_extent, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 240 / 25.4, units = "in")

#-----------------Figure S11 distribution of data input variables vs. all pixels ---------
# filter one row for each plot
df_unique <- df_std %>%
  distinct(siteID, plotID, n_pct_plo, clay_plo, ph_plo, mean_mat_site, mean_map_site)

# function to plot density of data vs. prediction layer
plot_density <- function(var_name="mean_mat_site", layer_df=mat_1992_df, title_str="MAT") {
  p <- ggplot() + 
    geom_density(data=layer_df, aes(x=value, ..scaled..), fill="lightblue", alpha = 0.5) + 
    geom_density(data=df_unique, aes(x=!!sym(var_name),..scaled..), fill="pink", alpha = 0.5) + 
    labs(x=NULL, y=NULL, title=title_str) + 
    theme_classic()
  return(p) 
}

# density plot input mean_mat_site and pixels mat_1992
p_mat <- plot_density("mean_mat_site", mat_1992_df, paste0("MAT (","\u00B0","C)"))
p_map <- plot_density("mean_map_site", map_1992_df, "MAP (mm)")
p_n_pct <- plot_density("n_pct_plo", n_pct_df, "Soil N%")
p_clay <- plot_density("clay_plo", clay_df, "Soil clay %")
p_ph <- plot_density("ph_plo", ph_df, "Soil pH")

legend <- legendGrob(
  labels = c("Prediction", "Input"),
  pch = 15,  # Square symbols
  gp = gpar(col = c("lightblue", "pink"), fill = c("lightblue", "pink"))
)

p_distribution <- grid.arrange(p_mat,p_map,p_n_pct,p_clay,p_ph,legend, ncol=2)

ggsave("./figureS11_predictor_distribution.png", plot = p_distribution, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 180 / 25.4, units = "in")


#-----------------Figure S12 land use proportions ----------------------
# plot
p_fraction <- 
  grid.arrange(
    #grid::nullGrob(), 
    ggpubr::text_grob("Young Forest"),ggpubr::text_grob("Old Forest"),ggpubr::text_grob("Grassland"),
    #ggpubr::text_grob("1992-2000"),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "youngforest1990")+ggtitle("1992-2000")),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "oldforest1990")+ggtitle("1992-2000")),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "grassland1990")+ggtitle("1992-2000")),
    #ggpubr::text_grob("2000-2010"),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "youngforest2000")+ggtitle("2000-2010")),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "oldforest2000")+ggtitle("2000-2010")),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "grassland2000")+ggtitle("2000-2010")),
    #ggpubr::text_grob("2010-2010"),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "youngforest2010")+ggtitle("2010-2020")),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "oldforest2010")+ggtitle("2010-2020")),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "grassland2010")+ggtitle("2010-2020")),
    #ggpubr::text_grob("1992-2020"),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "avg_youngforest1992_2020")+ggtitle("1992-2020")),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "avg_oldforest1992_2020")+ggtitle("1992-2020")),
    ggplotGrob(plot_single_value_viridis(proportion_decade_df, "grassland1992_2020")+ggtitle("1992-2020")),
    ncol=3, nrow=5, heights=c(0.1,1,1,1,1), widths=c(1,1,1)
  )

#save
ggsave("./figureS12_fraction.png", plot = p_fraction, bg = "white", dpi = 600, width = 183 / 25.4, height = 180 / 25.4, units = "in")

