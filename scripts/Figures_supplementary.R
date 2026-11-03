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
library(pracma)
library(ggridges)
library(rnaturalearth)
library(patchwork)

# working directory should be set by default to the folder of entire repo (R project)

map_dir <- "./data"
fig_dir <- "./figures"
set.seed(1)

#---------- load functions --------------------
# define min and max of MAT, MAP without outliers (within 1.5*IQR from Q1 and Q3)
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
# apply pre-defined legend range to squish long tail
plot_single_value_scico <- function(delta_df, var_name="value", legend_range="none",
                                    panel_letter=NULL, panel_title=NULL, include_legend=FALSE,
                                    legend_name=NULL) {
  # delta_df must have column x(lat), y(long), and a value column
  colnames(delta_df)[which(colnames(delta_df)==var_name)] <- "var"
  
  # make legend guide spec
  if (include_legend) {
    guide_spec <- guide_colorbar(title.position = "top", 
                                 title.hjust = 0.5,
                                 barwidth = 10, 
                                 barheight = 0.5)
  } else {guide_spec <- "none" }
  
  # set min max numbers in scale based on legend_range or plotted numbers
  if (legend_range[1]=="none") {
    min_val <- min(delta_df$var, na.rm=T)
    max_val <- max(delta_df$var, na.rm=T)
  } else {
    min_val <- legend_range[1]
    max_val <- legend_range[2]
  }
  
  # Create sf object with WGS84 coordinates
  spatial_df <- st_as_sf(delta_df, coords = c("x", "y"), crs = 4326)
  
  # transform to robinson
  spatial_df_robin <- st_transform(spatial_df, robinson_projection)
  
  # extract robinson coordinates
  tiles_df <- as.data.frame(st_coordinates(spatial_df_robin)) %>%
    bind_cols(st_drop_geometry(spatial_df_robin) %>% 
                dplyr::select(var))
  
  p <- ggplot() +
    # land shape as background
    geom_sf(data=land, fill="grey40", color=NA) +
    # tiles with color gradient
    geom_tile(data = tiles_df,
              aes(X, Y, fill = var),
              width = tile_width, height = tile_height) +
    # set scico scale
    # oob=scales::squish compresses values outside limits to the boundary colors
    scico::scale_fill_scico(palette = "vik",
                            breaks=c(min_val, max_val),
                            labels = c(paste0("<= ",round(min_val,1)), paste0(">= ", round(max_val,1))),
                            limits = c(min_val, max_val),
                            oob = scales::squish,
                            guide = guide_spec,
                            name = legend_name) +
    # Coordinate system for Robinson projection
    coord_sf(crs = robinson_projection, expand = FALSE,
             ylim = c(-6e6, 9.6e6)) +
    # panel label (left top)
    annotate("text", x = -1.6e7, y = 9.0e6, label = panel_letter, 
             hjust = 0, size = 4.5, color = "grey15", fontface = "bold") +
    # land cover label (left top, below panel letter)
    annotate("text",  x = -1.5e7, y = 9.0e6, label = panel_title,
             hjust = 0, size = 3.2, color = "grey30", fontface = "plain") +
    # Theme
    theme_void(base_size = 9) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA, linewidth = 0.3),
      plot.margin = if(include_legend) margin(6, 0, 0, 0) else margin(6, 0, 2, 0),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(size = 8.6, color = "grey30"),
      legend.text = element_text(size = 8, color = "grey30")
    )
  
  return(p)
}
# plot single value in viridis
# do not squish tail for these plots (predictor layer, land cover fraction)
plot_single_value_viridis <- function(delta_df, var_name="value",legend_range="none",
                                      panel_letter=NULL, panel_title=NULL, include_legend=FALSE, 
                                      legend_name=NULL) {
  # delta_df must have column x(lat), y(long), and a value column
  colnames(delta_df)[which(colnames(delta_df)==var_name)] <- "var"
  
  # make legend guide spec
  if (include_legend) {
    guide_spec <- guide_colorbar(title.position = "top", 
                                 title.hjust = 0.5,
                                 barwidth = 10, 
                                 barheight = 0.5)
  } else {guide_spec <- "none" }
  
  # set min max numbers in scale based on legend_range or plotted numbers
  if (legend_range[1]=="none") {
    min_val <- min(delta_df$var, na.rm=T)
    max_val <- max(delta_df$var, na.rm=T)
  } else {
    min_val <- legend_range[1]
    max_val <- legend_range[2]
  }
  
  # Create sf object with WGS84 coordinates
  spatial_df <- st_as_sf(delta_df, coords = c("x", "y"), crs = 4326)
  
  # transform to robinson
  spatial_df_robin <- st_transform(spatial_df, robinson_projection)
  
  # extract robinson coordinates
  tiles_df <- as.data.frame(st_coordinates(spatial_df_robin)) %>%
    bind_cols(st_drop_geometry(spatial_df_robin) %>% 
                dplyr::select(var))
  
  p <- ggplot() +
    # land shape as background
    geom_sf(data=land, fill="grey40", color=NA) +
    # tiles with color gradient
    geom_tile(data = tiles_df,
              aes(X, Y, fill = var),
              width = tile_width, height = tile_height) +
    # set scico scale
    # oob=scales::squish compresses values outside limits to the boundary colors
    scale_fill_viridis_c(breaks=c(min_val, max_val),
                         labels = c(round(min_val,1), round(max_val,1)),
                         oob = scales::squish,
                         guide = guide_spec,
                         name = legend_name) +
    # Coordinate system for Robinson projection
    coord_sf(crs = robinson_projection, expand = FALSE,
             ylim = c(-6e6, 9.6e6)) +
    # panel label (left top)
    annotate("text", x = -1.6e7, y = 9.0e6, label = panel_letter, 
             hjust = 0, size = 4.5, color = "grey15", fontface = "bold") +
    # land cover label (left top, below panel letter)
    annotate("text",  x = -1.5e7, y = 9.0e6, label = panel_title,
             hjust = 0, size = 3.2, color = "grey30", fontface = "plain") +
    # Theme
    theme_void(base_size = 9) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA, linewidth = 0.3),
      plot.margin = if(include_legend) margin(2, 0, 0, 0) else margin(2, 0, 2, 0),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(size = 8.6, color = "grey30"),
      legend.text = element_text(size = 8, color = "grey30")
    )
  
  return(p)
}

# plot map of estimated change, negative=red, positive=blue
plot_change_value <- function(delta_df, var_name="fit_delta", legend_range="none",
                              panel_letter=NULL, panel_title=NULL, include_legend=FALSE,
                              legend_name=NULL) {
  # delta_df must have column x(lat), y(long), and a value column
  colnames(delta_df)[which(colnames(delta_df)==var_name)] <- "var"
  
  # make legend guide spec
  if (include_legend) {
    guide_spec <- guide_colorbar(title.position = "top", 
                                 title.hjust = 0.5,
                                 barwidth = 10, 
                                 barheight = 0.5)
  } else {guide_spec <- "none" }
  
  # set min max numbers in scale based on legend_range or plotted numbers
  if (legend_range[1]=="none") {
    min_val <- min(delta_df$var, na.rm=T)
    max_val <- max(delta_df$var, na.rm=T)
  } else {
    min_val <- legend_range[1]
    max_val <- legend_range[2]
  }
  
  # set conditional breaks and labels
  # if minimal change is too close to zero, omit zero
  breaks_list <- if (abs(min_val)>0.1*abs(max_val)) {
    c(min_val, 0, max_val)
  } else { c(min_val, max_val) }
  
  labels_list <- if (abs(min_val)>0.1*abs(max_val)) {
    c(paste0("<= ", min_val), "0", paste0(">= ", max_val))
  } else { c(paste0("<= ", min_val), paste0(">= ", max_val)) }
  
  # Create sf object with WGS84 coordinates
  spatial_df <- st_as_sf(delta_df, coords = c("x", "y"), crs = 4326)
  
  # transform to robinson
  spatial_df_robin <- st_transform(spatial_df, robinson_projection)
  
  # extract robinson coordinates
  tiles_df <- as.data.frame(st_coordinates(spatial_df_robin)) %>%
    bind_cols(st_drop_geometry(spatial_df_robin) %>% 
                dplyr::select(var))
  
  p <- ggplot() +
    # land shape as background
    geom_sf(data=land, fill="grey40", color=NA) +
    # tiles with color gradient
    geom_tile(data = tiles_df,
              aes(X, Y, fill = var),
              width = tile_width, height = tile_height) +
    # set scico scale
    # oob=scales::squish compresses values outside limits to the boundary colors
    scale_fill_gradient2(
      low = color_decrease, mid = color_neutral, high = color_increase,
      midpoint = 0, limits = c(min_val, max_val),
      oob = scales::squish,
      breaks = breaks_list,
      labels = labels_list,
      name = legend_name,
      guide = guide_spec
    ) +
    # Coordinate system for Robinson projection
    coord_sf(crs = robinson_projection, expand = FALSE,
             ylim = c(-6e6, 9.6e6)) +
    # panel label (left top)
    annotate("text", x = -1.6e7, y = 9.0e6, label = panel_letter, 
             hjust = 0, size = 4.5, color = "grey15", fontface = "bold") +
    # land cover label (left top, below panel letter)
    annotate("text",  x = -1.5e7, y = 9.0e6, label = panel_title,
             hjust = 0, size = 3.2, color = "grey30", fontface = "plain") +
    # Theme
    theme_void(base_size = 9) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA, linewidth = 0.3),
      plot.margin = if(include_legend) margin(2, 0, 0, 0) else margin(2, 0, 2, 0),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(size = 8.6, color = "grey30"),
      legend.text = element_text(size = 8, color = "grey30")
    )
  
  return(p)
}

# function that plot magnitude of sink change
plot_sink_change_magnitude <- function(df_name = "summary_diff_youngforest_1990s_2000s",
                                       panel_letter = "a",
                                       decade_lab_str = "2000s minus 1990s",
                                       include_legend = FALSE
) {
  
  # get dataframe
  summary <- get(df_name)
  
  # land cover
  landcover_str <- unique(summary$type)
  
  # Determine significance: percentage_above_zero > 0.95 or < 0.05
  summary <- summary %>%
    mutate(
      is_significant = (percentage_above_zero > 0.95) | (percentage_above_zero < 0.05),
      tile_color = ifelse(is_significant, diff_mean, 0)
    )
  
  # Create sf object with WGS84 coordinates
  spatial_df <- st_as_sf(summary, coords = c("x", "y"), crs = 4326)
  
  # transform to robinson
  spatial_df_robin <- st_transform(spatial_df, robinson_projection)
  
  # extract robinson coordinates
  tiles_df <- as.data.frame(st_coordinates(spatial_df_robin)) %>%
    bind_cols(st_drop_geometry(spatial_df_robin) %>% 
                dplyr::select(tile_color, is_significant))
  
  # separate significant and non-significant tiles
  tiles_nonsignificant <- filter(tiles_df, !is_significant)
  tiles_significant <- filter(tiles_df, is_significant)
  
  # make legend guide spec
  if (include_legend) {
    guide_spec <- guide_colorbar(title.position = "top", 
                                 title.hjust = 0.5,
                                 barwidth = 10, 
                                 barheight = 0.5)
  } else {
    guide_spec <- "none"
  }
  
  # Create plot
  plot_output <- ggplot() +
    # land shape as background
    geom_sf(data=land, fill="grey40", color=NA) +
    # Plot non-significant tiles in gray first (background layer)
    geom_tile(data = tiles_nonsignificant,
              aes(X, Y),
              fill = color_sink_change_nonsignificant, color=NA, width = tile_width, height = tile_height, alpha = 1) +
    # Plot significant tiles with color gradient
    geom_tile(data = tiles_significant,
              aes(X, Y, fill = tile_color),
              width = tile_width, height = tile_height) +
    # Color scale: red for decrease, green for increase
    # oob=scales::squish compresses values outside limits to the boundary colors
    scale_fill_gradient2(
      low = color_sink_decrease, mid = color_sink_neutral, high = color_sink_increase,
      midpoint = 0, limits = c(-magnitude_limit, magnitude_limit),
      oob = scales::squish,
      breaks = c(-magnitude_limit, 0, magnitude_limit),
      labels = c(paste0("<= -", magnitude_limit, "\n(weakened)"), 
                 "0", 
                 paste0(">= +", magnitude_limit, "\n(strengthened)")),
      name = paste0("Magnitude of SOC rate change:\n",decade_lab_str," (Mg C ha-1 yr-1)"),
      guide = guide_spec
    ) +
    # Coordinate system for Robinson projection
    coord_sf(crs = robinson_projection, expand = FALSE,
             ylim = c(-6e6, 8.5e6)) +
    # panel label (left top)
    annotate("text", x = -1.6e7, y = 7.9e6, label = panel_letter, 
             hjust = 0, size = 4.5, color = "grey15", fontface = "bold") +
    # land cover label (left top, below panel letter)
    annotate("text",  x = -1.5e7, y = 7.9e6, label = landcover_str,
             hjust = 0, size = 3.2, color = "grey30", fontface = "plain") +
    # Theme
    theme_void(base_size = 9) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA, linewidth = 0.3),
      plot.margin = if(include_legend) margin(2, 0, 0, 0) else margin(2, 0, 2, 0),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(size = 8.6, color = "grey30"),
      legend.text = element_text(size = 8, color = "grey30")
    )
  
  return(plot_output)
}

# function that plot probability of sink change
plot_sink_change_probability <- function(df_name = "summary_diff_youngforest_1990s_2000s",
                                         panel_letter = "a",
                                         include_legend = FALSE
) {
  
  # get dataframe
  summary <- get(df_name)
  
  # land cover
  landcover_str <- unique(summary$type)
  
  # transform ercentage_above_zero of sink change to signed probability
  summary <- summary %>%
    mutate(
      signed_probability = 2 * percentage_above_zero - 1,
      is_significant = (percentage_above_zero > 0.95) | (percentage_above_zero < 0.05)
    )
  
  # Create sf object with WGS84 coordinates
  spatial_df <- st_as_sf(summary, coords = c("x", "y"), crs = 4326)
  
  # transform to robinson
  spatial_df_robin <- st_transform(spatial_df, robinson_projection)
  
  # extract robinson coordinates
  tiles_df <- as.data.frame(st_coordinates(spatial_df_robin)) %>%
    bind_cols(st_drop_geometry(spatial_df_robin) %>% 
                dplyr::select(signed_probability, is_significant))
  
  # make legend guide spec
  if (include_legend) {
    guide_spec <- guide_colorbar(title.position = "top", 
                                 title.hjust = 0.5,
                                 barwidth = 10, 
                                 barheight = 0.5)
  } else {
    guide_spec <- "none"
  }
  
  # Create plot
  plot_output <- ggplot() +
    # land shape as background
    geom_sf(data=land, fill="grey40", color=NA) +
    # Plot all tiles with probability color gradient
    geom_tile(data = tiles_df,
              aes(X, Y, fill = signed_probability),
              width = tile_width, height = tile_height, color = NA) +
    # Color scale with sharp threshold lines at ±0.95
    scale_fill_gradientn(
      colors = color_palette_sink_change_probability,
      values = scales::rescale(as.numeric(names(color_palette_sink_change_probability)), 
                               to = c(0, 1), from = c(-1, 1)),
      limits = c(-1, 1),
      oob = scales::squish,
      # -0.9, 0.9 is 0.05-0.95 after transformed to have sign
      breaks = c(-1, -0.9, 0, 0.9, 1),
      labels = c("weakened\n(>95% decrease)", "", "no detectable\nchange", "", "strengthened\n(>95% increase)"),
      name = "Probability of SOC rate change", # \n(signed: 2*P(strengthened)-1)
      guide = guide_spec
    ) +
    # Coordinate system for Robinson projection
    coord_sf(crs = robinson_projection, expand = FALSE,
             ylim = c(-6e6, 8.5e6)) +
    # panel label (left top)
    annotate("text", x = -1.6e7, y = 7.9e6, label = panel_letter, 
             hjust = 0, size = 4.5, color = "grey15", fontface = "bold") +
    # land cover label (left top, below panel letter)
    annotate("text",  x = -1.5e7, y = 7.9e6, label = landcover_str,
             hjust = 0, size = 3.2, color = "grey30", fontface = "plain") +
    # Theme
    theme_void(base_size = 9) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA, linewidth = 0.3),
      plot.margin = if(include_legend) margin(2, 0, 0, 0) else margin(2, 0, 2, 0),
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(size = 8.6, color = "grey30"),
      legend.text = element_text(size = 8, color = "grey30")
    )
  return(plot_output)
}
#---------- load and prepare data -------------------------
# # time series dataset (final object name: df_std)
load("./data/SOC_timeseries_for_analysis_2026Jul29.rda")

# pixel level results
load("./results/find_CI_bayesian_cap2_exclude_ts_extent_pixel_summary_timeseries_new.RData")
# load input predictor data of all applicable pixels
load("./results/post_processing_bayesian_cap2_new_inputs_timeseries_new.rda")
# all and predicted pixels in separate dataframes, with land use proportion >0
load("./results/predicted_inputs_ts_extent_timeseries_new.rda") 
# load sink change results
load("./results/find_CI_bayesian_cap2_exclude_ts_extent_pixel_summary_timeseries_new_diff.RData")
# load land cover extent
load("./data/proportion_decade_df.rda")

# load model
load("./results/brms1_new_timeseries_cmd_3chain26k_50thin.rda")
mod <- brms1; rm(brms1)

# input layers (code to generate these files is in 00_download_prepare_spatial_data.R)
# mat_1992
mat_1992 <- raster(paste0(map_dir, "/mat_1992.tif"))
# map_1992
map_1992 <- raster(paste0(map_dir, "/map_1992.tif"))
# n_pct
n_pct <- raster(paste0(map_dir, "/n_sg_halfdegree.tif"))
# clay
clay <- raster(paste0(map_dir, "/clay_sg_halfdegree.tif"))
# ph
ph <- raster(paste0(map_dir, "/ph_sg_halfdegree.tif"))

# convert tifs to df
mat_1992_df <- raster2df(mat_1992)
map_1992_df <- raster2df(map_1992)
n_pct_df <- raster2df(n_pct) 
clay_df <- raster2df(clay)
ph_df <- raster2df(ph)

# load world administrative boundaries
# https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/?flg=en-us
basemap_dir <- map_dir
countries_sf <- read_sf(paste0(basemap_dir, "/world-administrative-boundaries/world-administrative-boundaries.shp"))
countries_sf <- st_make_valid(countries_sf)
countries_sf <- st_transform(countries_sf, 4326)

# get land shape in robinson projection
robinson_projection <- "+proj=robin +datum=WGS84"
land <- st_transform(st_union(st_make_valid(ne_countries(scale=50, returnclass="sf"))), robinson_projection)
# tile with and height for 0.5 degree resolution
tile_width <- 6.5e4
tile_height <- 6.5e4

# load region boundaries (Pan et al. regions)
region_boundaries <- st_transform(
  st_make_valid(st_read("./data/pan_regions.gpkg", quiet = TRUE)),
  crs = robinson_projection
)
# make raster template
raster_template <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90,
                        resolution = 0.5, crs = "EPSG:4326")

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
  "grassland" = "#E1A02A", 
  "young forest" = "#3C8DC4", 
  "old forest" = "#8064B0"
)

# use this website to lighten 50% https://mdigi.tools/lighten-color/#c8445b
type_palette_light <- c(
  "grassland" = "#f0cf94", 
  "young forest" = "#9dc6e1",
  "old forest" = "#bfb1d7"
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

# define colors for SOC change at pixel level (follow main figure color scheme)
color_increase <- "#2E8B7D"
color_decrease <- "#C97E3C"
color_neutral <- "gray80"

# define colors for SOC sink change
# slightly different gradient to distinguish between SOC change and sink change
# Blue for weakened sink, Orange for strengthened sink, Gray for neutral
color_sink_increase <- "#4575B4"
color_sink_decrease <- "#E41A1C"
color_sink_neutral <- "#F5F5F5"
color_sink_change_nonsignificant <- "gray70"

# Define color palette with sharp thresholds at ±0.95 (95% CI boundary)
# the scale was c(-1,1). -0.9 represents 5% percentile
color_palette_sink_change_probability <- c(
  "-1.0" = "#7F1616",      # Dark teal (extreme weakened)
  "-0.9001" = "#7F1616",   # Dark teal
  "-0.9" = "#202020",      # Dark threshold line
  "-0.8999" = "#F8D4D3",   # Light blue-green
  "0.0" = "#F5F5F5",       # Center (off-white, no clear change)
  "0.8999" = "#A8C5E0",    # Light orange
  "0.9" = "#202020",       # Dark threshold line
  "0.9001" = "#1F3F5C",    # Dark orange
  "1.0" = "#1F3F5C"        # Dark orange (extreme strengthened)
)

theme_set(theme_minimal(base_family = "Helvetica", base_size = 14))

#-----------------Figure Extended Data 1. Est. map and SE 1992-2000 --------------------------
# legend range for pixel level SOC change per area
# squish tails <2.5% and >97.5% to same color
all_SOC_rate <- c(summary_youngforest1990$annual_mean,
                  summary_oldforest1990$annual_mean,
                  summary_grassland1990$annual_mean)
SOC_rate_limits <- c(round(quantile(all_SOC_rate, 0.025, na.rm = TRUE), 1),
                     round(quantile(all_SOC_rate, 0.975, na.rm = TRUE), 1) )

# legend range for SE of pixel level SOC change 
# squish tails <2.5% and >97.5% to same color
all_SOC_rate_se <- c(summary_youngforest1990$annual_sd,
                     summary_oldforest1990$annual_sd,
                     summary_grassland1990$annual_sd)
SOC_se_limits <- c(round(quantile(all_SOC_rate_se, 0.025, na.rm = TRUE), 1),
                   round(quantile(all_SOC_rate_se, 0.975, na.rm = TRUE), 1) )

# legend range for pixel-level total SOC stock change
# squish tails <2.5% and >97.5% to same color
all_SOC_total_change <- c(summary_youngforest1990$annual_mean_GgperPixel,
                          summary_oldforest1990$annual_mean_GgperPixel,
                          summary_grassland1990$annual_mean_GgperPixel)
total_change_limits <- c(round(quantile(all_SOC_total_change, 0.025, na.rm = TRUE), 1),
                         round(quantile(all_SOC_total_change, 0.975, na.rm = TRUE), 1) )

# plot pixel level SOC rates
avg_grassland <- plot_change_value(summary_grassland1990, "annual_mean",
                                   legend_range = SOC_rate_limits, panel_letter="",
                                   panel_title="grassland 1992-2000", include_legend=F,legend_name=NULL)

avg_youngforest <- plot_change_value(summary_youngforest1990, "annual_mean",
                                     legend_range = SOC_rate_limits, panel_letter="",
                                     panel_title="young forest 1992-2000", include_legend=F,legend_name=NULL)

avg_oldforest <- plot_change_value(summary_oldforest1990, "annual_mean",
                                   legend_range = SOC_rate_limits, panel_letter="",
                                   panel_title="old forest 1992-2000", include_legend=T,legend_name="mean SOC rate (Mg C/ha yr-1)")

# plot standard error
sd_grassland <- plot_single_value_scico(summary_grassland1990, var_name="annual_sd", 
                                        legend_range = SOC_se_limits,  
                                        panel_title="grassland 1992-2000", include_legend=F, legend_name=NULL)

sd_youngforest <- plot_single_value_scico(summary_youngforest1990, var_name="annual_sd", 
                                          legend_range = SOC_se_limits,
                                          panel_title="young forest 1992-2000", include_legend=F, legend_name=NULL)

sd_oldforest <- plot_single_value_scico(summary_oldforest1990, var_name="annual_sd", 
                                        legend_range = SOC_se_limits,  
                                        panel_title="old forest 1992-2000", include_legend=T, legend_name="standard error (Mg C/ha yr-1)")

# plot aggregated total pixel SOC change 
total_grassland <- plot_change_value(summary_grassland1990, "annual_mean_GgperPixel",
                                     legend_range = total_change_limits, panel_letter="",
                                     panel_title="grassland 1992-2000", include_legend=F,legend_name=NULL)

total_youngforest <- plot_change_value(summary_youngforest1990, "annual_mean_GgperPixel",
                                       legend_range = total_change_limits, panel_letter="",
                                       panel_title="young forest 1992-2000", include_legend=F,legend_name=NULL)

total_oldforest <- plot_change_value(summary_oldforest1990, "annual_mean_GgperPixel",
                                     legend_range = total_change_limits, panel_letter="",
                                     panel_title="old forest 1992-2000", include_legend=T,legend_name="total SOC change per 0.5° pixel (Gg C yr-1)")

# Combine 9 panels in 3 columns x 3 rows layout
p_1992_2000 <- 
  (avg_grassland | sd_grassland | total_grassland) /
  (avg_youngforest | sd_youngforest | total_youngforest) /
  (avg_oldforest | sd_oldforest | total_oldforest) +
  plot_layout(heights = c(1, 1, 1.2)) &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(paste0(fig_dir, "/figureS?_1992_2000_SOCchange.png"), 
       plot = p_1992_2000, bg = "white", dpi = 600, width = 200 / 25.4, height = 150 / 25.4, units = "in")

#-----------------Figure Extended Data 2. Est. map and SE 2000-2010 --------------------------
# legend range for pixel level SOC change per area
# squish tails <2.5% and >97.5% to same color
all_SOC_rate <- c(summary_youngforest2000$annual_mean,
                  summary_oldforest2000$annual_mean,
                  summary_grassland2000$annual_mean)
SOC_rate_limits <- c(round(quantile(all_SOC_rate, 0.025, na.rm = TRUE), 1),
                     round(quantile(all_SOC_rate, 0.975, na.rm = TRUE), 1) )

# legend range for SE of pixel level SOC change 
# squish tails <2.5% and >97.5% to same color
all_SOC_rate_se <- c(summary_youngforest2000$annual_sd,
                     summary_oldforest2000$annual_sd,
                     summary_grassland2000$annual_sd)
SOC_se_limits <- c(round(quantile(all_SOC_rate_se, 0.025, na.rm = TRUE), 1),
                   round(quantile(all_SOC_rate_se, 0.975, na.rm = TRUE), 1) )

# legend range for pixel-level total SOC stock change
# squish tails <2.5% and >97.5% to same color
all_SOC_total_change <- c(summary_youngforest2000$annual_mean_GgperPixel,
                          summary_oldforest2000$annual_mean_GgperPixel,
                          summary_grassland2000$annual_mean_GgperPixel)
total_change_limits <- c(round(quantile(all_SOC_total_change, 0.025, na.rm = TRUE), 1),
                         round(quantile(all_SOC_total_change, 0.975, na.rm = TRUE), 1) )

# plot pixel level SOC rates
avg_grassland <- plot_change_value(summary_grassland2000, "annual_mean",
                                   legend_range = SOC_rate_limits, panel_letter="",
                                   panel_title="grassland 2000-2010", include_legend=F,legend_name=NULL)

avg_youngforest <- plot_change_value(summary_youngforest2000, "annual_mean",
                                     legend_range = SOC_rate_limits, panel_letter="",
                                     panel_title="young forest 2000-2010", include_legend=F,legend_name=NULL)

avg_oldforest <- plot_change_value(summary_oldforest2000, "annual_mean",
                                   legend_range = SOC_rate_limits, panel_letter="",
                                   panel_title="old forest 2000-2010", include_legend=T,legend_name="mean SOC rate (Mg C/ha yr-1)")

# plot standard error
sd_grassland <- plot_single_value_scico(summary_grassland2000, var_name="annual_sd", 
                                        legend_range = SOC_se_limits,  
                                        panel_title="grassland 2000-2010", include_legend=F, legend_name=NULL)

sd_youngforest <- plot_single_value_scico(summary_youngforest2000, var_name="annual_sd", 
                                          legend_range = SOC_se_limits,
                                          panel_title="young forest 2000-2010", include_legend=F, legend_name=NULL)

sd_oldforest <- plot_single_value_scico(summary_oldforest2000, var_name="annual_sd", 
                                        legend_range = SOC_se_limits,  
                                        panel_title="old forest 2000-2010", include_legend=T, legend_name="standard error (Mg C/ha yr-1)")

# plot aggregated total pixel SOC change 
total_grassland <- plot_change_value(summary_grassland2000, "annual_mean_GgperPixel",
                                     legend_range = total_change_limits, panel_letter="",
                                     panel_title="grassland 2000-2010", include_legend=F,legend_name=NULL)

total_youngforest <- plot_change_value(summary_youngforest2000, "annual_mean_GgperPixel",
                                       legend_range = total_change_limits, panel_letter="",
                                       panel_title="young forest 2000-2010", include_legend=F,legend_name=NULL)

total_oldforest <- plot_change_value(summary_oldforest2000, "annual_mean_GgperPixel",
                                     legend_range = total_change_limits, panel_letter="",
                                     panel_title="old forest 2000-2010", include_legend=T,legend_name="total SOC change per 0.5° pixel (Gg C yr-1)")

# Combine 9 panels in 3 columns x 3 rows layout
p_2000_2010 <- 
  (avg_grassland | sd_grassland | total_grassland) /
  (avg_youngforest | sd_youngforest | total_youngforest) /
  (avg_oldforest | sd_oldforest | total_oldforest) +
  plot_layout(heights = c(1, 1, 1.2)) &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(paste0(fig_dir, "/figureS?_2000_2010_SOCchange.png"), 
       plot = p_2000_2010, bg = "white", dpi = 600, width = 200 / 25.4, height = 150 / 25.4, units = "in")

#-----------------Figure Extended Data 3. Est. map and SE 2010-2020 --------------------------
# legend range for pixel level SOC change per area
# squish tails <2.5% and >97.5% to same color
all_SOC_rate <- c(summary_youngforest2010$annual_mean,
                  summary_oldforest2010$annual_mean,
                  summary_grassland2010$annual_mean)
SOC_rate_limits <- c(round(quantile(all_SOC_rate, 0.025, na.rm = TRUE), 1),
                     round(quantile(all_SOC_rate, 0.975, na.rm = TRUE), 1) )

# legend range for SE of pixel level SOC change 
# squish tails <2.5% and >97.5% to same color
all_SOC_rate_se <- c(summary_youngforest2010$annual_sd,
                     summary_oldforest2010$annual_sd,
                     summary_grassland2010$annual_sd)
SOC_se_limits <- c(round(quantile(all_SOC_rate_se, 0.025, na.rm = TRUE), 1),
                   round(quantile(all_SOC_rate_se, 0.975, na.rm = TRUE), 1) )

# legend range for pixel-level total SOC stock change
# squish tails <2.5% and >97.5% to same color
all_SOC_total_change <- c(summary_youngforest2010$annual_mean_GgperPixel,
                          summary_oldforest2010$annual_mean_GgperPixel,
                          summary_grassland2010$annual_mean_GgperPixel)
total_change_limits <- c(round(quantile(all_SOC_total_change, 0.025, na.rm = TRUE), 1),
                         round(quantile(all_SOC_total_change, 0.975, na.rm = TRUE), 1) )

# plot pixel level SOC rates
avg_grassland <- plot_change_value(summary_grassland2010, "annual_mean",
                                   legend_range = SOC_rate_limits, panel_letter="",
                                   panel_title="grassland 2010-2020", include_legend=F,legend_name=NULL)

avg_youngforest <- plot_change_value(summary_youngforest2010, "annual_mean",
                                     legend_range = SOC_rate_limits, panel_letter="",
                                     panel_title="young forest 2010-2020", include_legend=F,legend_name=NULL)

avg_oldforest <- plot_change_value(summary_oldforest2010, "annual_mean",
                                   legend_range = SOC_rate_limits, panel_letter="",
                                   panel_title="old forest 2010-2020", include_legend=T,legend_name="mean SOC rate (Mg C/ha yr-1)")

# plot standard error
sd_grassland <- plot_single_value_scico(summary_grassland2010, var_name="annual_sd", 
                                        legend_range = SOC_se_limits,  
                                        panel_title="grassland 2010-2020", include_legend=F, legend_name=NULL)

sd_youngforest <- plot_single_value_scico(summary_youngforest2010, var_name="annual_sd", 
                                          legend_range = SOC_se_limits,
                                          panel_title="young forest 2010-2020", include_legend=F, legend_name=NULL)

sd_oldforest <- plot_single_value_scico(summary_oldforest2010, var_name="annual_sd", 
                                        legend_range = SOC_se_limits,  
                                        panel_title="old forest 2010-2020", include_legend=T, legend_name="standard error (Mg C/ha yr-1)")

# plot aggregated total pixel SOC change 
total_grassland <- plot_change_value(summary_grassland2010, "annual_mean_GgperPixel",
                                     legend_range = total_change_limits, panel_letter="",
                                     panel_title="grassland 2010-2020", include_legend=F,legend_name=NULL)

total_youngforest <- plot_change_value(summary_youngforest2010, "annual_mean_GgperPixel",
                                       legend_range = total_change_limits, panel_letter="",
                                       panel_title="young forest 2010-2020", include_legend=F,legend_name=NULL)

total_oldforest <- plot_change_value(summary_oldforest2010, "annual_mean_GgperPixel",
                                     legend_range = total_change_limits, panel_letter="",
                                     panel_title="old forest 2010-2020", include_legend=T,legend_name="total SOC change per 0.5° pixel (Gg C yr-1)")

# Combine 9 panels in 3 columns x 3 rows layout
p_2010_2020 <- 
  (avg_grassland | sd_grassland | total_grassland) /
  (avg_youngforest | sd_youngforest | total_youngforest) /
  (avg_oldforest | sd_oldforest | total_oldforest) +
  plot_layout(heights = c(1, 1, 1.2)) &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(paste0(fig_dir, "/figureS?_2010_2020_SOCchange.png"), 
       plot = p_2010_2020, bg = "white", dpi = 600, width = 200 / 25.4, height = 150 / 25.4, units = "in")

#-----------------Figure Extended Data 4. Est. map and SE 1992-2020 --------------------------
# legend range for pixel level SOC change per area
# squish tails <2.5% and >97.5% to same color
all_SOC_rate <- c(summary_youngforest1992_2020$annual_mean,
                  summary_oldforest1992_2020$annual_mean,
                  summary_grassland1992_2020$annual_mean)
SOC_rate_limits <- c(round(quantile(all_SOC_rate, 0.025, na.rm = TRUE), 1),
                     round(quantile(all_SOC_rate, 0.975, na.rm = TRUE), 1) )

# legend range for SE of pixel level SOC change 
# squish tails <2.5% and >97.5% to same color
all_SOC_rate_se <- c(summary_youngforest1992_2020$annual_sd,
                     summary_oldforest1992_2020$annual_sd,
                     summary_grassland1992_2020$annual_sd)
SOC_se_limits <- c(round(quantile(all_SOC_rate_se, 0.025, na.rm = TRUE), 1),
                   round(quantile(all_SOC_rate_se, 0.975, na.rm = TRUE), 1) )

# legend range for pixel-level total SOC stock change
# squish tails <2.5% and >97.5% to same color
all_SOC_total_change <- c(summary_youngforest1992_2020$annual_mean_GgperPixel,
                          summary_oldforest1992_2020$annual_mean_GgperPixel,
                          summary_grassland1992_2020$annual_mean_GgperPixel)
total_change_limits <- c(round(quantile(all_SOC_total_change, 0.025, na.rm = TRUE), 1),
                         round(quantile(all_SOC_total_change, 0.975, na.rm = TRUE), 1) )

# plot pixel level SOC rates
avg_grassland <- plot_change_value(summary_grassland1992_2020, "annual_mean",
                                   legend_range = SOC_rate_limits, panel_letter="",
                                   panel_title="grassland 1992-2020", include_legend=F,legend_name=NULL)

avg_youngforest <- plot_change_value(summary_youngforest1992_2020, "annual_mean",
                                     legend_range = SOC_rate_limits, panel_letter="",
                                     panel_title="young forest 1992-2020", include_legend=F,legend_name=NULL)

avg_oldforest <- plot_change_value(summary_oldforest1992_2020, "annual_mean",
                                   legend_range = SOC_rate_limits, panel_letter="",
                                   panel_title="old forest 1992-2020", include_legend=T,legend_name="mean SOC rate (Mg C/ha yr-1)")

# plot standard error
sd_grassland <- plot_single_value_scico(summary_grassland1992_2020, var_name="annual_sd", 
                                        legend_range = SOC_se_limits,  
                                        panel_title="grassland 1992-2020", include_legend=F, legend_name=NULL)

sd_youngforest <- plot_single_value_scico(summary_youngforest1992_2020, var_name="annual_sd", 
                                          legend_range = SOC_se_limits,
                                          panel_title="young forest 1992-2020", include_legend=F, legend_name=NULL)

sd_oldforest <- plot_single_value_scico(summary_oldforest1992_2020, var_name="annual_sd", 
                                        legend_range = SOC_se_limits,  
                                        panel_title="old forest 1992-2020", include_legend=T, legend_name="standard error (Mg C/ha yr-1)")

# plot aggregated total pixel SOC change 
total_grassland <- plot_change_value(summary_grassland1992_2020, "annual_mean_GgperPixel",
                                     legend_range = total_change_limits, panel_letter="",
                                     panel_title="grassland 1992-2020", include_legend=F,legend_name=NULL)

total_youngforest <- plot_change_value(summary_youngforest1992_2020, "annual_mean_GgperPixel",
                                       legend_range = total_change_limits, panel_letter="",
                                       panel_title="young forest 1992-2020", include_legend=F,legend_name=NULL)

total_oldforest <- plot_change_value(summary_oldforest1992_2020, "annual_mean_GgperPixel",
                                     legend_range = total_change_limits, panel_letter="",
                                     panel_title="old forest 1992-2020", include_legend=T,legend_name="total SOC change per 0.5° pixel (Gg C yr-1)")

# Combine 9 panels in 3 columns x 3 rows layout
p_1992_2020 <- 
  (avg_grassland | sd_grassland | total_grassland) /
  (avg_youngforest | sd_youngforest | total_youngforest) /
  (avg_oldforest | sd_oldforest | total_oldforest) +
  plot_layout(heights = c(1, 1, 1.2)) &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(paste0(fig_dir, "/figureS?_1992_2020_SOCchange.png"), 
       plot = p_1992_2020, bg = "white", dpi = 600, width = 200 / 25.4, height = 150 / 25.4, units = "in")

#-----------------Figure Extended Data 5. Significant pixels -------------------------------
# filter delta dataframes for significant pixels
#1990-2000
summary_youngforest1990_sig <- summary_youngforest1990 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_oldforest1990_sig <- summary_oldforest1990 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_grassland1990_sig <- summary_grassland1990 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

# 2000-2010
summary_youngforest2000_sig <- summary_youngforest2000 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_oldforest2000_sig <- summary_oldforest2000 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_grassland2000_sig <- summary_grassland2000 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

# 2010-2020
summary_youngforest2010_sig <- summary_youngforest2010 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_oldforest2010_sig <- summary_oldforest2010 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_grassland2010_sig <- summary_grassland2010 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

# 1992-2020
summary_youngforest1992_2020_sig <- summary_youngforest1992_2020 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_oldforest1992_2020_sig <- summary_oldforest1992_2020 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

summary_grassland1992_2020_sig <- summary_grassland1992_2020 %>%
  dplyr::filter( (annual_97.5>=0 & annual_2.5>=0) | (annual_97.5<=0 & annual_2.5<=0) )

# legend range for pixel level SOC change per area
# squish tails <2.5% and >97.5% to same color
# grassland limits
sig_SOC_rate_g <- c(summary_grassland1990_sig$annual_mean,
                    summary_grassland2000_sig$annual_mean,
                    summary_grassland2010_sig$annual_mean,
                    summary_grassland1992_2020_sig$annual_mean)
sig_SOC_rate_g_limits <- c(round(quantile(sig_SOC_rate_g, 0.025, na.rm = TRUE), 1),
                           round(quantile(sig_SOC_rate_g, 0.975, na.rm = TRUE), 1) )

# young forest limits
sig_SOC_rate_yf <- c(summary_youngforest1990_sig$annual_mean,
                     summary_youngforest2000_sig$annual_mean,
                     summary_youngforest2010_sig$annual_mean,
                     summary_youngforest1992_2020_sig$annual_mean)
sig_SOC_rate_yf_limits <- c(round(quantile(sig_SOC_rate_yf, 0.025, na.rm = TRUE), 1),
                            round(quantile(sig_SOC_rate_yf, 0.975, na.rm = TRUE), 1) )

# old forest limits
sig_SOC_rate_of <- c(summary_oldforest1990_sig$annual_mean,
                     summary_oldforest2000_sig$annual_mean,
                     summary_oldforest2010_sig$annual_mean,
                     summary_oldforest1992_2020_sig$annual_mean)
sig_SOC_rate_of_limits <- c(round(quantile(sig_SOC_rate_of, 0.025, na.rm = TRUE), 1),
                            round(quantile(sig_SOC_rate_of, 0.975, na.rm = TRUE), 1) )

# plot grassland
sig_grassland1990 <- plot_change_value(summary_grassland1990_sig, "annual_mean",
                                       legend_range = sig_SOC_rate_g_limits, panel_letter="",
                                       panel_title="grassland 1992-2000", include_legend=F,legend_name=NULL)

sig_grassland2000 <- plot_change_value(summary_grassland2000_sig, "annual_mean",
                                       legend_range = sig_SOC_rate_g_limits, panel_letter="",
                                       panel_title="grassland 2000-2010", include_legend=F,legend_name=NULL)

sig_grassland2010 <- plot_change_value(summary_grassland2010_sig, "annual_mean",
                                       legend_range = sig_SOC_rate_g_limits, panel_letter="",
                                       panel_title="grassland 2010-2020", include_legend=F,legend_name=NULL)

sig_grassland1992_2020 <- plot_change_value(summary_grassland1992_2020_sig, "annual_mean",
                                            legend_range = sig_SOC_rate_g_limits, panel_letter="",
                                            panel_title="grassland 1992-2020", include_legend=T,legend_name="mean SOC rate (Mg C/ha yr-1)")

# plot young forest
sig_youngforest1990 <- plot_change_value(summary_youngforest1990_sig, "annual_mean",
                                         legend_range = sig_SOC_rate_yf_limits, panel_letter="",
                                         panel_title="young forest 1992-2000", include_legend=F,legend_name=NULL)

sig_youngforest2000 <- plot_change_value(summary_youngforest2000_sig, "annual_mean",
                                         legend_range = sig_SOC_rate_yf_limits, panel_letter="",
                                         panel_title="young forest 2000-2010", include_legend=F,legend_name=NULL)

sig_youngforest2010 <- plot_change_value(summary_youngforest2010_sig, "annual_mean",
                                         legend_range = sig_SOC_rate_yf_limits, panel_letter="",
                                         panel_title="young forest 2010-2020", include_legend=F,legend_name=NULL)

sig_youngforest1992_2020 <- plot_change_value(summary_youngforest1992_2020_sig, "annual_mean",
                                              legend_range = sig_SOC_rate_yf_limits, panel_letter="",
                                              panel_title="young forest 1992-2020", include_legend=T,legend_name="mean SOC rate (Mg C/ha yr-1)")

# plot old forest
sig_oldforest1990 <- plot_change_value(summary_oldforest1990_sig, "annual_mean",
                                       legend_range = sig_SOC_rate_of_limits, panel_letter="",
                                       panel_title="old forest 1992-2000", include_legend=F,legend_name=NULL)

sig_oldforest2000 <- plot_change_value(summary_oldforest2000_sig, "annual_mean",
                                       legend_range = sig_SOC_rate_of_limits, panel_letter="",
                                       panel_title="old forest 2000-2010", include_legend=F,legend_name=NULL)

sig_oldforest2010 <- plot_change_value(summary_oldforest2010_sig, "annual_mean",
                                       legend_range = sig_SOC_rate_of_limits, panel_letter="",
                                       panel_title="old forest 2010-2020", include_legend=F,legend_name=NULL)

sig_oldforest1992_2020 <- plot_change_value(summary_oldforest1992_2020_sig, "annual_mean",
                                            legend_range = sig_SOC_rate_of_limits, panel_letter="",
                                            panel_title="old forest 1992-2020", include_legend=T,legend_name="mean SOC rate (Mg C/ha yr-1)")


# Combine 9 panels in 3 columns x 4 rows layout
p_sig <-
  (sig_grassland1990  | sig_youngforest1990  | sig_oldforest1990 ) /
  (sig_grassland2000  | sig_youngforest2000  | sig_oldforest2000 ) /
  (sig_grassland2010  | sig_youngforest2010  | sig_oldforest2010 ) /
  (sig_grassland1992_2020  | sig_youngforest1992_2020  | sig_oldforest1992_2020 ) +
  plot_layout(heights = c(1, 1, 1, 1.2)) &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(paste0(fig_dir, "/figureS?_sig_SOCchange.png"), plot = p_sig, bg = "white", dpi = 600, 
       width = 200 / 25.4, height = 180 / 25.4, units = "in")

#-----------------Figure Extended Data 6. sink change 1990s to 2000s ------------------------
# set limit to legend of sink changes for this figure (1990s vs. 2000s) and the next (2000s vs. 2010s)
all_differences <- c(
  abs(summary_diff_grassland_1990s_2000s$diff_mean),
  abs(summary_diff_youngforest_1990s_2000s$diff_mean),
  abs(summary_diff_oldforest_1990s_2000s$diff_mean),
  abs(summary_diff_grassland_2000s_2010s$diff_mean),
  abs(summary_diff_youngforest_2000s_2010s$diff_mean),
  abs(summary_diff_oldforest_2000s_2010s$diff_mean)
)
magnitude_limit <- round(quantile(all_differences, 0.96, na.rm = TRUE), 1)
print(paste("Magnitude limit (96th percentile):", magnitude_limit))

# plot magnitude 1990 vs 2000
diff_g_1990v2000 <- plot_sink_change_magnitude(df_name = "summary_diff_grassland_1990s_2000s",
                                               panel_letter = "a",
                                               decade_lab_str = "2000s minus 1990s",
                                               include_legend = FALSE)
diff_yf_1990v2000 <- plot_sink_change_magnitude(df_name = "summary_diff_youngforest_1990s_2000s",
                                                panel_letter = "b",
                                                decade_lab_str = "2000s minus 1990s",
                                                include_legend = FALSE)
diff_of_1990v2000 <- plot_sink_change_magnitude(df_name = "summary_diff_oldforest_1990s_2000s",
                                                panel_letter = "c",
                                                decade_lab_str = "2000s minus 1990s",
                                                include_legend = TRUE)

# plot probability, 1990v2000
diff_g_1990v2000_prob <- plot_sink_change_probability(df_name = "summary_diff_grassland_1990s_2000s",
                                                      panel_letter = "d",
                                                      include_legend = FALSE)
diff_yf_1990v2000_prob <- plot_sink_change_probability(df_name = "summary_diff_youngforest_1990s_2000s",
                                                       panel_letter = "e",
                                                       include_legend = FALSE)
diff_of_1990v2000_prob <- plot_sink_change_probability(df_name = "summary_diff_oldforest_1990s_2000s",
                                                       panel_letter = "f",
                                                       include_legend = TRUE)

# if 
# Combine 6 panels in 2 columns x 3 rows layout
combined_figure <- (diff_g_1990v2000 | diff_g_1990v2000_prob) /
  (diff_yf_1990v2000 | diff_yf_1990v2000_prob) /
  (diff_of_1990v2000 | diff_of_1990v2000_prob) +
  plot_layout(heights = c(1, 1, 1.2)) &
  theme(plot.tag = element_text(face = "bold", size = 13))

# Save as full page PNG (Letter: 8.5 x 11 inches)
ggsave(paste0(fig_dir, "/FigureS?_sink_change_magnitude_probability_1990s_vs_2000s.png"),
       plot = combined_figure,
       width = 8.5, height = 8, dpi = 300, bg = "white")

#-----------------Figure Extended Data 7. sink change 2000s to 2010s ------------------------
# plot magnitude 2000s vs 2010s
diff_g_2000v2010 <- plot_sink_change_magnitude(df_name = "summary_diff_grassland_2000s_2010s",
                                               panel_letter = "a",
                                               decade_lab_str = "2010s minus 2000s",
                                               include_legend = FALSE)
diff_yf_2000v2010 <- plot_sink_change_magnitude(df_name = "summary_diff_youngforest_2000s_2010s",
                                                panel_letter = "b",
                                                decade_lab_str = "2010s minus 2000s",
                                                include_legend = FALSE)
diff_of_2000v2010 <- plot_sink_change_magnitude(df_name = "summary_diff_oldforest_2000s_2010s",
                                                panel_letter = "c",
                                                decade_lab_str = "2010s minus 2000s",
                                                include_legend = TRUE)

# plot probability, 1990v2000
diff_g_2000v2010_prob <- plot_sink_change_probability(df_name = "summary_diff_grassland_2000s_2010s",
                                                      panel_letter = "d",
                                                      
                                                      include_legend = FALSE)
diff_yf_2000v2010_prob <- plot_sink_change_probability(df_name = "summary_diff_youngforest_2000s_2010s",
                                                       panel_letter = "e",
                                                       include_legend = FALSE)
diff_of_2000v2010_prob <- plot_sink_change_probability(df_name = "summary_diff_oldforest_2000s_2010s",
                                                       panel_letter = "f",
                                                       include_legend = TRUE)

# if 
# Combine 6 panels in 2 columns x 3 rows layout
combined_figure <- (diff_g_2000v2010 | diff_g_2000v2010_prob) /
  (diff_yf_2000v2010 | diff_yf_2000v2010_prob) /
  (diff_of_2000v2010 | diff_of_2000v2010_prob) +
  plot_layout(heights = c(1, 1, 1.2)) &
  theme(plot.tag = element_text(face = "bold", size = 13))

# Save as full page PNG (Letter: 8.5 x 11 inches)
ggsave(paste0(fig_dir, "/FigureS?_sink_change_magnitude_probability_2000s_vs_2010s.png"),
       plot = combined_figure,
       width = 8.5, height = 8, dpi = 300, bg = "white")

#-----------------Figure S1 extrapolation extent ------------------------------

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
    # region boundaries
    geom_sf(data = region_boundaries,
            fill = NA, colour = "grey60", linewidth = 0.2) +
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

# build rasters of all pixels in a dataframe in robinson projection
build_mask <- function(predicted_df) {
  # make raster with value 1=predicted
  r <- raster_template; r[cellFromXY(r, as.matrix(predicted_df[, c("x","y")]))] <- 1
  # transform to robinson projection
  suppressWarnings(st_transform(st_as_sf(as.polygons(r, dissolve = TRUE)), robinson_projection))
}

# plot with explicit which variable is out of range
plot_extrapolation_variable <- function(input_df,type_str="young forest",
                                        panel_letter="", panel_title="") {
  
  # pixels within domain (predicted)
  predicted_mask <- build_mask(filter(input_df, extrapolation_fraction==0))
  
  # pixels that require extrapolation (excluded)
  extrapolate_combined <- rbind(
    build_mask(filter(input_df, MAT_extrapolation)) %>% mutate(extrapolation_type = "MAT"),
    build_mask(filter(input_df, MAP_extrapolation)) %>% mutate(extrapolation_type = "MAP"),
    build_mask(filter(input_df, n_extrapolation)) %>% mutate(extrapolation_type = "soil N"),
    build_mask(filter(input_df, clay_extrapolation)) %>% mutate(extrapolation_type = "soil clay"),
    build_mask(filter(input_df, ph_extrapolation)) %>% mutate(extrapolation_type = "soil pH")
  )
  
  # plot
  plot <- ggplot(countries_sf) +
    # region boundaries
    geom_sf(data = region_boundaries,
            fill = NA, colour = "grey60", linewidth = 0.2) +
    # pixels that requires extrapolation (excluded from estimates)
    geom_sf(data = extrapolate_combined, aes(fill = extrapolation_type), 
            color = NA, alpha = alpha_extrapolation) +
    scale_fill_manual(values = extrapolation_variable_palette, 
                      breaks = c("MAT", "MAP", "soil N", "soil clay", "soil pH"),
                      name = "Excluded pixels due to \nout-of-range predictor(s)",) +
    new_scale_fill() +
    # predicted pixels within domain
    geom_sf(data = predicted_mask, aes(fill = type_str), color = NA,) +
    scale_fill_manual(values = type_palette, 
                      labels = type_str,
                      name = "Predicted pixels") +
    guides(fill = guide_legend(order = 1),
           `fill_newscale` = guide_legend(order = 2)) +
    labs(x=NULL, y=NULL) + 
    # Coordinate system for Robinson projection
    coord_sf(crs = robinson_projection, expand = FALSE,
             ylim = c(-6e6, 9.6e6)) +
    # panel label (left top)
    annotate("text", x = -1.6e7, y = 9.0e6, label = panel_letter, 
             hjust = 0, size = 4.5, color = "grey15", fontface = "bold") +
    # land cover label (left top, below panel letter)
    annotate("text",  x = -1.5e7, y = 9.0e6, label = panel_title,
             hjust = 0, size = 3.2, color = "grey30", fontface = "plain") +
    # Theme
    theme_void(base_size = 9) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA, linewidth = 0.3),
      legend.position = "right",
      legend.title = element_text(size = 8.6, color = "grey30"),
      legend.text = element_text(size = 8, color = "grey30"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.title.x = element_blank(),  # Remove x-axis title
      axis.title.y = element_blank(),  # Remove y-axis title
      axis.text.x = element_blank(),    # Remove x-axis text
      axis.text.y = element_blank(),    # Remove y-axis text
      axis.ticks.x = element_blank(),   # Remove x-axis ticks
      axis.ticks.y = element_blank(),  # Remove y-axis ticks
    )
  
  return(plot)
}

extrapolation_variable_g <- plot_extrapolation_variable(input_grassland_all,"grassland", panel_title="grassland")
extrapolation_variable_yf <- plot_extrapolation_variable(input_youngforest_all,"young forest", panel_title="young forest")
extrapolation_variable_of <- plot_extrapolation_variable(input_oldforest_all,"old forest", panel_title="old forest")

prediction_extent <- grid.arrange(
  extrapolation_variable_g,
  extrapolation_variable_yf,
  extrapolation_variable_of,
  ncol=1)

# combine 3 panels
prediction_extent <- 
  extrapolation_variable_g /
  extrapolation_variable_yf /
  extrapolation_variable_of +
  plot_layout(heights = c(1, 1, 1)) &
  theme(plot.tag = element_text(face = "bold", size = 13))


# save
ggsave(paste0(fig_dir, "/figureS?_prediction_extent.png"), 
       plot = prediction_extent, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 210 / 25.4, units = "in")

#-----------------Figure S2 distribution of data input variables vs. predicted pixels (cap2_exclude) ---------
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

# # save
# save(predicted_xy, file="./results/predicted_xy.rda")

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

ggsave(paste0(fig_dir, "/figureS?_predictor_data_distribution.png"), plot = p_distribution, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 120 / 25.4, units = "in")


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
    known_grassland_grazing = 0,
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
    known_grassland_grazing = 0,
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


#-----------------Figure S4 predictor layer maps ------------
predicted_pixels <- rbind(input_grassland_pred,
                          input_youngforest_pred,input_oldforest_pred) %>%
  distinct(x,y)

# omit NA values
clay_df_omit <- merge(predicted_pixels, clay_df, all.x=T)
n_pct_df_omit <- merge(predicted_pixels, n_pct_df, all.x=T)
ph_df_omit <- merge(predicted_pixels, ph_df, all.x=T)

mat_1992_df_omit <- merge(predicted_pixels, mat_1992_df, all.x=T)
map_1992_df_omit <- merge(predicted_pixels, map_1992_df, all.x=T)

# plot input layers
p_mat <- plot_single_value_viridis(mat_1992_df_omit, var_name="value", 
                                   panel_title="", include_legend=T, legend_name="MAT 1992 (°C)")
p_map <- plot_single_value_viridis(map_1992_df_omit, var_name="value", 
                                   panel_title="", include_legend=T, legend_name="MAP 1992 (mm)")
p_n_pct <- plot_single_value_viridis(n_pct_df_omit, var_name="value", 
                                     panel_title="", include_legend=T, legend_name="Soil nitrogen (%))")
p_clay <- plot_single_value_viridis(clay_df_omit, var_name="value", 
                                    panel_title="", include_legend=T, legend_name="Soil clay (%)")
p_ph <- plot_single_value_viridis(ph_df_omit, var_name="value", 
                                  panel_title="", include_legend=T, legend_name="Soil pH")

# Combine 9 panels in 3 columns x 3 rows layout
p_input_layers <- 
  (p_mat | p_map) /
  (p_n_pct | p_clay) /
  (p_ph | plot_spacer() ) +
  plot_layout(heights = c(1, 1, 1)) &
  theme(plot.tag = element_text(face = "bold", size = 13))

#save
ggsave(paste0(fig_dir, "figureS?_predictor_layers.png"), plot = p_input_layers, bg = "white", dpi = 600, width = 183 / 25.4, height = 180 / 25.4, units = "in")

#-----------------Figure S5 land use proportions ----------------------
# make into one map per land use (cuz every decade is the same extent now)
# use truncated or not tunrcate version?
# use input_lc_pred, or input_lc_all # maybe do _pred for now see what people say
# or do one _pred one the difference between _all and _pred (to show the uncaptured pixels?)
# that seems redundant with the truncation extent layer

# plot land cover fractions (modify to vertical legend on the right)
p_g_fraction <- plot_single_value_viridis(input_grassland_pred, var_name="proportion", 
                                          panel_title="grassland", include_legend=T, legend_name="fraction") + 
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 0.5,      # ← swapped from 10
    barheight = 6,      # ← swapped from 0.5
    orientation = "vertical"  # ← add this
  )) +
  theme(legend.position = "right")
p_yf_fraction <- plot_single_value_viridis(input_youngforest_pred, var_name="proportion", 
                                           panel_title="young forest", include_legend=T, legend_name="fraction") +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 0.5,      # ← swapped from 10
    barheight = 6,      # ← swapped from 0.5
    orientation = "vertical"  # ← add this
  )) +
  theme(legend.position = "right")
p_of_fraction <- plot_single_value_viridis(input_oldforest_pred, var_name="proportion", 
                                           panel_title="old forest", include_legend=T, legend_name="fraction") + 
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 0.5,      # ← swapped from 10
    barheight = 6,      # ← swapped from 0.5
    orientation = "vertical"  # ← add this
  )) +
  theme(legend.position = "right")

p_fraction <- 
  p_g_fraction /
  p_yf_fraction /
  p_of_fraction  +
  theme(plot.tag = element_text(face = "bold", size = 13))

#save
ggsave(paste0(fig_dir, "/FigureS?_fraction.png"), plot = p_fraction, bg = "white", 
       dpi = 600, width = 183 / 25.4, height = 210 / 25.4, units = "in")
