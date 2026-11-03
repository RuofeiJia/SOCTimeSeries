# Figure 1: SOC time series database
rm(list = ls(all = TRUE))
## ---- 0. packages ----
suppressMessages({
  library(tidyverse); library(purrr); library(ggplot2); library(cNORM);
  library(sf); library(rnaturalearth); library(patchwork); library(ggpattern);
  library(tictoc)
})
set.seed(1)
suppressMessages(sf::sf_use_s2(FALSE))   # planar ops; avoids s2 invalid-loop errors
select <- dplyr::select
GLOBAL <- "#2B2B2B"; CORAL <- "#D85A30"; GREY <- "#B4B2A9"

## ---- 1. calculate plot-level SOC rate ----
load("./data/SOC_timeseries_for_analysis_2026Jul29.rda")   # -> df_std

base <- df_std %>%
  filter(!is.na(SOC_stock30), !is.na(observation_year),
         !is.na(zone), !is.na(forest_cond_gf_besnard60))

plot_rate <- base %>%
  group_by(siteID) %>%
  dplyr::mutate(
    n_plot = n_distinct(plotID),
    weight = 1/n_distinct(plotID)
  ) %>% ungroup() %>%
  group_by(plotID) %>%
  filter(n_distinct(observation_year) >= 2) %>%
  summarise(
    rate_val = cov(observation_year, SOC_stock30) / var(observation_year),
    weight = weight[1],
    n_obs    = n(),
    yr_first = min(observation_year), yr_last = max(observation_year),
    span     = yr_last - yr_first,
    zone2    = tools::toTitleCase(as.character(first(zone))),
    cond     = as.character(first(forest_cond_gf_besnard60)),
    lat      = first(lat), long = first(long),
    .groups  = "drop") %>%
  mutate(biome = paste0(zone2, " · ", cond))

biome_levels <- c("Temperate · grassland","Temperate · young forest",
                  "Temperate · old forest","Boreal · grassland",
                  "Boreal · young forest","Boreal · old forest",
                  "Tropical · grassland","Tropical · young forest",
                  "Tropical · old forest")

boot_ci <- function(x, w, B = 1000) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(c(mean = mean(x), lo = NA, hi = NA))
  bs <- replicate(B, mean(sample(x, length(x), replace = TRUE, prob=w/sum(w))))
  c(mean = mean(x), lo = unname(quantile(bs, .025)), hi = unname(quantile(bs, .975)))
}

TAG <- theme(plot.tag = element_text(face = "bold", size = 19))

## ---- prepare PANEL a: map of TS points ----
world <- ne_countries(scale = 110, returnclass = "sf") |> st_make_valid()
land  <- st_union(world)
robin <- "+proj=robin +datum=WGS84"
# region contour (in data folder)
regions <- st_read("./data/pan_regions.gpkg", quiet = TRUE) |> st_make_valid()

suppressMessages(library(terra))

load("./results/find_CI_bayesian_cap2_exclude_ts_extent_pixel_summary_timeseries_new.RData")
raster_template <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90,
                        resolution = 0.5, crs = "EPSG:4326")

build_mask <- function(clip) {
  predicted_df <- get(paste0("summary_", clip, "1992_2020"))
  r <- raster_template; r[cellFromXY(r, as.matrix(predicted_df[, c("x","y")]))] <- 1
  suppressWarnings(st_transform(st_as_sf(as.polygons(r, dissolve = TRUE)), robin))
}

mask_grass <- build_mask("grassland")
mask_young <- build_mask("youngforest")
mask_old   <- build_mask("oldforest")

class_cols <- c(
  "grassland" = "#E1A02A", 
  "young forest" = "#3C8DC4", 
  "old forest" = "#8064B0")
class_cols_light <- c(
  "grassland" = "#e7b355", 
  "young forest" = "#63a4d0",
  "old forest" = "#9983c0"
)

## ---- in-domain footprint and plot PANEL a ----
RGBM <- col2rgb(class_cols_light)

all_predicted_pixels <- rbind(summary_grassland1992_2020,summary_youngforest1992_2020,
                              summary_oldforest1992_2020) %>%
  mutate(x = round(x, 3), y = round(y, 3),
         type = stringr::str_remove(type, " ") 
  ) %>%
  pivot_wider(
    id_cols = c(x, y),
    names_from = type,
    values_from = proportion,
    names_glue = "{type}_proportion"
  ) %>%
  mutate(across(ends_with("_proportion"), ~replace_na(., 0))) %>%
  mutate(
    coord_str = paste0(x," ",y),
    total_proportion = grassland_proportion + youngforest_proportion + oldforest_proportion
  ) %>%
  mutate(
    grassland_weight = grassland_proportion/total_proportion,
    youngforest_weight = youngforest_proportion/total_proportion,
    oldforest_weight = oldforest_proportion/total_proportion,
  ) %>%
  mutate(
    R = grassland_weight*RGBM[1,1]+youngforest_weight*RGBM[1,2]+oldforest_weight*RGBM[1,3],
    G = grassland_weight*RGBM[2,1]+youngforest_weight*RGBM[2,2]+oldforest_weight*RGBM[2,3],
    B = grassland_weight*RGBM[3,1]+youngforest_weight*RGBM[3,2]+oldforest_weight*RGBM[3,3]
  ) %>%
  mutate(
    col = grDevices::rgb(R,G,B, alpha=255, maxColorValue=255) 
  )

pixel_blended_colors <- all_predicted_pixels %>%
  dplyr::select(x,y,col)

pixel_blended_colors <- cbind(as.data.frame(st_coordinates(st_transform(st_as_sf(pixel_blended_colors, coords=c("x","y"), crs=4326), robin))), 
                              col=pixel_blended_colors$col)

plots_sf <- plot_rate %>%
  filter(!is.na(lat), !is.na(long), cond %in% names(class_cols)) %>%
  mutate(cond = factor(cond, levels = names(class_cols))) %>%
  arrange(desc(span)) %>%
  st_as_sf(coords = c("long","lat"), crs = 4326)

tp <- function(x) st_transform(x, robin)
indom_union <- st_union(c(st_geometry(mask_grass), st_geometry(mask_young), st_geometry(mask_old)))
outdom_land <- st_sf(geometry = st_difference(st_make_valid(tp(land)), st_make_valid(indom_union)))
outdom_land$domain <- "out of domain (excluded)"

p_a <- ggplot() +
  # world background
  geom_sf(data = tp(land),  fill = "grey90", color = NA) +
  # out of domain pattern
  geom_sf_pattern(data = outdom_land, aes(pattern = domain), fill = "grey90", colour = NA,
                  pattern_fill = "grey55", pattern_colour = NA, pattern_angle = 45,
                  pattern_density = 0.07, pattern_spacing = 0.008, pattern_size = 0.15) +
  
  # three land cover colors overlay
  geom_sf(data = mask_grass, fill = class_cols["grassland"],    color = NA, alpha = 0.55) +
  geom_sf(data = mask_young, fill = class_cols["young forest"], color = NA, alpha = 0.55) +
  geom_sf(data = mask_old,   fill = class_cols["old forest"],   color = NA, alpha = 0.55) +
  
  # contour
  geom_sf(data = tp(regions), fill = NA, color = "grey72", linewidth = 0.2) +
  # data points
  geom_sf(data = tp(plots_sf), aes(fill = cond, size = span),
          shape = 21, color = "black", stroke = 0.2, alpha = 0.9) +
  
  # legends
  scale_fill_manual(values = class_cols, name = "repeat-sampling site",
                    breaks = c("grassland","young forest","old forest")) +
  scale_pattern_manual(values = c("out of domain (excluded)" = "stripe"), name = NULL) +
  scale_size_continuous(range = c(0.6, 4.6), name = "time-series span (yr)",
                        breaks = c(5, 15, 30)) +
  guides(fill = guide_legend(title = "repeat-sampling sites", order = 1,
                             override.aes = list(size = 4, shape = 21, alpha = 1, pattern = "none")),
         pattern = guide_legend(title = NULL, order = 2,
                                override.aes = list(fill = "grey90", colour = NA)),
         size = guide_legend(title = "time-series span (yr)", order = 3,
                             override.aes = list(shape = 21, fill = "grey55", pattern = "none"))) +
  # crop the dead eastern Pacific (left) + western Pacific (right) + Southern Ocean so
  # the sampled continents fill the banner; banner height (below) tuned to this aspect
  coord_sf(crs = robin, xlim = c(-1.26e7, 1.56e7), ylim = c(-6.2e6, 8.0e6), expand = FALSE) +
  labs(tag = "a") +
  theme_void(base_size = 13) + TAG +
  # ocean fill on BOTH panel and plot background so the map fills its cell edge-to-edge
  theme(panel.background = element_rect(fill = "#EAF0F3", color = NA),
        plot.background  = element_rect(fill = "#EAF0F3", color = "grey75", linewidth = 0.4),
        plot.margin = margin(2, 2, 2, 2),
        legend.position = "bottom", legend.box = "horizontal",
        legend.text = element_text(size = 10), legend.title = element_text(size = 10),
        legend.margin = margin(1, 6, 1, 6))

p_a

## ---- PANEL b: temporal structure ----
# randomly sample 180 plots
samp <- plot_rate %>% filter(biome %in% biome_levels) %>%
  slice_sample(n = 180) %>% arrange(yr_first, span) %>% mutate(row = row_number())
# x and xend segments for plotting
seg  <- samp %>% transmute(row, x = yr_first, xend = yr_last)
# sampled points to be plotted
pts  <- base %>% distinct(plotID, observation_year) %>%
  # get row number to plot on line segments
  inner_join(samp %>% select(plotID, row), by = "plotID")

# get resampled intervals
dt_tab <- base %>% distinct(plotID, observation_year) %>%
  group_by(plotID) %>% filter(n() >= 2) %>% arrange(observation_year) %>%
  reframe(dt = diff(observation_year))

# median resampled interval
med_dt   <- median(dt_tab$dt)
# median sampling time span of plots
med_span <- median(plot_rate$span) # plot_rate$span[plot_rate$biome %in% biome_levels]

# inset: resample-interval histogram (the one thing the raster doesn't show)
p_inset <- ggplot(dt_tab, aes(dt)) +
  geom_histogram(binwidth = 2, boundary = 0, fill = GLOBAL, color = "white", linewidth = 0.2) +
  geom_vline(xintercept = med_dt, linetype = "dashed", color = "grey25", linewidth = 0.3) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.03)), limits = c(0, 30)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "resample interval (yr)", y = "intervals") +
  theme_classic(base_size = 10) +
  theme(plot.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3),
        axis.title = element_text(size = 9), plot.margin = margin(3, 5, 3, 3))

p_b <- ggplot() +
  geom_segment(data = seg, aes(x = x, xend = xend, y = row, yend = row),
               color = GREY, linewidth = 0.3) +
  geom_point(data = pts, aes(observation_year, row), color = CORAL, size = 0.95) +
  annotate("text", x = max(pts$observation_year), y = 8, hjust = 1, size = 4.0,
           color = "grey25", label = sprintf("median interval %g yr  ·  median span %g yr",
                                             med_dt, med_span)) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = "Year", y = "Resampled plot", tag = "b") +
  theme_classic(base_size = 14) + TAG +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank(), plot.margin = margin(2, 6, 2, 4))

p_b <- p_b + inset_element(p_inset, left = 0.03, bottom = 0.52, right = 0.42,
                           top = 0.98, ignore_tag = TRUE)

p_b
## ---- PANEL c: in-domain coverage per biome ----
# % of each biome's area that falls within the upscaling model's predictor range.
# manually input % of each biome's area within the model domain
domain_pct <- c(
  "Boreal · grassland"    = 14, "Boreal · young forest"    = 48, "Boreal · old forest"    = 34,
  "Temperate · grassland" = 35, "Temperate · young forest" = 59, "Temperate · old forest" = 72,
  "Tropical · grassland"  = 13, "Tropical · young forest"  = 26, "Tropical · old forest"  = 11)

# order boreal -> temperate -> tropical (top to bottom), grassland -> young -> old within each
biome_order <- c("Boreal · grassland","Boreal · young forest","Boreal · old forest",
                 "Temperate · grassland","Temperate · young forest","Temperate · old forest",
                 "Tropical · grassland","Tropical · young forest","Tropical · old forest")

cov_long <- tibble(
  biome = rep(names(domain_pct), 2),
  part  = rep(c("in","out"), each = length(domain_pct)),
  pct   = c(unname(domain_pct), 100 - unname(domain_pct))) %>%
  mutate(biome = factor(biome, levels = rev(biome_order)),
         part  = factor(part, levels = c("out","in")))

# out-of-domain drawn with hatched = excluded from the result
p_c <- ggplot(cov_long, aes(x = pct, y = biome, fill = part, pattern = part)) +
  geom_col_pattern(width = 0.7, color = "grey70", linewidth = 0.2,
                   pattern_fill = "grey55", pattern_colour = NA, pattern_angle = 45,
                   pattern_density = 0.12, pattern_spacing = 0.017,
                   pattern_key_scale_factor = 0.4) +
  scale_fill_manual(values = c(`in` = GLOBAL, out = "grey90"),
                    labels = c(`in` = "in-domain", out = "out of domain (excluded)"), name = NULL) +
  scale_pattern_manual(values = c(`in` = "none", out = "stripe"),
                       labels = c(`in` = "in-domain", out = "out of domain (excluded)"), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
  labs(x = "% of biome area within model domain", y = NULL, tag = "c") +
  theme_minimal(base_size = 11) + TAG +
  theme(legend.position = "top", legend.text = element_text(size = 9.5),
        legend.key.size = unit(0.35, "cm"), axis.text.y = element_text(size = 10),
        plot.margin = margin(2, 8, 2, 4), panel.grid.minor = element_blank())
p_c

## ---- PANEL d: climate niche (MAT x MAP, Whittaker-style) ----
# sampling plots by class = the clipimate space the DATA cover, over the classic
# Whittaker biome envelopes. No domain shading here: this panel shows where the

# samples sit in clipimate space, not where the model predicts.
site_env2 <- base %>% group_by(plotID) %>%
  summarise(MAT = first(round(mean_mat_site,2)), MAP = first(round(mean_map_site)),
            cond = as.character(first(forest_cond_gf_besnard60)), .groups = "drop") %>%
  filter(cond %in% names(class_cols)) %>%
  mutate(cond = factor(cond, levels = names(class_cols)))

# Whittaker biome boundaries + labels (plotbiomes::Whittaker_biomes; precip cm -> mm)
data("Whittaker_biomes", package = "plotbiomes")
whit <- Whittaker_biomes %>% mutate(MAP_mm = precp_cm * 10)
whit_short <- c(
  "Tundra" = "Tundra", 
  "Boreal forest" = "Boreal\nforest",
  "Temperate seasonal forest" = "Temperate\nseasonal\nforest",
  "Temperate rain forest" = "Temperate\nrainforest",
  "Woodland/shrubland" = "Woodland/\nshrubland",
  "Temperate grassland/desert" = "Temperate\ngrassland/desert",
  "Subtropical desert" = "Subtropical\ndesert",
  "Tropical seasonal forest/savanna" = "Tropical\nseasonal\nforest/savanna",
  "Tropical rain forest" = "Tropical\nrainforest")

# locations of biome labels in MAT, MAP
whit_lab <- whit %>% group_by(biome) %>%
  summarise(MAT = mean(temp_c), MAP = pmin(mean(MAP_mm), 3350), .groups = "drop") %>%
  mutate(MAT = pmax(pmin(MAT, 27), -6.5), lab = whit_short[biome]) %>%
  # nudge the crowded dry, low-MAP labels apart (shrubland / grassland-desert / subtropical desert)
  mutate(MAT = case_when(biome=="Temperate grassland/desert" ~ 10, 
                         biome=="Temperate seasonal forest" ~ 15,
                         biome=="Temperate rain forest" ~ 17,
                         biome=="Woodland/shrubland" ~ 17,
                         biome=="Subtropical desert" ~ 25.5, 
                         biome=="Tropical seasonal forest/savanna" ~ 23,
                         biome=="Boreal forest" ~ 1,
                         biome=="Tundra" ~ -8,
                         TRUE ~ MAT),
         MAP = case_when(biome=="Temperate grassland/desert" ~ 110, 
                         biome=="Temperate seasonal forest" ~ 2000,
                         biome=="Temperate rain forest" ~ 2900,
                         biome=="Woodland/shrubland" ~ 1000,
                         biome=="Subtropical desert" ~ 250, 
                         biome=="Tropical seasonal forest/savanna" ~ 2100,
                         biome=="Boreal forest" ~ 1500,
                         biome=="Tundra" ~ 400,
                         TRUE ~ MAP))

p_d <- ggplot() +
  geom_polygon(data = whit, aes(temp_c, MAP_mm, group = biome),
               fill = NA, color = "grey55", linewidth = 0.25) +
  geom_point(data = site_env2, aes(MAT, MAP, color = cond), size = 0.75, alpha = 0.6) +
  geom_text(data = whit_lab, aes(MAT, MAP, label = lab), size = 2.9,
            color = "grey25", lineheight = 0.78, fontface = "italic") +
  scale_color_manual(values = class_cols, guide = "none") +
  coord_cartesian(xlim = c(-8, 29), ylim = c(0, 4100)) + # zoom out so high-MAP (Amazon) points are not truncated
  labs(x = "MAT (°C)", y = "MAP (mm)", tag = "d") +
  theme_minimal(base_size = 12) + TAG +
  theme(plot.margin = margin(2, 10, 2, 6), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11))

p_d

## ---- PANEL e: signal by the 3 classes ----
## (light) so the significant positive mean reads clearly against the spread. --

summ_e <- plot_rate %>% 
  mutate(cond = factor(cond, levels = rev(c("grassland","young forest","old forest")))) %>%
  group_by(cond) %>%
  dplyr::summarise(n = n(), 
                   # interquartile range
                   q25 = weighted.quantile(rate_val, .25, weight), 
                   q75 = weighted.quantile(rate_val, .75, weight),
                   median = weighted.quantile(rate_val, .5, weight),
                   # weighted mean
                   mean = weighted.mean(rate_val, weight),
                   s = list(boot_ci(rate_val, weight)),
                   .groups = "drop") %>%
  mutate(
    lo = map_dbl(s, "lo"), hi = map_dbl(s, "hi"), sig = lo > 0 | hi < 0
  ) %>% dplyr::select(-s)

# summary of all predicted pixel level SOC change at decadal scale
rates_yf_pred <- rbind(summary_youngforest1990, summary_youngforest2000,
                       summary_youngforest2010) %>%
  select(decade, type, region_display, annual_mean)

rates_of_pred <- rbind(summary_oldforest1990, summary_oldforest2000,
                       summary_oldforest2010) %>%
  select(decade, type, region_display, annual_mean)

rates_g_pred <- rbind(summary_grassland1990, summary_grassland2000,
                      summary_grassland2010) %>%
  select(decade, type, region_display, annual_mean)

tic()
predicted_decdal_rates <- rbind(rates_yf_pred,rates_of_pred,rates_g_pred) %>%
  rename(cond=type) %>%
  group_by(cond) %>%
  summarize(mean = mean(annual_mean),
            s = list(boot_ci(annual_mean, rep(1,n()) )),
            q25 = quantile(annual_mean, .25),
            median = quantile(annual_mean, .5),
            q75 = quantile(annual_mean, .75)) %>%
  mutate(
    lo = map_dbl(s, "lo"), hi = map_dbl(s, "hi"), sig = lo > 0 | hi < 0
  ) %>%
  select(-s)
toc()

p_e <- ggplot(summ_e, aes(y = cond)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey55") +
  geom_linerange(aes(xmin = q25, xmax = q75, color = cond), linewidth = 5.2, alpha = 0.35) +
  geom_linerange(aes(xmin = median-0.01, xmax = median+0.01, color = cond), linewidth = 5.2, alpha = 1) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = 1.3, color = "black") +
  geom_point(aes(x = mean, fill = cond), shape = 23, size = 4.8, color = "black", stroke = 0.95) +
  geom_text(aes(x = mean, label = sprintf("%+.2f%s", mean, ifelse(sig, " *", ""))),
            vjust = -1.5, size = 4.0, color = "grey20") +
  
  # Add predicted rate crosses (horizontal line = IQR range)
  geom_linerange(data = predicted_decdal_rates, aes(y = cond, xmin = q25, xmax = q75, color=cond), 
                 linewidth = 2, linetype = "solid", alpha=0.35,
                 position = position_nudge(y = -0.3)) +
  # Vertical line at median 
  geom_linerange(data = predicted_decdal_rates, 
                 aes(y = cond, xmin = median - 0.01, xmax = median + 0.01, color=cond), 
                 linewidth = 2, alpha=1,
                 position = position_nudge(y = -0.3)) +
  # diamond at mean
  geom_errorbarh(data = predicted_decdal_rates,
                 aes(xmin = lo, xmax = hi,), height = 0, linewidth = 0.7, color = "black",
                 position = position_nudge(y = -0.3)) +
  geom_point(data = predicted_decdal_rates,
             aes(x = mean, fill = cond), shape = 23, size = 1, color = "black", stroke = 0.4,
             position = position_nudge(y = -0.3)) +
  
  # predicted mean annotation
  geom_text(data = predicted_decdal_rates, 
            aes(x = q75, label = paste0(sprintf("%+.2f", mean)," *"), color=cond),
            vjust = 0.5, hjust = -0.1, size = 3.5,
            position = position_nudge(y = -0.3)) +
  
  # Add "Observed" annotation
  geom_text(data = summ_e %>% filter(cond == "grassland"),
            aes(x = q25, y = cond, label = "Observed"),
            vjust = -1.2, hjust = 0, size = 4.2, color = "black") +  
  
  # Add "Predicted" annotation
  geom_text(data = predicted_decdal_rates %>% filter(cond == "grassland"),
            aes(x = q25, y = cond, label = "Predicted"),
            vjust = 0.5, hjust = 1.15, size = 3.2, color = "black",
            position = position_nudge(y = -0.3)) +
  scale_color_manual(values = class_cols, guide = "none") +
  scale_fill_manual(values = class_cols, guide = "none") +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.9))) +
  coord_cartesian(xlim = c(min(summ_e$q25) - 0.3, max(summ_e$q75) + 0.3), clip = "off") +
  labs(x = expression("Top 30 cm SOC rate (Mg C ha"^-1*" yr"^-1*")"), y = NULL, tag = "e") +
  theme_minimal(base_size = 12) + TAG +
  theme(plot.margin = margin(2, 12, 2, 6), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 11))

p_e

## ---- assemble (AAAA / BBCC / DDEE): map (a) full-width banner on top; then
##   b temporal | c coverage ; d niche | e signal. free() lets the map ignore
##   the lower panels' axis-label margins and fill the full width. ----
design <- "AAAA\nBBCC\nDDEE"
fig1 <- free(p_a, type = "space", side = "lr") + p_b + p_c + p_d + p_e +
  plot_layout(design = design, heights = c(2.5, 1, 1.2))

ggsave("./figures/Fig1_2026September.png", fig1, width = 11.5, height = 11, dpi = 400, bg = "white")

