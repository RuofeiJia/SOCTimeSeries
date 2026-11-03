# Figure 3: Regional decadal SOC sink for each land cover in half violins on a world map
rm(list = ls(all = TRUE))

suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
})

sf::sf_use_s2(FALSE)

# Define Robinson map projection
robinson_proj <- "+proj=robin +datum=WGS84"

# Define color palette for land cover classes
land_cover_colors <- c(
  "grassland" = "#E1A02A",
  "young forest" = "#3C8DC4",
  "old forest" = "#8064B0"
)

land_cover_order <- names(land_cover_colors)

# Create color gradients for half-violin fill (sink/source)
# Green gradient for positive values (sinks: carbon uptake)
sink_gradient <- colorRampPalette(c("#F4FAF7", "#4E9C82"))(101)

# Tan/sand gradient for negative values (sources: carbon release)
source_gradient <- colorRampPalette(c("#FBF5EA", "#D3A867"))(101)

# Function: assign color to half-violin based on value
# Positive values (sinks) get green; negative values (sources) get tan
assign_sink_source_color <- function(value, reference_value = 120, ease = 0.8) {
  # Normalize value relative to reference and apply power curve
  gradient_index <- round(pmin(abs(value) / reference_value, 1) ^ ease * 100) + 1
  
  # Select from appropriate gradient
  ifelse(value >= 0, sink_gradient[gradient_index], source_gradient[gradient_index])
}


# Remap Pan et al. regional categories to display regions
# Display regions combine multiple sub-regions:
# Europe = Europe + Northern Europe
# East Asia = China + Japan/Korea
# Russia = European Russia + Asian Russia
# Oceania = Australia + New Zealand
# Central & West Asia = Other temperate
# NOTE: Alaska and Southeast Asia are intentionally NOT mapped (<15% of their 
# applicable area is in-domain). They are reported in Table Sx.

region_mapping <- c(
  "Canada" = "Canada",
  "Continental United States" = "United States",
  "Mexico and Central America" = "Mexico & Central America",
  "Russia" = "Russia",
  "Europe" = "Europe",
  "East Asia" = "East Asia",
  "South Asia" = "South Asia",
  "Africa" = "Africa",
  "South America" = "South America",
  "Oceania" = "Oceania",
  "Central & West Asia" = "Central & West Asia"
)

# Helper function: get all sub-regions that map to a display region
get_sub_regions <- function(display_region) {
  names(region_mapping)[region_mapping == display_region]
}


# -------------------- Load data and set up ------------------------------------

# Load regional estimates 
load("./results/find_CI_bayesian_cap2_exclude_ts_extent_timeseries_new.RData")

# Load regional posterior estimates
load("./results/find_CI_bayesian_cap2_exclude_ts_extent_numbers_timeseries_new.RData")

# Load pixel summary
load("./results/find_CI_bayesian_cap2_exclude_ts_extent_pixel_summary_timeseries_new.RData")


# Posterior estimates at regional level: 
# sum_delta_combined  # contains posterior draws (delta_total_* columns)

# Column names for all posterior predictions
posterior_draw_cols <- grep("^delta_total", names(sum_delta_combined), value = TRUE)

# Year spans for each decade
year_span <- c("1990" = 8, "2000" = 10, "2010" = 10)

# List of decades
decades <- c("1990", "2000", "2010")

# Land cover class name mapping
land_cover_names <- c(
  grassland = "grassland",
  young_forest = "youngforest",
  old_forest = "oldforest"
)


# Function: extract posterior draws for a region/land cover/decade combination
get_posterior_draws <- function(display_region, land_cover, decade) {
  
  # Get sub-regions that map to this display region
  sub_region_names <- get_sub_regions(display_region)
  
  # Find rows matching region, land cover, and decade
  row_indices <- which(
    sum_delta_combined$region_display %in% sub_region_names &
      sum_delta_combined$type == land_cover &
      sum_delta_combined$decade == decade
  )
  
  # Return NULL if no data
  if (length(row_indices) == 0) {
    return(NULL)
  }
  
  # Sum posterior draws across all sub-regions (posterior totals add)
  # Convert from Petagram to Teragram (×1000)
  # Convert to annual rate (divide by year span in years)
  annual_draws <- colSums(
    as.matrix(sum_delta_combined[row_indices, posterior_draw_cols, drop = FALSE])
  ) / year_span[decade] * 1000
  
  return(annual_draws)
}

# List of display regions to plot
display_regions <- c(
  "Canada",
  "United States",
  "Mexico & Central America",
  "South America",
  "Africa",
  "Europe",
  "Russia",
  "Central & West Asia",
  "East Asia",
  "South Asia",
  "Oceania"
)


#------------- build in-domain map and region center locations -----------------
# get predicted pixels to overlay 
predicted_pixels <- rbind(summary_grassland1992_2020,
                          summary_youngforest1992_2020,
                          summary_oldforest1992_2020) %>%
  distinct(x,y) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = robinson_proj) %>%
  st_coordinates() %>%
  as.data.frame()

# get centered coordinates for each display region
region_centroids <- rbind(summary_grassland1992_2020,
                          summary_youngforest1992_2020,
                          summary_oldforest1992_2020 ) %>%
  select(x, y, region_display) %>%
  distinct() %>%
  mutate(
    display_region = region_mapping[region_display]
  ) %>%
  filter(!is.na(display_region), display_region %in% display_regions) %>%
  group_by(display_region) %>%
  summarise(
    lon = mean(x),
    lat = mean(y),
    .groups = "drop"
  )

# Convert to spatial object and project to Robinson
region_sf <- st_as_sf(
  region_centroids,
  coords = c("lon", "lat"),
  crs = 4326
) %>%
  st_transform(crs = robinson_proj)

centroids <- st_coordinates(region_sf) %>%
  as.data.frame() %>%
  bind_cols(display_region = region_centroids$display_region) %>%
  rename(X = X, Y = Y) %>%
  column_to_rownames("display_region")

# Save original centroids (for leader line endpoints)
centroids_original <- centroids

# Manual nudges to avoid cluster collisions
# North America diagonal (Canada / US / Mexico) is fanned out;
# Mexico is taken offshore to reduce overlap
cluster_nudges <- tribble(
  ~display_region, ~dx_meters, ~dy_meters,
  "Canada", 1.30e6, 0.85e6,
  "United States", 0.10e6, -0.65e6,
  "Mexico & Central America", -1.55e6, -1.75e6,
  "Europe", -1.00e6, 0.15e6,
  "Russia", 1.10e6, 0.6e6,
  "Central & West Asia", -0.20e6, -1.17e6,
  "East Asia", 1.5e6, -0.55e6,
  "South Asia", -0.55e6, -2.05e6,
  "Africa", -0.20e6, 0.3e6,
  "South America", 0.00e6, -0.30e6,
  "Oceania", -0.30e6, 0.23e6
)

# Apply nudges to centroids
centroids$X <- centroids$X + 
  cluster_nudges$dx_meters[match(rownames(centroids), cluster_nudges$display_region)]

centroids$Y <- centroids$Y + 
  cluster_nudges$dy_meters[match(rownames(centroids), cluster_nudges$display_region)]

#------------------- plotting parameters ---------------------------------------
# Glyph spacing parameters
class_x_offset <- 13e5        # spacing between land cover classes
decade_x_offset <- 4.0e5      # spacing between decades
max_violin_width <- 2.7e5     # maximum half-violin width

# Offsets for each class and decade
class_offsets <- c(
  "grassland" = -class_x_offset,
  "young forest" = 0,
  "old forest" = class_x_offset
)

decade_offsets <- c(
  "1990" = -decade_x_offset,
  "2000" = 0,
  "2010" = decade_x_offset
)

# Collect all posterior draws to determine global color saturation
all_posterior_values <- unlist(lapply(
  display_regions,
  function(region) {
    lapply(
      land_cover_order,
      function(land_cover) {
        lapply(
          decades,
          function(decade) {
            get_posterior_draws(region, land_cover, decade)
          }
        )
      }
    )
  }
))

# Reference value for color saturation (90th percentile of absolute values)
# Colors saturate near this value
reference_value_for_color <- as.numeric(quantile(abs(all_posterior_values), 0.90))

# Envelope size for glyphs (fixed maximum height)
envelope_height <- 1.4e6

# Minimum certainty weight (noisy regions shrink to this fraction of full envelope)
min_certainty_weight <- 0.5


# --------------- Calculate per-region y axis scaling --------------------------
# get posterior draw for each display region, each land cover 
get_glyph_values <- function(display_region) {
  lapply(
    land_cover_order,
    function(land_cover) {
      lapply(
        decades,
        function(decade) {
          get_posterior_draws(display_region, land_cover, decade)
        }
      )
    }
  )
}

# Calculate reach (99th percentile of absolute values per region)
reach <- setNames(
  sapply(
    display_regions,
    function(region) {
      as.numeric(quantile(abs(unlist(get_glyph_values(region))), 0.99))
    }
  ),
  display_regions
)

# Calculate signal-to-noise ratio per region
# (mean of |median| / sd across all glyphs in region)
signal_to_noise_ratio <- setNames(
  sapply(
    display_regions,
    function(region) {
      mean(
        unlist(lapply(
          get_glyph_values(region),
          function(class_values) {
            lapply(
              class_values,
              function(draws) {
                if (is.null(draws)) {
                  NA
                } else {
                  abs(median(draws)) / sd(draws)
                }
              }
            )
          }
        )),
        na.rm = TRUE
      )
    }
  ),
  display_regions
)

# Convert signal-to-noise ratio to size weight: [min_certainty_weight, 1]
# values: certainty weight to decide y axis size, names: display regions
certainty_weight <- setNames(
  min_certainty_weight + 
    (1 - min_certainty_weight) * 
    (signal_to_noise_ratio - min(signal_to_noise_ratio)) / 
    (max(signal_to_noise_ratio) - min(signal_to_noise_ratio)),
  display_regions
)

# Calculate per-region y-axis scale (that maps sink values to region-specific y axis size)
# (scales 99th percentile to fit within envelope, adjusted by certainty)
region_y_scale <- setNames(
  envelope_height * certainty_weight / reach,
  display_regions
)

cat("Per-region certainty weight (1 = full size, ", min_certainty_weight, 
    " = most zoomed out):\n", sep = "")
print(round(sort(certainty_weight, decreasing = TRUE), 2))

# Helper: round to numbers (1, 2, 5, 10, 20, 50, 100, ...)
round_number <- function(z) {
  rounded_numbers <- c(1, 2, 5, 10, 20, 50, 100, 200, 500)
  rounded_numbers[max(which(rounded_numbers <= max(z, 1)))]
}


# ----------------- Region labels and leader lines -----------------------------

# Specify label position (left or right) for each region
label_side <- c(
  Canada = "L",
  "United States" = "L",
  "Mexico & Central America" = "R",
  "South America" = "L",
  Africa = "L",
  Europe = "L",
  Russia = "R",
  "Central & West Asia" = "L",
  "East Asia" = "R",
  "South Asia" = "R",
  Oceania = "L"
)

# Display name (spell out abbreviated names)
label_display_names <- c(
  "Mexico & Central America" = "Mexico &\nCentral America"
)

# Calculate label positions
label_positions <- do.call(rbind, lapply(
  display_regions,
  function(region) {
    center_x <- centroids[region, "X"]
    center_y <- centroids[region, "Y"]
    
    # region label text: spell out C. america (this can be reduced later)
    display_name <- if (region %in% names(label_display_names)) {
      label_display_names[[region]]
    } else { region}
    
    # region label position ()
    if (label_side[region] == "L") {
      data.frame(
        x = center_x - class_x_offset - decade_x_offset - 3.2e5,
        y = center_y, hjust = 1, label = display_name)
    } else {
      data.frame(
        x = center_x + class_x_offset + decade_x_offset + max_violin_width + 2.0e5,
        y = center_y, hjust = 0,label = display_name)
    }
  }
))

# Leader lines: connect nudged positions to true geographic centroids
# (for regions that sit offshore)
offshore_regions <- c("Mexico & Central America", "South Asia", "Central & West Asia")

leader_lines <- do.call(rbind, lapply(
  offshore_regions,
  function(region) {
    data.frame(
      x = centroids[region, "X"], y = centroids[region, "Y"],
      x_end = centroids_original[region, "X"], y_end = centroids_original[region, "Y"]
    )
  }
))


# ----------------- Per-region scale ticks and baselines -----------------------

# Calculate scale tick values and positions
scale_ticks <- do.call(rbind, lapply(
  # for each region
  display_regions,
  function(region) {
    # get rounded value for y axis max for each region
    rounded_value <- round_number(reach[region] * 0.9)
    # dataframe for location of y axis and ticks
    data.frame(
      x = centroids[region, "X"] - class_x_offset - decade_x_offset - 1.2e5,
      y_bottom = centroids[region, "Y"],
      y_top = centroids[region, "Y"] + rounded_value * region_y_scale[region],
      tick_value = rounded_value)
  }
))

# location of x axis that connects all glyphs in each region
region_baselines <- data.frame(
  x_start = centroids[display_regions, "X"] - class_x_offset - decade_x_offset - 0.4e5,
  x_end = centroids[display_regions, "X"] + class_x_offset + decade_x_offset + max_violin_width + 0.2e5,
  y = centroids[display_regions, "Y"]
)

# Class legend (placed in South Atlantic to save bottom row space)
class_legend <- data.frame(
  land_cover = factor(land_cover_order, levels = land_cover_order),
  x = c(-3.2e6, 0.6e6, 4.4e6),
  y = -5.95e6, # -5.35e6
  label = land_cover_order
)

# ----------------- Build half-violin ------------------------------------------

# Initialize lists to store geometry data
half_violins <- list()      # rectangles for half-violin fill
violin_outlines <- list()   # paths for half-violin edges
credible_intervals <- list() # credible interval line segments
mean_points <- list()     # mean points
trend_lines <- list()       # lines connecting decadal means
geometry_index <- 1

# Loop over regions, land covers, and decades
for (region in display_regions) {
  # extract location and y scale
  region_x <- centroids[region, "X"]
  region_y <- centroids[region, "Y"]
  region_scale <- region_y_scale[region]
  
  # for each land cover
  for (land_cover in land_cover_order) {
    mean_x_coords <- c()   # x-positions of decade means (for trend lines)
    mean_y_coords <- c()   # y-positions of decade means
    decade_posteriors <- list()  # store posterior draws per decade
    
    # for each decade
    for (decade in decades) {
      # Get posterior draws for this combination
      posterior_draws <- get_posterior_draws(region, land_cover, decade)
      # Skip if no data
      if (is.null(posterior_draws)) {next}
      
      # Calculate glyph x-position: region center + class offset + decade offset
      glyph_x <- region_x + class_offsets[land_cover] + decade_offsets[decade]
      
      # Compute density and trim tails below 6% of peak
      density_obj <- density(posterior_draws, n = 140)
      keep_points <- density_obj$y >= 0.06 * max(density_obj$y)
      
      density_x <- density_obj$x[keep_points]
      density_y <- density_obj$y[keep_points]
      
      # Compute half-violin widths (normalized by maximum density, scaled by max width)
      violin_widths <- density_y / max(density_y) * max_violin_width
      n_points <- length(density_x)
      
      # ---- Half-violin rectangles (trapezoids) ----
      # Pair consecutive density points to form trapezoids
      half_violins[[geometry_index]] <- data.frame(
        x_min = glyph_x,
        x_max = glyph_x + violin_widths[-n_points], # list of widths except last one
        y_min = region_y + density_x[-n_points] * region_scale, # list of widths except first one
        y_max = region_y + density_x[-1] * region_scale,
        fill_color = assign_sink_source_color(
          (density_x[-n_points] + density_x[-1]) / 2,
          reference_value = reference_value_for_color
        )
      )
      
      # ---- Half-violin outline ----
      # Vertical line up center, then reverse along edge
      violin_outlines[[geometry_index]] <- data.frame(
        glyph_id = paste(region, land_cover, decade),
        x = c(rep(glyph_x, n_points), rev(glyph_x + violin_widths)),
        y = region_y + c(density_x, rev(density_x)) * region_scale
      )
      
      # ---- Credible intervals and means ----
      quantile_values <- as.numeric(quantile(
        posterior_draws,
        c(0.025, 0.17, 0.5, 0.83, 0.975)
      ))
      mean_value <- as.numeric(mean(posterior_draws))
      
      # Proportion of draws > 0 (probability of sink)
      prob_sink <- mean(posterior_draws > 0)
      
      credible_intervals[[geometry_index]] <- data.frame(
        x = glyph_x,
        y_95_lower = region_y + quantile_values[1] * region_scale,
        y_95_upper = region_y + quantile_values[5] * region_scale,
        y_66_lower = region_y + quantile_values[2] * region_scale,
        y_66_upper = region_y + quantile_values[4] * region_scale,
        land_cover = land_cover
      )
      
      # Mean point: filled if significant (|prob_sink| far from 0.5),
      # hollow otherwise
      is_filled <- prob_sink > 0.975 | prob_sink < 0.025
      
      mean_points[[geometry_index]] <- data.frame(
        x = glyph_x,
        y = region_y + mean_value * region_scale,
        land_cover = land_cover,
        fill_color = if (is_filled) land_cover_colors[land_cover] else "white"
      )
      
      # attach to list for plotting trend lines
      mean_x_coords <- c(mean_x_coords, glyph_x)
      mean_y_coords <- c(mean_y_coords, region_y + mean_value * region_scale)
      decade_posteriors[[decade]] <- posterior_draws
      
      geometry_index <- geometry_index + 1
    }
    
    # for each region x land cover
    # ---- Trend lines connecting decade means ----
    # Connect 1990s → 2000s and 2000s → 2010s
    if (length(mean_x_coords) > 1) {
      decade_names <- names(decade_posteriors)
      
      # for each 2 consecutive decades
      # calculate probability of sink change (decided to not visualize it here, but in table)
      for (j in seq_len(length(mean_x_coords) - 1)) {
        # Posterior probability of increase between consecutive decades
        prob_strengthen <- mean(
          decade_posteriors[[decade_names[j + 1]]] - 
            decade_posteriors[[decade_names[j]]] > 0
        )
        
        # Significant if probability of consistent change ≥ 90% or ≤ 10%
        is_significant_change <- prob_strengthen >= 0.95 | prob_strengthen <= 0.05
        # attach trendline locations to list
        trend_lines[[length(trend_lines) + 1]] <- data.frame(
          x = mean_x_coords[j],
          x_end = mean_x_coords[j + 1],
          y = mean_y_coords[j],
          y_end = mean_y_coords[j + 1],
          land_cover = land_cover,
          region = region,
          from_decade = decade_names[j],
          to_decade = decade_names[j + 1],
          prob_strengthen = prob_strengthen,
          significant_change = is_significant_change
        )
      }
    }
  }
}

# Convert lists to data frames
half_violins <- bind_rows(half_violins)
violin_outlines <- bind_rows(violin_outlines)
credible_intervals <- bind_rows(credible_intervals)
mean_points <- bind_rows(mean_points)
trend_lines <- bind_rows(trend_lines)

# ------------------------ plot ------------------------------------------------
# Load world coastlines and region boundaries
world <- ne_countries(scale = 110, returnclass = "sf") %>%
  st_make_valid()

land <- st_transform(st_union(world), crs = robinson_proj)

# Region boundaries (in data folder) 
region_boundaries <- st_transform(
  st_make_valid(st_read("./data/pan_regions.gpkg", quiet = TRUE)),
  crs = robinson_proj
)

# Build plot
p <- ggplot() +
  # Background: land masses
  geom_sf(data = land, fill = "grey92", color = NA) +
  # In-domain footprint: neutral grey-blue pixels (0.5-degree tiles)
  geom_tile(data = predicted_pixels,
            aes(X, Y),fill = "#A9C0CC",width = 6.5e4,
            height = 6e4,alpha = 0.6 ) +
  # Region boundaries
  geom_sf(data = region_boundaries,
          fill = NA, color = "grey60", linewidth = 0.2) +
  # Leader lines (for offshore clusters)
  geom_segment(data = leader_lines,
               aes(x = x, xend = x_end, y = y, yend = y_end),
               color = "grey55", linewidth = 0.25, linetype = "dashed") +
  # Leader line endpoints
  geom_point(data = leader_lines,
             aes(x = x_end, y = y_end), color = "grey55",
             size = 0.9, shape = 1, stroke = 0.4 ) +
  # x axis (one per region)
  geom_segment(data = region_baselines,
               aes(x = x_start, xend = x_end, y = y, yend = y),
               color = "grey35", linewidth = 0.34) +
  # Per-region scale ticks
  # Main vertical line
  geom_segment(data = scale_ticks,
               aes(x = x, xend = x, y = y_bottom, yend = y_top),
               color = "grey35", linewidth = 0.36) +
  # Horizontal cap at top of tick
  geom_segment(data = scale_ticks,
               aes(x = x - 0.7e5, xend = x, y = y_top, yend = y_top),
               color = "grey35", linewidth = 0.36) +
  # Horizontal cap at bottom of tick
  geom_segment(data = scale_ticks,
               aes(x = x - 0.7e5, xend = x, y = y_bottom, yend = y_bottom),
               color = "grey35", linewidth = 0.36) +
  # Tick value label (Tg C/yr)
  geom_text(data = scale_ticks,
            aes(x = x - 0.95e5, y = y_top, #(y_bottom + y_top) / 2, 
                label = tick_value),
            size = 2.8, color = "grey25", hjust = 1) +
  
  # ---- HALF-VIOLINS (sink/source gradient) ----
geom_rect(data = half_violins,
          aes(xmin = x_min, xmax = x_max, 
              ymin = y_min, ymax = y_max, fill = fill_color)) +
  scale_fill_identity() +
  # Half-violin outlines
  geom_path(data = violin_outlines,
            aes(x = x, y = y, group = glyph_id), 
            color = "grey45", linewidth = 0.16) +
  
  # ---- mean and CI (credible intervals and means, colored by land cover) ---
  # Decadal trend lines (connecting means
  geom_segment(
    data = trend_lines,
    aes(x = x, xend = x_end, y = y, yend = y_end, color = land_cover),
    linewidth = 0.7) +
  # 95% credible interval (thin whisker, grey)
  geom_segment(
    data = credible_intervals,
    aes(x = x, xend = x, y = y_95_lower, yend = y_95_upper),
    color = "grey15", linewidth = 0.5) +
  # 66% credible interval (thick bar, class color)
  geom_segment(data = credible_intervals,
               aes(x = x, xend = x, y = y_66_lower, yend = y_66_upper, 
                   color = land_cover), linewidth = 1.6) +
  # Mean point (filled = significant sink, hollow = not)
  geom_point(data = mean_points,
             aes(x = x, y = y, color = land_cover, fill = fill_color),
             shape = 21, size = 1.5, stroke = 0.55) +
  # Color scale for land cover classes
  scale_color_manual(values = land_cover_colors,
                      name = NULL, breaks = land_cover_order) +
  # Region labels
  geom_text(data = label_positions, aes(x = x, y = y, label = label, hjust = hjust),
            size = 3.2, color = "grey15") +
  
  # ---- LEGEND: sink/source gradient (lower-left ocean) ----
annotate("text", x = -1.43e7, y = -4.1e6, hjust = 0,
         size = 2.8, color = "grey35", label = "half-violin fill") +
  # Sink (green) sample
  annotate("rect",xmin = -1.43e7, xmax = -1.375e7,
           ymin = -4.9e6,ymax = -5.35e6,fill = sink_gradient[85]) +
  annotate("text", x = -1.365e7, y = -5.13e6, hjust = 0,
           size = 2.8, color = "grey25", label = "sink") +
  # Source (sand) sample
  annotate("rect", xmin = -1.43e7, xmax = -1.375e7,
           ymin = -5.7e6, ymax = -6.15e6, fill = source_gradient[85]) +
  annotate("text", x = -1.365e7, y = -5.93e6,
           hjust = 0, size = 2.8, color = "grey25", label = "source") +
  
  # ---- KEY: decade ordering (1990s → 2000s → 2010s, left to right) ----
annotate("text", x = -1.44e7, y = 9.2e6,
         hjust = 0, size = 3.2, color = "grey10",
         label = "Within each land cover: 1990s · 2000s · 2010s  (left → right)" ) +
  
  # ---- CLASS LEGEND (placed in South Atlantic to save bottom row) ----
# Shows: class color + representative glyph + label

# Line segment (representing thick credible interval)
geom_segment(data = class_legend,
             aes(x = x, xend = x + 0.62e6, y = y, yend = y, color = land_cover),
             linewidth = 1.6) +
  # Outer dot (representing mean point outline)
  geom_point(data = class_legend,
             aes(x = x + 0.31e6, y = y), color = "grey15", size = 1.5) +
  # Inner dot (representing mean point fill)
  geom_point(data = class_legend,
             aes(x = x + 0.31e6, y = y, color = land_cover), size = 0.9) +
  # Class label
  geom_text(data = class_legend,
            aes(x = x + 0.8e6, y = y, label = label),
            hjust = 0, size = 3.0, color = "grey15" ) +
  # Set map projection and bounds
  coord_sf(crs = robinson_proj,
           xlim = c(-1.44e7, 1.57e7),ylim = c(-6.4e6, 9.5e6), # original
           #xlim = c(-1.26e7, 1.56e7), ylim = c(-6.2e6, 8.0e6), # same as fig 1a, but annotation clipped
           #xlim=c(-1.65e7, 1.75e7), ylim = c(-6.2e6, 8.0e6), # super big,
           # c(-1.50e7, 1.70e7), ylim = c(-7.2e6, 9.0e6), # to get fig 1a appearance, top clipped
           expand = FALSE) +
  # Minimal theme
  theme_void(base_size = 10) +
  theme(
    panel.background = element_rect(fill = "#EAF0F3", color = NA),
    plot.background = element_rect(fill = "#EAF0F3", color = NA),
    legend.position = "none",
    plot.margin = margin(3, 3, 3, 3)
  )
p

# Save figure
ggsave("./figures/Fig3_2026September.png", p,
       width = 18.0,height = 9.6, units = "cm", dpi = 450, bg = "white")
