# Figure 1
# instead of predicting temporal trend from average condition of input data,
# predict trends from average condition of input layer (all pixels)
# Mann Kendall tests of trends in figure 1
rm(list = ls(all = TRUE))

# packages
library(brms)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)

# working directory should be set by default to the folder of entire repo (R project)

# load data (object name: df_std)
load("./data/SOC_time_series_for_analysis.rda")
# load input layers
load("./results/prediction_inputs.rda")
# load land use proportion
load("./data/proportion_decade_df.rda")
# load model
load("./results/gamm_brms12.rda")
gamm_brms <- gamm_brms12

# assign climate zone to regions
boreal_regions <- c("Canada","Northern Europe","Asian Russia","European Russia", "Alaska")
temperate_regions <- c("Continental United States", "Europe", "China", "Japan and Korea", "Australia", 
                       "New Zealand", "Other temperate", "Others")
tropical_regions <- c("Africa", "Mexico and Central America", "South America", "Southeast Asia", "South Asia")

# add proportion column to input layers 
input_youngforest1990_first_p <- merge(input_youngforest1990_first, 
                                       proportion_decade_df[,c("x","y","avg_youngforest1992_2020","pan_region_new2")],
                                       by=c("x","y"), all.x=T) %>%
  dplyr::rename(proportion=avg_youngforest1992_2020)

input_oldforest1990_first_p <- merge(input_oldforest1990_first, 
                                     proportion_decade_df[,c("x","y","avg_oldforest1992_2020","pan_region_new2")],
                                     by=c("x","y"), all.x=T) %>%
  dplyr::rename(proportion=avg_oldforest1992_2020)

input_grassland1990_first_p <- merge(input_grassland1990_first, 
                                     proportion_decade_df[, c("x","y","grassland1992_2020","pan_region_new2")],
                                     by=c("x","y"), all.x=T) %>%
  dplyr::rename(proportion=grassland1992_2020)

# add region and biome to input layers
pred_conditions <- rbind(input_youngforest1990_first_p,input_oldforest1990_first_p,
                         input_grassland1990_first_p#, input_shrubland2010_first_p
                         ) %>%
  filter(proportion>0) %>% # keep only pixels with certain land cover type
  filter(!is.na(pan_region_new2)&pan_region_new2!="Others" ) %>%
  mutate(zone = case_when(
    pan_region_new2 %in% boreal_regions ~ "boreal",
    pan_region_new2 %in% temperate_regions ~ "temperate",
    pan_region_new2 %in% tropical_regions ~ "tropical"
  )) %>%
  mutate(cond_biome = paste0(zone, " ", forest_cond_gf_besnard60))

# define color
my_palette_zone <- 
  c(
    "tropical" = "darkgray", #"#e1a5d7",
    "temperate" = "#913f99",
    "boreal" = "#46559e"
  )

# Define custom facet labels
custom_labels <- c(
  "young forest" = "Young forest",
  "old forest" = "Old forest",
  "grassland" = "Grassland"
)

# Function to create the dataset for visualization using brms model
create_dataset_brms <- function(mod, df, cond_name = "cond_biome", year_lim=c(1992,2020)) {
  mod_pred <- data.frame()
  
  for (cond_str in unique(df[[cond_name]])) {
    if (!is.na(cond_str)) {
      # filter df with each land cover type
      df_cond <- df %>% dplyr::filter(!!sym(cond_name) == cond_str)
      
      # define year limits
      year_min <-year_lim[1]
      year_max <- year_lim[2]
      
      # construct prediction data under each land cover type (forest_cond)
      temp_mod_pred <- data.frame(
        year = seq(year_min, year_max, 0.1),
        MAT_cru = weighted.mean(df_cond$MAT_cru, df_cond$proportion, na.rm = TRUE),
        MAP_cru = weighted.mean(df_cond$MAP_cru, df_cond$proportion, na.rm = TRUE),
        forest_cond_gf_besnard60 = unique(df_cond$forest_cond_gf_besnard60),
        std_clay_plo = weighted.mean(df_cond$std_clay_plo, df_cond$proportion, na.rm = TRUE),
        std_n_pct_plo = weighted.mean(df_cond$std_n_pct_plo, df_cond$proportion, na.rm = TRUE),
        std_ph_plo = weighted.mean(df_cond$std_ph_plo, df_cond$proportion, na.rm=TRUE),
        std_elev_plo = weighted.mean(df_cond$std_elev_plo, df_cond$proportion, na.rm = TRUE),
        siteID = 0,
        plotID = 0,
        cond_biome = cond_str,
        zone = unique(df_cond$zone)) %>% 
        mutate(std_MAT_cru = (MAT_cru - mean(df_std$MAT_cru)) / sd(df_std$MAT_cru),
               std_MAP_cru = (MAP_cru - mean(df_std$MAP_cru)) / sd(df_std$MAP_cru),
               std_year = (year - mean(df_std$observation_year)) / sd(df_std$observation_year)
        )
      
      # Generate predictions using posterior_epred
      pred <- posterior_epred(mod, newdata = temp_mod_pred, re_formula = NA)
      pred_summary <- data.frame(
        exp_fit_mean = apply(exp(pred), 2, mean),
        exp_fit_sd = apply(exp(pred), 2, sd),
        exp_fit_2.5 = apply(exp(pred), 2, quantile, probs = 0.025),
        exp_fit_97.5 = apply(exp(pred), 2, quantile, probs = 0.975)
      )
      
      temp_mod_pred <- cbind(temp_mod_pred, pred_summary)
      
      temp_mod_pred <- temp_mod_pred %>%
        dplyr::mutate(
          mean_change = exp_fit_mean - first(exp_fit_mean),
          change_lowCI = exp_fit_2.5 - first(exp_fit_mean),
          change_highCI = exp_fit_97.5 - first(exp_fit_mean)
        )
      
      mod_pred <- rbind(mod_pred, temp_mod_pred)
    }
  }
  
  mod_pred$forest_cond_gf_besnard60 <- factor(mod_pred$forest_cond_gf_besnard60, levels =c("young forest", "old forest","grassland","shrubland"))
  mod_pred$cond_biome <- factor(mod_pred$cond_biome, levels = c("tropical young forest","temperate young forest","boreal young forest", 
                                                                "tropical old forest","temperate old forest", "boreal old forest",
                                                                "tropical grassland","temperate grassland", "boreal grassland"
                                                               ))
  mod_pred$zone <- factor(mod_pred$zone, levels = c("tropical","temperate","boreal"))
  
  return(mod_pred)
}

# Generate the dataset
mod_pred <- create_dataset_brms(gamm_brms, pred_conditions)

# dataframe for panel tags
facet_labels <- data.frame(
  forest_cond_gf_besnard60 = unique(mod_pred$forest_cond_gf_besnard60),
  label = c("A", "B", "C"),  # Adjust based on number of facets
  year = 1992,  # Adjust X position
  mean_change = 45  # Adjust Y position for visibility
)

###### plot data
# set theme
showtext_auto()
theme_set(theme_minimal(base_family = "Helvetica", base_size = 12))

#Plot the data using ggplot (reuse the ggplot code with mod_pred)
pgam_brms <- ggplot() +
  geom_hline(yintercept = 0, linetype = "solid", size = .25) +
  geom_line(data = mod_pred[!grepl("boreal shrubland", mod_pred$cond_biome),],
            aes(x = year, y = mean_change, color = zone), size = 0.75) +
  geom_ribbon(data = mod_pred[!grepl("boreal shrubland", mod_pred$cond_biome),],
              aes(ymin = change_lowCI, ymax = change_highCI, x = year, group = cond_biome, fill = zone),
              alpha = 0.3, color = NA, linetype = 0) +
  scale_fill_manual(values = my_palette_zone, labels = str_to_title(levels(mod_pred$zone))) +
  scale_color_manual(values = my_palette_zone, labels = str_to_title(levels(mod_pred$zone))) +
  ylab("0-30cm SOC stock change (Mg C/ha)") + xlab("Year") +
  facet_wrap(~ forest_cond_gf_besnard60, ncol=3, labeller = labeller(forest_cond_gf_besnard60 = custom_labels)) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(labels = c(-25,0,25), 
                     breaks=c(-25,0,25), limits = c(-27,45)) + 
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "#f0f0f0"),
    strip.text.x = element_text(face = "bold", size = 12),
    strip.text.y = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 9),
    #plot.margin = margin(10, 55, 10, 10),
    legend.title = element_blank(),
    legend.position="bottom") +
  guides(fill = guide_legend(title = NULL, nrow = 1, byrow = TRUE),
         color = guide_legend(title = NULL, nrow = 1, byrow = TRUE)) + 
  geom_text(data = facet_labels, aes(x = year, y = mean_change, label = label), 
            hjust = 0, vjust = 1, size = 10/.pt, fontface = "bold")

showtext_auto(FALSE)

# Display the plot
print(pgam_brms)
# Save the figure with maximum quality for a double column width
ggsave("./Figure1.png", plot = pgam_brms, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 80 / 25.4, units = "in")


#-----------------Table S1: predictor values shown in Figure 1--------------------------

table_predictor_vals <- mod_pred %>%
  dplyr::select(cond_biome, MAT_cru, MAP_cru, std_n_pct_plo, std_clay_plo, std_ph_plo) %>%
  distinct() %>% # transform standardized value back to original
  mutate(
    `Soil N (%)` = std_n_pct_plo*sd(df_std$n_pct_plo) + mean(df_std$n_pct_plo),
    `Soil Clay (%)` = std_clay_plo*sd(df_std$clay_plo) + mean(df_std$clay_plo),
    `Soil pH` = std_ph_plo*sd(df_std$ph_plo) + mean(df_std$ph_plo)
  ) %>%
  rename(Biome=cond_biome, `MAT (°C)`=MAT_cru, `MAP (mm)`=MAP_cru,) %>%
  dplyr::select(-contains("std")) %>%
  mutate_at(2:6, round, 2) # round
