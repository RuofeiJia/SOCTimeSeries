# all tables in main text and supplementary material
# climate zone: boreal, temperate, tropical
rm(list = ls(all = TRUE))

# libraries
library(dplyr)
library(brms)

# working directory should be set by default to the folder of entire repo (R project)

#-------------------- load data ------------------------------------------------
# load proportion_decade_df
load("./data/proportion_decade_df.rda")
# load prediction results
load("./results/upscaled_regional_summary.RData")
# load statistical model
load("./results/gamm_brms12.rda")

# assign climate zone to regions
boreal_regions <- c("Canada","Northern Europe","Asian Russia","European Russia", "Alaska")
temperate_regions <- c("Continental United States", "Europe", "China", "Japan and Korea", "Australia", 
                       "New Zealand", "Other temperate", "Others") # 
tropical_regions <- c("Africa", "Mexico and Central America", "South America", "Southeast Asia", "South Asia")

# set symbol to connect lower and upper credible intervals
connect_symbol <- ","


#------------------ Table 1: Total SOC changes by biome ------------------------
# by biome
total_SOC_change_biome <- pan_zone_CI %>%
  dplyr::mutate(
    decade=factor(decade, levels=c(1990,2000,2010,"1992-2020")),
    type=factor(type, levels=c("young forest", "old forest", "grassland")),
    zone=factor(zone, levels=c("boreal", "temperate", "tropical"))) %>%
  dplyr::filter(!is.na(zone)) %>%
  dplyr::select(type, zone, decade, annual_mean, annual_sd, annual_2.5, annual_97.5) %>%
  dplyr::arrange(decade) %>%
  tidyr::pivot_wider(names_from = decade, values_from = contains("annual")) %>%
  dplyr::arrange(type,zone) %>%
  dplyr::mutate(
    summary1990 = paste0(round(annual_mean_1990,2), "±", round(annual_sd_1990,2),
                         " (", round(annual_2.5_1990,2), connect_symbol, round(annual_97.5_1990,2),")"),
    summary2000 = paste0(round(annual_mean_2000,2), "±", round(annual_sd_2000,2),
                         " (", round(annual_2.5_2000,2), connect_symbol, round(annual_97.5_2000,2), ")"),
    summary2010 = paste0(round(annual_mean_2010,2), "±", round(annual_sd_2010,2),
                         " (", round(annual_2.5_2010,2), connect_symbol, round(annual_97.5_2010,2), ")"),
    summary1992_2020 = paste0(round(`annual_mean_1992-2020`,2), "±", round(`annual_sd_1992-2020`,2),
                              " (", round(`annual_2.5_1992-2020`,2), connect_symbol, round(`annual_97.5_1992-2020`,2), ")")
  ) %>%
  dplyr::select(
    type, zone,
    summary1990,summary2000,summary2010,summary1992_2020 )

# by type
total_SOC_change_type <- pan_type_CI %>%
  dplyr::mutate(
    decade=factor(decade, levels=c(1990,2000,2010,"1992-2020")),
    type=factor(type, levels=c("young forest", "old forest", "grassland"))) %>%
  dplyr::select(type, decade, annual_mean, annual_sd, annual_2.5, annual_97.5) %>%
  dplyr::arrange(decade) %>%
  tidyr::pivot_wider(names_from = decade, values_from = contains("annual")) %>%
  dplyr::arrange(type) %>%
  dplyr::mutate(
    zone="all",
    summary1990 = paste0(round(annual_mean_1990,2), "±", round(annual_sd_1990,2),
                         " (", round(annual_2.5_1990,2), connect_symbol, round(annual_97.5_1990,2),")"),
    summary2000 = paste0(round(annual_mean_2000,2), "±", round(annual_sd_2000,2),
                         " (", round(annual_2.5_2000,2), connect_symbol, round(annual_97.5_2000,2), ")"),
    summary2010 = paste0(round(annual_mean_2010,2), "±", round(annual_sd_2010,2),
                         " (", round(annual_2.5_2010,2), connect_symbol, round(annual_97.5_2010,2), ")"),
    summary1992_2020 = paste0(round(`annual_mean_1992-2020`,2), "±", round(`annual_sd_1992-2020`,2),
                              " (", round(`annual_2.5_1992-2020`,2), connect_symbol, round(`annual_97.5_1992-2020`,2), ")")
  ) %>%
  dplyr::select(
    type, zone,
    summary1990,summary2000,summary2010,summary1992_2020 )

# global
total_SOC_change_global <- pan_global_CI %>%
  dplyr::mutate(
    decade=factor(decade, levels=c(1990,2000,2010,"1992-2020"))) %>%
  dplyr::select(decade, annual_mean, annual_sd, annual_2.5, annual_97.5) %>%
  dplyr::arrange(decade) %>%
  tidyr::pivot_wider(names_from = decade, values_from = contains("annual")) %>%
  dplyr::mutate(
    type="all",
    zone="all",
    summary1990 = paste0(round(annual_mean_1990,2), "±", round(annual_sd_1990,2),
                         " (", round(annual_2.5_1990,2), connect_symbol, round(annual_97.5_1990,2),")"),
    summary2000 = paste0(round(annual_mean_2000,2), "±", round(annual_sd_2000,2),
                         " (", round(annual_2.5_2000,2), connect_symbol, round(annual_97.5_2000,2), ")"),
    summary2010 = paste0(round(annual_mean_2010,2), "±", round(annual_sd_2010,2),
                         " (", round(annual_2.5_2010,2), connect_symbol, round(annual_97.5_2010,2), ")"),
    summary1992_2020 = paste0(round(`annual_mean_1992-2020`,2), "±", round(`annual_sd_1992-2020`,2),
                              " (", round(`annual_2.5_1992-2020`,2), connect_symbol, round(`annual_97.5_1992-2020`,2), ")")
  ) %>%
  dplyr::select(
    type, zone,
    summary1990,summary2000,summary2010,summary1992_2020 )

# assemble
total_SOC_change_table1 <- rbind(total_SOC_change_biome, total_SOC_change_type, total_SOC_change_global)
#------------------ Tabl2 2: SOC sink and global carbon budget -----------------
# this table was made manually.
# SLAND_soil is calculated in script 04_upscale.R

#------------------ Table S1: Summary of model coefficient estimates -----------
fixed_coef <- as.data.frame(summary(gamm_brms12)$fixed) %>%
  mutate_at(1:7, round, 3) %>%
  dplyr::select(1:4)
#------------------ Table S2: predictor values for trends in Fig1 --------------
# Code for this table is in the script: Figure1.R
#------------------ Table S3: Total SOC changes by region ----------------------
total_SOC_change_region <- pan_region_CI %>%
  filter(!is.na(pan_region_new2)) %>%
  dplyr::mutate( # add zone
    zone = case_when(
      pan_region_new2 %in% tropical_regions ~ "tropical",
      pan_region_new2 %in% temperate_regions ~ "temperate",
      pan_region_new2 %in% boreal_regions ~ "boreal"
    )
  ) %>%
  dplyr::mutate(
    decade=factor(decade, levels=c(1990,2000,2010,"1992-2020")),
    type=factor(type, levels=c("young forest", "old forest", "grassland")),
    zone=factor(zone, levels=c("boreal", "temperate", "tropical"))) %>%
  dplyr::filter(type!="shrubland") %>%
  dplyr::select(type, zone, pan_region_new2, decade, 
                annual_mean, annual_sd, annual_2.5, annual_97.5) %>%
  dplyr::arrange(decade) %>%
  tidyr::pivot_wider(names_from = decade, values_from = c("annual_mean", "annual_sd", "annual_2.5","annual_97.5")) %>%
  dplyr::mutate(across(annual_mean_1990:`annual_97.5_1992-2020`, ~ .* 1000)) %>% # convert all unit from PgC/yr to TgC/yr
  dplyr::mutate( # rename region, format results
    region=pan_region_new2,
    summary1990 = paste0(round(annual_mean_1990,2), "±", round(annual_sd_1990,2),
                         " (", round(annual_2.5_1990,2), connect_symbol, round(annual_97.5_1990,2),")"),
    summary2000 = paste0(round(annual_mean_2000,2), "±", round(annual_sd_2000,2),
                         " (", round(annual_2.5_2000,2), connect_symbol, round(annual_97.5_2000,2), ")"),
    summary2010 = paste0(round(annual_mean_2010,2), "±", round(annual_sd_2010,2),
                         " (", round(annual_2.5_2010,2), connect_symbol, round(annual_97.5_2010,2), ")"),
    summary1992_2020 = paste0(round(`annual_mean_1992-2020`,2), "±", round(`annual_sd_1992-2020`,2),
                              " (", round(`annual_2.5_1992-2020`,2), connect_symbol, round(`annual_97.5_1992-2020`,2), ")")
  ) %>%
  arrange(type,zone) %>%
  dplyr::select(
    type, zone,region,
    summary1990,summary2000,summary2010,summary1992_2020,`annual_mean_1992-2020` )

# biome level subtotal in TgC
total_SOC_change_biome_Tg <- pan_zone_CI %>%
  dplyr::mutate(
    decade=factor(decade, levels=c(1990,2000,2010,"1992-2020")),
    type=factor(type, levels=c("young forest", "old forest", "grassland")),
    zone=factor(zone, levels=c("boreal", "temperate", "tropical"))) %>%
  dplyr::filter(!is.na(zone)) %>%
  dplyr::select(type, zone, decade, annual_mean, annual_sd, annual_2.5, annual_97.5) %>%
  dplyr::arrange(decade) %>%
  tidyr::pivot_wider(names_from = decade, values_from = contains("annual")) %>%
  dplyr::mutate(across(annual_mean_1990:`annual_97.5_1992-2020`, ~ .* 1000)) %>% # convert all unit from PgC/yr to TgC/yr
  dplyr::arrange(type,zone) %>%
  dplyr::mutate(
    summary1990 = paste0(round(annual_mean_1990,2), "±", round(annual_sd_1990,2),
                         " (", round(annual_2.5_1990,2), connect_symbol, round(annual_97.5_1990,2),")"),
    summary2000 = paste0(round(annual_mean_2000,2), "±", round(annual_sd_2000,2),
                         " (", round(annual_2.5_2000,2), connect_symbol, round(annual_97.5_2000,2), ")"),
    summary2010 = paste0(round(annual_mean_2010,2), "±", round(annual_sd_2010,2),
                         " (", round(annual_2.5_2010,2), connect_symbol, round(annual_97.5_2010,2), ")"),
    summary1992_2020 = paste0(round(`annual_mean_1992-2020`,2), "±", round(`annual_sd_1992-2020`,2),
                              " (", round(`annual_2.5_1992-2020`,2), connect_symbol, round(`annual_97.5_1992-2020`,2), ")")
  ) %>%
  dplyr::select(
    type, zone,
    summary1990,summary2000,summary2010,summary1992_2020 )

# global total in TgC
total_SOC_change_global_Tg <- pan_global_CI %>%
  dplyr::mutate(
    decade=factor(decade, levels=c(1990,2000,2010,"1992-2020"))) %>%
  dplyr::select(decade, annual_mean, annual_sd, annual_2.5, annual_97.5) %>%
  dplyr::arrange(decade) %>%
  tidyr::pivot_wider(names_from = decade, values_from = contains("annual")) %>%
  dplyr::mutate(across(annual_mean_1990:`annual_97.5_1992-2020`, ~ .* 1000)) %>% # convert all unit from PgC/yr to TgC/yr
  dplyr::mutate(
    type="all",
    zone="all",
    summary1990 = paste0(round(annual_mean_1990,2), "±", round(annual_sd_1990,2),
                         " (", round(annual_2.5_1990,2), connect_symbol, round(annual_97.5_1990,2),")"),
    summary2000 = paste0(round(annual_mean_2000,2), "±", round(annual_sd_2000,2),
                         " (", round(annual_2.5_2000,2), connect_symbol, round(annual_97.5_2000,2), ")"),
    summary2010 = paste0(round(annual_mean_2010,2), "±", round(annual_sd_2010,2),
                         " (", round(annual_2.5_2010,2), connect_symbol, round(annual_97.5_2010,2), ")"),
    summary1992_2020 = paste0(round(`annual_mean_1992-2020`,2), "±", round(`annual_sd_1992-2020`,2),
                              " (", round(`annual_2.5_1992-2020`,2), connect_symbol, round(`annual_97.5_1992-2020`,2), ")")
  ) %>%
  dplyr::select(
    type, zone,
    summary1990,summary2000,summary2010,summary1992_2020 )

#------------------ Table S4: SOC change per area by region --------------------
SOC_change_per_area_region <- pan_region_CI %>%
  filter(!is.na(pan_region_new2)) %>%
  dplyr::mutate( # add zone
    zone = case_when(
      pan_region_new2 %in% tropical_regions ~ "tropical",
      pan_region_new2 %in% temperate_regions ~ "temperate",
      pan_region_new2 %in% boreal_regions ~ "boreal",
      TRUE ~ "other"
    )
  ) %>%
  dplyr::mutate(
    decade=factor(decade, levels=c(1990,2000,2010,"1992-2020")),
    type=factor(type, levels=c("young forest", "old forest", "grassland")),
    zone=factor(zone, levels=c("boreal", "temperate", "tropical"))) %>%
  dplyr::select(type, zone, pan_region_new2, decade, 
                annual_mean_perha, annual_sd_perha, annual_2.5_perha, annual_97.5_perha) %>%
  dplyr::arrange(decade) %>%
  tidyr::pivot_wider(names_from = decade, values_from = c("annual_mean_perha", "annual_sd_perha", "annual_2.5_perha","annual_97.5_perha")) %>%
  dplyr::mutate( # rename region, format results
    region=pan_region_new2,
    summary1990 = paste0(round(annual_mean_perha_1990,2), "±", round(annual_sd_perha_1990,2),
                         " (", round(annual_2.5_perha_1990,2), connect_symbol, round(annual_97.5_perha_1990,2),")"),
    summary2000 = paste0(round(annual_mean_perha_2000,2), "±", round(annual_sd_perha_2000,2),
                         " (", round(annual_2.5_perha_2000,2), connect_symbol, round(annual_97.5_perha_2000,2), ")"),
    summary2010 = paste0(round(annual_mean_perha_2010,2), "±", round(annual_sd_perha_2010,2),
                         " (", round(annual_2.5_perha_2010,2), connect_symbol, round(annual_97.5_perha_2010,2), ")"),
    summary1992_2020 = paste0(round(`annual_mean_perha_1992-2020`,2), "±", round(`annual_sd_perha_1992-2020`,2),
                              " (", round(`annual_2.5_perha_1992-2020`,2), connect_symbol, round(`annual_97.5_perha_1992-2020`,2), ")")
  ) %>%
  arrange(type,zone) %>%
  dplyr::select(
    type, zone,region,
    summary1990,summary2000,summary2010,summary1992_2020)

# biome level subtotal
SOC_change_per_area_biome <- pan_zone_CI %>%
  dplyr::mutate(
    decade=factor(decade, levels=c(1990,2000,2010,"1992-2020")),
    type=factor(type, levels=c("young forest", "old forest", "grassland", "shrubland")),
    zone=factor(zone, levels=c("boreal", "temperate", "tropical"))) %>%
  dplyr::filter(!is.na(zone)) %>%
  dplyr::select(type, zone, decade, annual_mean_perha, annual_sd_perha, annual_2.5_perha, annual_97.5_perha) %>%
  dplyr::arrange(decade) %>%
  tidyr::pivot_wider(names_from = decade, values_from = contains("annual")) %>%
  dplyr::arrange(type,zone) %>%
  dplyr::mutate(
    summary1990 = paste0(round(annual_mean_perha_1990,2), "±", round(annual_sd_perha_1990,2),
                         " (", round(annual_2.5_perha_1990,2), connect_symbol, round(annual_97.5_perha_1990,2),")"),
    summary2000 = paste0(round(annual_mean_perha_2000,2), "±", round(annual_sd_perha_2000,2),
                         " (", round(annual_2.5_perha_2000,2), connect_symbol, round(annual_97.5_perha_2000,2), ")"),
    summary2010 = paste0(round(annual_mean_perha_2010,2), "±", round(annual_sd_perha_2010,2),
                         " (", round(annual_2.5_perha_2010,2), connect_symbol, round(annual_97.5_perha_2010,2), ")"),
    summary1992_2020 = paste0(round(`annual_mean_perha_1992-2020`,2), "±", round(`annual_sd_perha_1992-2020`,2),
                              " (", round(`annual_2.5_perha_1992-2020`,2), connect_symbol, round(`annual_97.5_perha_1992-2020`,2), ")")
  ) %>%
  dplyr::select(
    type, zone,
    summary1990,summary2000,summary2010,summary1992_2020)

#------------------ Table S5: Area of each land cover by region ----------------
area_by_region <- pan_region_CI %>% 
  dplyr::rename(region=pan_region_new2) %>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::mutate(
    zone = case_when(
      region %in% boreal_regions ~ "boreal",
      region %in% temperate_regions ~ "temperate",
      region %in% tropical_regions ~ "tropical"
    )) %>%
  dplyr::select(type, zone, region, decade, region_area_ha) %>%
  dplyr::mutate(
    decade=factor(decade, levels=c(1990,2000,2010,"1992-2020")),
    type=factor(type, levels=c("young forest", "old forest", "grassland")),
    zone=factor(zone, levels=c("boreal", "temperate", "tropical"))) %>%
  dplyr::arrange(decade) %>%
  tidyr::pivot_wider(names_from = decade, values_from = region_area_ha) %>%
  arrange(type,zone, region)

#------------------ Table S6: Data Source details ------------------------------
# This table was made manually
