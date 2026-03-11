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
load("./results/find_CI_bayesian_cap2_exclude_same_extent.RData")
# load statistical model
load("./results/brms1_withOneTime_37k12chain.rda")

# assign climate zone to regions
boreal_regions <- c("Canada","Northern Europe","Asian Russia","European Russia", "Alaska")
temperate_regions <- c("Continental United States", "Europe", "China", "Japan and Korea", "Australia", 
                       "New Zealand", "Other temperate", "Others")
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
#------------------ Table S1: Summary of model coefficient estimates -----------
fixed_coef <- as.data.frame(summary(brms1)$fixed) %>%
  mutate_at(1:7, round, 3) %>%
  dplyr::select(1:4)
#------------------ Table S2: predictor values for trends in Fig1 --------------
# Code for this table is in the script: Figure1.R
#------------------ Table S3: Area of each land cover by region, 1992-2020 ----------------
area_by_region_predicted <- pan_region_CI %>% 
  dplyr::rename(region=pan_region_new2) %>%
  dplyr::filter(!is.na(region) & decade=="1992-2020") %>%
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
  arrange(type,zone, region) %>%
  rename(`1992-2020_predicted` = `1992-2020`)

# calculate full regions
# (boreal, temperate, tropical region defined at top)
# generate proportion_biome_df with column biome
proportion_zone_df <- proportion_decade_df %>%
  dplyr::mutate(zone = case_when(
    pan_region_new2 %in% boreal_regions ~ "boreal",
    pan_region_new2 %in% temperate_regions ~ "temperate",
    pan_region_new2 %in% tropical_regions ~ "tropical"#,
    #pan_region_new2=="Others" ~ "other"
  ))

area_by_region_all <- proportion_zone_df %>% 
  dplyr::select(-youngforest1992_2020, -oldforest1992_2020, -shrubland2010,
                -youngforest1990, -youngforest2000, -youngforest2010,
                -oldforest1990, -oldforest2000, -oldforest2010, 
                -grassland1990, -grassland2000, -grassland2010) %>%
  dplyr::rename(region = pan_region_new2) %>%
  # pivot long first
  tidyr::pivot_longer(
    cols = c(avg_youngforest1992_2020,avg_oldforest1992_2020,grassland1992_2020),
    names_to = "type_decade",
    values_to = "proportion"
  ) %>% # add type and decade column
  dplyr::mutate(
    type = case_when(
      grepl("youngforest", type_decade) ~ "young forest",
      grepl("oldforest", type_decade) ~ "old forest",
      grepl("grassland", type_decade) ~ "grassland"
    ),
    decade = case_when(
      grepl("1990", type_decade) ~ "1990",
      grepl("2000", type_decade) ~ "2000",
      grepl("2010", type_decade) ~ "2010",
      grepl("1992_2020", type_decade) ~ "1992-2020"
    )) %>% # summarize entire proportion
  dplyr::mutate(
    decade=factor(decade, levels=c(1990,2000,2010,"1992-2020")),
    type=factor(type, levels=c("young forest", "old forest", "grassland")),
    zone=factor(zone, levels=c("boreal", "temperate", "tropical"))) %>%
  group_by(region, type, zone, decade) %>%
  dplyr::summarize(
    region_area_ha = sum(proportion*cellsize_ha),
    .groups = "drop" # to give an ungrouped df
  ) %>%
  tidyr::pivot_wider(
    names_from = decade,
    values_from = region_area_ha
  ) %>%
  arrange(type, zone, region) %>%
  rename(`1992-2020_total` = `1992-2020`)

# make presentable table (if use same proportion for all decade, make % only one column on the right)
area_by_region <- area_by_region_predicted %>%
  left_join(
    area_by_region_all, by=c("type","zone","region")
  )

area_by_biome <- area_by_region_predicted %>%
  left_join(
    area_by_region_all, by=c("type","zone","region")
  ) %>%
  group_by(type, zone) %>%
  # summ area by biome
  dplyr::summarize(
    `1992-2020_predicted` = sum(`1992-2020_predicted`, na.rm = TRUE),
    `1992-2020_total` = sum(`1992-2020_total`, na.rm = TRUE),
    .groups = "drop"
  ) %>% # for rbind with area_by_region
  mutate(region = NA)

area_by_type <- area_by_biome %>%
  group_by(type) %>%
  # summ area by type
  dplyr::summarize(
    `1992-2020_predicted` = sum(`1992-2020_predicted`, na.rm = TRUE),
    `1992-2020_total` = sum(`1992-2020_total`, na.rm = TRUE),
    .groups = "drop"
  ) %>% # for rbind with area_by_region
  mutate(region = NA, zone = NA)

area_by_global <- area_by_type %>%
  # summ area for all
  dplyr::summarize(
    `1992-2020_predicted` = sum(`1992-2020_predicted`, na.rm = TRUE),
    `1992-2020_total` = sum(`1992-2020_total`, na.rm = TRUE),
    .groups = "drop"
  ) %>% # for rbind with area_by_region
  mutate(region = NA, zone = NA, type = NA)

# summarize area predicted in ha and its % of all area
area_table <- rbind(area_by_region, area_by_biome,  area_by_type,  area_by_global) %>%
  # calculate percentage of predicted in all area
  dplyr::mutate(
    '1992-2020pct' = round(100*`1992-2020_predicted`/`1992-2020_total`)
  )

#------------------ Table S4: Total SOC changes by region ----------------------
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

#------------------ Table S5: SOC change per area by region --------------------
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

#------------------ Table S6: SOC sink and global carbon budget -----------------
# this table was made manually.
#------------------ Table S7: Data Source details ------------------------------
# This table was made manually
