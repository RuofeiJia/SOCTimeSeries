# Figure 2: regional estimates and relave uncertainty
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
library(gridExtra)
library(stringr)
library(patchwork)

# working directory should be set by default to the folder of entire repo (R project)

# load proportion_decade_df
load("./data/proportion_decade_df.rda")
# load prediction results
load("./results/upscaled_regional_summary.RData")

# load countries boundary
# # load world administrative boundaries (see 00_download_prepare_spatial_data.R)
# # https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/?flg=en-us
countries_sf <- read_sf("./data/world-administrative-boundaries/world-administrative-boundaries.shp")
countries_sf <- st_make_valid(countries_sf)
countries_sf <- st_transform(countries_sf, 4326)

#------------ load palette ---------------
# palette for land cover type
type_palette <- c(
  "young forest" = "#c8445b",
  "old forest" = "#4d5ca1",
  "grassland" = "#66aa8b"
)

# palette for regions
region_palette <- c(
  # boreal
  "Alaska" = "cornsilk3",# hex "#cdc8b1"
  "Canada" = "#e6beb6", #"coral4",
  "Northern Europe" = "lightyellow2", # hex "#EEEED1"
  "European Russia" = "burlywood", # hex "#DEB887"
  "Asian Russia" = "#e0dcc6", #"lightgoldenrod4",
  # temperate 
  "Europe" = "#f0cfde", #"hotpink3",
  "Continental United States" = "#fad9d0", #"salmon2",
  "China" = "#ffecef", #"pink",
  "Japan and Korea" = "#ffdfa3", #"orange3",
  "Australia" = "#fce4cf", # "sandybrown",
  "New Zealand" = "#ffe4b3", # "orange",
  "Other temperate" = "#fad9d9", #"lightcoral",
  "Others" = "#ffebff", #"plum1", 
  # tropical
  "Africa" = "#faf5d9", #"lightgoldenrod",
  "Mexico and Central America" = "#c7ff90", #"chartreuse4",
  "South America" = "#defade", #"lightgreen",
  "Southeast Asia"= "#e1f0c2", #"yellowgreen",
  "South Asia" = "#f0eed5" #"khaki3" #,
  #NA = "gray"
)

 
#-----------------Figure 2a base map ----------------------------
# 3a: region base map. Will put regional estimates on top in illustrator
regional_base <- ggplot() +
  #geom_sf(data=countries_sf, fill=NA, col=NA) + # plot background with sf
  # geom_tile(data=proportion_decade_df, aes(x=x, y=y), # plot background (greenland with tiles)
  #           color="gray",fill="gray", alpha=1) +
  geom_tile(data=proportion_decade_df[!is.na(proportion_decade_df$pan_region_new2),], 
            aes(x=x, y=y, color=pan_region_new2, fill=pan_region_new2), alpha=1) +
  scale_fill_manual(values = region_palette) +
  scale_color_manual(values = region_palette) +
  labs(x=NULL, y=NULL,fill=NULL, tags="A") + 
  # scale_x_continuous(expand=c(0,0)) + 
  # scale_y_continuous(expand=c(0,0)) +
  coord_fixed() + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.x = element_blank(),    # Remove x-axis text
        axis.text.y = element_blank(),    # Remove y-axis text
        axis.ticks.x = element_blank(),   # Remove x-axis ticks
        axis.ticks.y = element_blank(),   # Remove y-axis ticks,
        panel.border = element_blank(),
        panel.background = element_rect(fill='white', color=NA), 
        plot.background = element_rect(fill='white', color=NA),
        plot.tag = element_text(face = "bold",size = 10),
        plot.tag.position = c(0.01, 1),
        plot.margin=margin(0, 0, 0, 0)
  )

regional_base

# save
ggsave("./Fig2a_base.png", plot = regional_base, bg = "white", dpi = 600, width = 0.8*183 / 25.4, height = 60 / 25.4, units = "in")


#--------------------- 2b: uncertainty by latitude -----------------------------
load("./results/upscaled_pixel_summary.RData")

# set function that summarize pixel results by latitude as mean, sd, total
sum_by_lat <- function(layer_df, var_name="se_annual", type_str="young forest") {
  # layer_df should have column x, y
  # generate summarizing dataset
  sum_df_type <- layer_df %>% group_by(y) %>%
    dplyr::summarize(
      mean = mean(!!sym(var_name), na.rm=T),
      median = median(!!sym(var_name), na.rm=T),
      sd = sd(!!sym(var_name), na.rm=T)
    ) %>%
    dplyr::mutate(type=type_str)
  
  return(sum_df_type)
}

# summarize bayesian standard deviation
sum_sd_yf <- sum_by_lat(summary_youngforest1992_2020[summary_youngforest1992_2020$proportion>0,], 
                        var_name="annual_sd", type_str="young forest")
sum_sd_of <- sum_by_lat(summary_oldforest1992_2020[summary_oldforest1992_2020$proportion>0,], 
                        var_name="annual_sd", type_str="old forest")
sum_sd_g <- sum_by_lat(summary_grassland1992_2020[summary_grassland1992_2020$proportion>0,], 
                       var_name="annual_sd", type_str="grassland")

# combine into one df
sum_sd <- rbind(sum_sd_yf,sum_sd_of,sum_sd_g) %>%
  dplyr::mutate(type=factor(type, levels=c("young forest", "old forest", "grassland"))) %>%
  group_by(type) %>%
  dplyr::mutate(
    #relative_mean_sd = mean/max(mean, na.rm=T),
    relative_median_sd = median/max(median, na.rm=T),
  )

# plot
p_sd_latitude <- ggplot(sum_sd) + 
  geom_path(aes(x=relative_median_sd, y=y, group=type, color=type) ) + 
  scale_color_manual(values = type_palette) + 
  scale_y_continuous(breaks=c(-50, 0, 50, 80), labels = ~ paste0(.x, "\u00B0")) +
  scale_x_continuous(breaks=c(0, 1),limits=c(0, 1)) + 
  labs(x="relative\nuncertainty", y=NULL, title=NULL,tags="B") + # y="latitude" # , 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(color = "black"),
        legend.title=element_blank(), legend.position="none",
        plot.tag = element_text(face = "bold",size = 10),
        plot.tag.position = c(-0.01, 1),
        plot.margin = margin(5, 10, 0, 5),
        axis.title = element_text(size = 9)
        ) # top, right, bottom, left, default all 5.5

p_sd_latitude

# save
ggsave("./Fig2b_panel.png", plot = p_sd_latitude, bg = "white", dpi = 600, width = 0.2*183 / 25.4, height = 60 / 25.4, units = "in")

#------------------------ Figure2 legend----------------------------------------
# Create data frames for regions legend
region_df <- data.frame(
  region = names(region_palette),
  color = unname(region_palette),
  biome = c(rep("Boreal", 5), rep("Temperate", 7), "Other", rep("Tropical", 5)),
  x = 1
) %>%
  group_by(biome) %>%
  mutate(y = -row_number()) %>%
  ungroup()

# create dataframe for land cover type legend
type_df <- data.frame(
  type = str_to_title(names(type_palette)[1:3]),
  color = unname(type_palette)[1:3],
  x = 1, y=c(-1,-2,-3)
)

# Plot legend for each biome
boreal_legend <- ggplot(region_df[region_df$biome=="Boreal",]) +
  geom_rect(aes(xmin=x-0.7, xmax=x-0.1, ymin=y, ymax=y+0.6, fill=color)) +
  geom_text(aes(x=x,y=y,label = region), hjust=0, vjust=0, size=3.5) + 
  geom_text(aes(x=0.3, y=0, label = "Boreal"), hjust=0, vjust=0, size=3.5, fontface="bold") + 
  scale_fill_identity() +
  xlim(c(0.3,6)) +  ylim(c(-8,1)) +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 1,expand=T, clip = "on") +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(0,0,0,0)
  )

temperate_legend <- ggplot(region_df[region_df$biome=="Temperate",]) +
  geom_rect(aes(xmin=x-0.7, xmax=x-0.1, ymin=y, ymax=y+0.6, fill=color)) +
  geom_text(aes(x=x,y=y,label = region), hjust=0, vjust=0, size=3.5) + 
  geom_text(aes(x=0.3, y=0, label = "Temperate"), hjust=0, vjust=0, size=3.5, fontface="bold") + 
  scale_fill_identity() +
  xlim(c(0.3,9)) +  ylim(c(-8,1)) +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 1, expand=T, clip = "on") +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(0,0,0,0)
  )

tropical_other_legend <- ggplot(region_df[region_df$biome=="Tropical",]) +
  geom_rect(aes(xmin=x-0.7, xmax=x-0.1, ymin=y, ymax=y+0.6, fill=color)) +
  geom_text(aes(x=x,y=y,label = region), hjust=0, vjust=0, size=3.5) + 
  geom_text(aes(x=0.3, y=0, label = "Tropical"), hjust=0, vjust=0, size=3.5, fontface="bold") + 
  scale_fill_identity() +
  xlim(c(0.3,10)) +  ylim(c(-8,1)) +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 1, expand=T, clip = "on") +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(0,0,0,0)
  ) + # other
  geom_text(data=region_df[region_df$biome=="Other",],
            aes(x=0.3, y=-6, label = "Other"), hjust=0, vjust=0, size=3.5, fontface="bold") + 
  geom_rect(data=region_df[region_df$biome=="Other",],
            aes(xmin=x-0.7, xmax=x-0.1, ymin=-7, ymax=-7+0.6, fill=color)) +
  geom_text(data=region_df[region_df$biome=="Other",],
            aes(x=x,y=-7,label = region), hjust=0, vjust=0, size=3.5)

# Plot for type legend with rectangular symbols and labels
type_legend <- ggplot(type_df) +
  geom_rect(aes(xmin=x-0.7, xmax=x-0.1, ymin=y, ymax=y+0.6, fill=color)) +
  geom_text(aes(x=x,y=y,label = type), hjust=0, vjust=0, size=3.5) + 
  geom_text(aes(x=0.3, y=0, label = "Land Cover"), hjust=0, vjust=0, size=3.5, fontface="bold") + 
  scale_fill_identity() +
  xlim(c(0.3,5)) +  ylim(c(-8,1)) +
  labs(x = NULL, y = NULL) +
  coord_fixed(ratio = 1, expand=T, clip = "on") +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(0,0,0,0)
  )
type_legend

# Combine the legends
fig2_legend <- boreal_legend |temperate_legend | tropical_other_legend | type_legend
fig2_legend

# save legend
ggsave("./Fig2_legend.png", plot = fig2_legend, 
       bg = "white", dpi = 600, width = 183 / 25.4, height = 80 / 25.4, units = "in")

#-----------------Figure 3a: regional estimates-----------------------------------

# filter to extent==all, decade=1992-2020
regional_combined_bt_all_decades <- pan_region_CI %>% 
  filter(decade=="1992-2020" & !is.na(pan_region_new2)) %>%
  mutate(type = factor(type, levels=c("young forest", "old forest", "grassland")),
         extent="all" # dummy for plotting
         ) %>%
  dplyr::select(-starts_with("delta")) %>%
  dplyr::mutate(across(annual_mean:annual_97.5, ~ .* 1000))# convert all unit from PgC/yr to TgC/yr

# find ylim_val from max and min value of regional numbers
min(regional_combined_bt_all_decades$annual_mean) # -104.5499
max(regional_combined_bt_all_decades$annual_mean) # 736.0494

# set theme
theme_set(theme_minimal(base_family = "Helvetica", base_size = 7))

# function that plots annual SOC change for each region (1992-2020)
plot_annualChange <- function(region_str="Africa", df=regional_combined_bt_all_decades, 
                              palette=type_palette, ylim_val=c(-145,780)) {
  p <- ggplot(df[df$pan_region_new2==region_str,],
              aes(x = extent, y = annual_mean, fill=type)) +
    scale_fill_manual(values = palette) +
    geom_bar(stat = "identity", position = "dodge", show.legend = F) +
    geom_text(aes(label = round(annual_mean,0), # add value numbers
                  y = annual_mean),
              position = position_dodge(width = 0.9),
              vjust = ifelse(df[df$pan_region_new2 == region_str, ]$annual_mean > 0, -0.5, 1.5),
              size = 3) + # adjust size for printed text size
    geom_segment(aes(x = 1-0.45, # add black line in y=0
                     xend = 1+0.45,
                     y = 0,
                     yend = 0),
                 color = "black") +
    #geom_errorbar(aes(ymin = annual_2.5, ymax = annual_97.5), width = 0.2, position = position_dodge(0.9)) +
    labs(title = NULL, x = NULL, y =NULL) +
    ylim(ylim_val) + 
    theme(text = element_text(family = "Helvetica"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill='transparent', color=NA), 
          plot.background = element_rect(fill='transparent', color=NA),
          axis.line = element_blank(), axis.ticks=element_blank(),
          axis.text = element_blank())
  
  return(p)
}

# plot each regions
unique(regional_combined_bt_all_decades$pan_region_new2)

change_africa <- plot_annualChange("Africa")
change_alaska <- plot_annualChange("Alaska")
change_asianrussia <- plot_annualChange("Asian Russia")
change_australia <- plot_annualChange("Australia")
change_canada <- plot_annualChange("Canada")
change_china <- plot_annualChange("China")
change_us <- plot_annualChange("Continental United States")
change_europe <- plot_annualChange("Europe")
change_eurussia <- plot_annualChange("European Russia")
change_japankorea <- plot_annualChange("Japan and Korea")
change_mexico <- plot_annualChange("Mexico and Central America")
change_nz <- plot_annualChange("New Zealand")
change_northeu <- plot_annualChange("Northern Europe")
change_othertemp <- plot_annualChange("Other temperate")
change_others <- plot_annualChange("Others")
change_southamerica <- plot_annualChange("South America")
change_southasia <- plot_annualChange("South Asia")
change_southeastasia <- plot_annualChange("Southeast Asia")

#
ggsave("./Fig2_africa.png", change_africa, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_alaska.png", change_alaska, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_asianrussia.png", change_asianrussia, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_australia.png", change_australia, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_canada.png", change_canada, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_china.png", change_china, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_us.png", change_us, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_europe.png", change_europe, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_eurussia.png", change_eurussia, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_japankorea.png", change_japankorea, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_mexico.png", change_mexico, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_nz.png", change_nz, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_northeu.png", change_northeu, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_othertemp.png", change_othertemp, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_others.png", change_others, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_southamerica.png", change_southamerica, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_southasia.png", change_southasia, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")
ggsave("./Fig2_southeastasia.png", change_southeastasia, bg="transparent", dpi = 600, width=0.8, height=1.65, units="in")

# figure is assembled and exported in Powerpoint
