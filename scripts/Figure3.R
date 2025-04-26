# Figure 3. Comparison between data-driven and modeled gross SOC sinks
rm(list = ls(all = TRUE))

library(dplyr)
library(ggplot2)
library(stringr)
library(grid)
library(gridExtra)
library(patchwork)
library(ggbreak) 

# working directory should be set by default to the folder of entire repo (R project)

# load TRENDY data (Unit: PgC)
# total soil carbon (cSoil+cCwd+cLitter) in PgC for each model in TREDNY-v12 at each year
# this is analysis is available at https://github.com/yinonbaron/SOC_change_TRENDYv12/tree/main
trendy_df <- read.csv(file = "./results/TRENDY_v12_global_cSoil_S2_20250114.csv")

# load my result
load("./results/upscaled_regional_summary.RData")

# process global SOC change (1992-2020) to compare
# trendy
trendy_global_1992_2020 <- trendy_df %>% filter(model!="CARDAMOM") %>%
  dplyr::mutate(mean = (X2020 - X1992)/28,
                std=NA, Q2.5=NA, Q5=NA, Q95=NA, Q97.5=NA,
                source = "TRENDY") %>%
  mutate(x_name = sub(".*/", "", model)) %>%
  dplyr::select(source, x_name, mean, std, Q2.5, Q5, Q95, Q97.5)

# data_driven
data_global_1992_2020 <- pan_global_CI %>%
  filter(decade=="1992-2020") %>%
  mutate(source="data",x_name="data") %>%
  rename(
    mean=annual_mean,
    std=annual_sd,
    Q2.5=annual_2.5,
    Q5=annual_5,
    Q95=annual_95,
    Q97.5=annual_97.5
  ) %>%
  dplyr::select(source, x_name, mean, std, Q2.5, Q5, Q95, Q97.5)

data_forest_1992_2020 <- pan_type_CI %>%
  filter(decade=="1992-2020" & grepl("forest", type)) %>%
  mutate(source="data",x_name="data") %>%
  rename(
    mean=annual_mean,
    std=annual_sd,
    Q2.5=annual_2.5,
    Q5=annual_5,
    Q95=annual_95,
    Q97.5=annual_97.5
  ) %>%
  dplyr::select(source, x_name, type,mean, std, Q2.5, Q5, Q95, Q97.5)

# plot with individual models as dot
models_data <- trendy_global_1992_2020 %>%
  arrange(desc(mean)) %>%
  mutate(
    model_num=factor(1:20),
    legend_x= c(rep(1,10),rep(6.5,10)), # this control the legend column width
    legend_y= c(10:1,10:1)
  )

# collate data for panel a: TRENDY (soil+litter+debris) vs this project, global soil
comparison_global <- data.frame(
  source = "TRENDY",
  mean = mean(trendy_global_1992_2020$mean),
  std = sd(trendy_global_1992_2020$mean)
) %>%
  add_row(
    source="Data-driven",
    mean=data_global_1992_2020$mean,
    std=data_global_1992_2020$std) %>%
  mutate(
    source=factor(source, levels=c("TRENDY", "Data-driven"))
  )

# collate data for panel a: Pan et al. (2024) vs this project, global forest soils
comparison_forest <- data.frame(
  source="Pan et al. (2024)",
  mean=0.418462780901968, # from Pan et al. 2024
  std=NA) %>% 
  add_row( # young forest+old forest, 1992-2020 from pan_type_CI 
    # this is generated in script 04_upscaled.R
    source="Data-driven",
    mean=1.57626445,
    std=0.8679839 ) %>% # from analysis code
  mutate(
    source=factor(source, levels=c("Pan et al. (2024)", "Data-driven"))
  )

# set palette
source_palette_global <- c(
  "TRENDY" = "#a49489", # "#B7E0FF",
  "Data-driven" = "chocolate4") # "#A1D6B2"

source_palette_forest <- c(
  "Pan et al. (2024)" = "#a49489", # "#B7E0FF",
  "Data-driven" = "chocolate4") # "#A1D6B2"


# plot panel a: TRENDY vs this project
#theme_set(theme())
p_global_nolegend <- ggplot() + 
  geom_bar(data=comparison_global,
           aes(x=source,y=mean, fill=source),
           stat="identity",color=NA) + 
  scale_fill_manual(values = source_palette_global, guide="none") +
  geom_errorbar(data=comparison_global,
                aes(x=source, ymin=mean-std, ymax=mean+std), color="black", width = 0.2) +
  geom_point(data=models_data,
             aes(x=1,y=mean, color=model_num, fill=model_num), shape=1:20, size=1) + # 
  geom_hline(yintercept=0) + 
  labs(x=NULL, y="Global SOC sink\n(Pg C/yr)", tag="A") + 
  scale_x_discrete(
    labels=c("TRENDY\n(1992-2020)", "Data-driven\n(1992-2020)")) +
  #scale_y_break(c(3, 4.5),ticklabels=4.5 ) +  # Separate breaks for each section. It creates issue like shifting label (not solved) and outer frame (solved by setting theme)
  #expand_limits(y = 5) +  
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(), 
        axis.title = element_text(size = 9),
        axis.line.x = element_blank(),
        axis.ticks.x=element_blank(), legend.title = element_blank(),
        legend.position="none",
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank(),
        plot.tag = element_text(face = "bold",size=10),
        plot.tag.position = c(-0.05, 1),
        plot.margin=margin(5.5, 5.5, 5.5, 8.5) # top, right, bottom, left, default all 5.5
  )

p_global_nolegend

# plot panel b: Pan 2024 vs this project, forest soils
p_forest <- ggplot() + 
  geom_bar(data=comparison_forest,
           aes(x=source,y=mean, fill=source),
           stat="identity",color=NA) + 
  geom_errorbar(data=comparison_forest,
                aes(x=source, ymin=mean-std, ymax=mean+std), color="black", width = 0.2) +
  geom_hline(yintercept=0) + 
  scale_fill_manual(values = source_palette_forest) +
  labs(x=NULL, y="Forest SOC sink\n(Pg C/yr)", tag="B") + 
  scale_x_discrete(
    labels=c("Pan et al. (2024)\n(1990-2019)", "Data-driven\n(1992-2020)")) +
  theme_classic() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line.x = element_blank(),
        panel.border = element_blank(),
        axis.title = element_text(size = 9),
        axis.ticks.x=element_blank(), legend.position="none",
        axis.text.x=element_text(angle=90,vjust=0.5,hjust=1), # 
        plot.tag = element_text(face = "bold",size=10),
        plot.tag.position = c(-0.05, 1),
        plot.margin=margin(5.5,5.5,5.5,8.5) # top, right, bottom, left default all 5.5
  ) 

p_forest

#--------------- graph that compares biomass vs. soil sink estimates -----------
# import data
library(openxlsx)
biome_df <- read.xlsx("./results/SOC_change_table.xlsx",
                      sheet="Fig3_data_for_forest_sink") 

# set palettes
pool_palette_fill <- c(
  "harvested wood"="yellow3",
  "dead wood"="yellow4",
  "living biomass"="darkgreen",
  "litter"="darkgoldenrod",
  "soil (this study)"="chocolate4"
)

pool_palette_color <- c(
  "harvested wood"="yellow3",
  "dead wood"="yellow4",
  "living biomass"="darkgreen",
  "litter"="darkgoldenrod",
  "soil (this study)"="chocolate4" #"#A1D6B2"
)

# process data (make soil and litter below y-axis)
biome_df <- data.frame(biome_df) %>% filter(biome!="grassland") %>%
  mutate(above_below_ground = case_when(
    pool %in% c("harvested wood", "dead wood","living biomass") ~ "Aboveground",
    pool %in% c("soil (this study)", "litter") ~ "Belowground"
  )) %>%
  mutate(
    biome=factor(biome, levels=c("tropical forest","temperate forest","boreal forest","grassland")),
    pool=factor(pool, levels=c("harvested wood","dead wood","living biomass",
                               "litter","soil (this study)")),
    above_below_ground=factor(above_below_ground, levels=c("Aboveground", "Belowground"))
  )

# set facet labels
facet_labels <- c(
  "tropical forest" = "Tropical\nForest",
  "temperate forest" = "Temperate\nForest",
  "boreal forest" = "Boreal\nForest"
)

# plot panel c: different forest C sinks
p_biome_nolegend <- ggplot(biome_df) +
  facet_wrap(~biome, nrow=1,strip.position = "bottom", labeller = as_labeller(facet_labels)) +
  geom_bar(aes(x=as.numeric(above_below_ground),y=sink_PgC_yr,fill=pool, color=pool), stat="identity",
           width=0.98) + 
  scale_fill_manual(name="Pool", values = pool_palette_fill,labels=str_to_title) + 
  scale_color_manual(name="Pool", values = pool_palette_color,labels=str_to_title) + 
  geom_hline(yintercept=0) + 
  labs(x=NULL, y="Forest Carbon Sink\n(Pg C/yr)",tag="C") +
  scale_y_continuous(labels = c(0,1,2), 
                     breaks=c(0,1,2)) + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        panel.spacing = unit(0.5, "lines"),
        axis.line.x = element_blank(),
        strip.text = element_text(size = 9,angle = 90, color="grey30"),
        strip.background = element_blank(),
        axis.title = element_text(size = 9),
        #strip.text =element_blank(), #element_text(angle=90),
        axis.line.y = element_line(color = "black"),
        axis.ticks.x=element_blank(), 
        axis.text.x=element_blank(), #text(angle=90,hjust=1,vjust=0.5),
        #axis.ticks.y=element_blank(), 
        legend.position="none",
        plot.tag = element_text(face = "bold",size=10),
        plot.tag.position = c(0, 1),
        plot.margin=margin(9,8.5,5.5,5.5) ) # top, right, bottom, left default all 5.5)

p_biome_nolegend


#------- legend -----------------------
# load function that emulate ggplot colors
# https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n <- 20
cols <- gg_color_hue(n)

# set up dataframe for models legend
models_legend_df <- trendy_global_1992_2020 %>%
  arrange(desc(mean)) %>%
  mutate(
    model_num=factor(1:20),
    legend_x= c(rep(1,10),rep(7,10)), # this control the legend column width
    legend_y= c(-1:-10,-1:-10),
    color=cols
  )

# if want one column
# models_legend_df <- trendy_global_1992_2020 %>%
#   arrange(desc(mean)) %>%
#   mutate(
#     model_num=factor(1:20),
#     legend_x= c(rep(1,20)),# this control the legend column width
#     legend_y= c(-1:-20),
#     color=cols
#   )

# set up dataframe for pools legend 
AGpool_legend_df <- data.frame(
  x=rep(1,1,1,1),
  y=c(-1,-2,-3),
  color=unname(pool_palette_color)[1:3],
  fill=unname(pool_palette_fill)[1:3],
  label=c("Harvested wood","Dead wood","Living biomass")
)

BGpool_legend_df <- data.frame(
  x=rep(1,1,1,1),
  y=c(-1,-2),
  color=unname(pool_palette_color)[4:5],
  fill=unname(pool_palette_fill)[4:5],
  label=c("Litter","Soil (this study)")
)

estimates_legend_df <- data.frame(
  x=rep(1,1,1),
  y=c(-1,-2,-3),
  color="darkgreen",
  fill="darkgreen",
  label=c("Least restrictive", "Moderately restrictive","Highly restrictive")
)

models_legend <- ggplot() + 
  geom_point(data=models_legend_df,
             aes(x=legend_x-0.7,y=legend_y, color=color), shape=1:20, size=1) + 
  geom_text(data=models_legend_df,
            aes(x=legend_x,y=legend_y,label=x_name),hjust=0, vjust=0.5, size=3.2) +
  geom_text(data=models_legend_df,
            x=0.3, y=0, label="Models", hjust=0, vjust=0, size=3.2, fontface="bold") +
  scale_fill_identity() + scale_color_identity() +
  theme_void() + xlim(c(-0,13)) + ylim(c(-10,0.5)) +
  # if one column:
  # xlim(c(-0,7)) + ylim(c(-20,0.5)) +
  coord_fixed(ratio = 1, expand=T, clip = "on") +
  theme(legend.position="none", plot.margin=margin(0,0,0,0),
        plot.tag = element_blank())

pools_legend <- ggplot() +
  geom_rect(data=AGpool_legend_df,
            aes(xmin=x-0.7, xmax=x-0.1, ymin=y, ymax=y+0.6, fill=fill, color=color)) + 
  geom_text(data=AGpool_legend_df,
            aes(x=x+0.5,y=y,label = label), hjust=0, vjust=0, size=3.2) +
  geom_text(data=AGpool_legend_df,
            x=0.3, y=0, label = "Aboveground", #"Aboveground Pools", 
            hjust=0, vjust=-0.5, size=3.2, fontface="bold") +
  geom_rect(data=BGpool_legend_df,
            aes(xmin=x-0.7, xmax=x-0.1, ymin=y-4.5, ymax=y-4.5+0.6, fill=fill, color=color)) + 
  geom_text(data=BGpool_legend_df,
            aes(x=x+0.5,y=y-4.5,label = label), hjust=0, vjust=0, size=3.2) +
  geom_text(data=BGpool_legend_df,
            x=0.3, y=-4.5, label = "Belowground", #"Belowground Pools", 
            hjust=0, vjust=-0.5, size=3.2, fontface="bold") +
  scale_fill_identity() + scale_color_identity() +
  theme_void() + xlim(c(0,12)) + ylim(c(-7,1)) +
  coord_fixed(ratio = 1, expand=T, clip = "on") +
  theme(legend.position="none", plot.margin=margin(0,0,0,0),
        plot.tag = element_blank())

#------- put panels together ----------
# set font
theme_set(theme_minimal(base_family = "Helvetica", base_size = 14))

# assemble panels
right_column <- arrangeGrob(
  grobs = list(models_legend, p_biome_nolegend),
  ncol = 1,
  heights = c(0.5, 1)  # Adjust ratio as needed
)

# Now layout with the left panels
combined_fig4 <- grid.arrange(
  grobs = list(p_global_nolegend, p_forest, right_column),
  widths = c(1, 1.3),
  layout_matrix = rbind(c(1, 3),
                        c(2, 3))
)

# save 
ggsave("./Figure3.png", plot = combined_fig4, bg = "white", dpi = 600, 
       width = 90 / 25.4, height = 120 / 25.4, units = "in")

# save 
ggsave("./Fig3_pools_legend.png", plot = pools_legend, 
       bg = "transparent", dpi = 600, 
       width = 3, height = 1, units = "in")

# figure is assembled and exported in Powerpoint
