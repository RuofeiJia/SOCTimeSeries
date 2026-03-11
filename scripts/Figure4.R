# Figure that visualizes natural soils' contribution to the land sink in 
# global carbon budget framework
# try to make color and style consistent with other main figures
# skip Observational netland in panel C
# minimize redundant estimation bars in visual
library(dplyr)
library(ggplot2)
library(showtext)

# working directory should be set by default to the folder of entire repo (R project)

#------------- create dataframes to plot ---------------------------------------

# normal bar: 0.9 wide (+- 0.45 around bar_position)
# ELUC and biomass_net_flux: 0.4 wide ( +- 0.2 around bar_position)
# GCB bars: 0.6 width

# panel A, GCB land sinks (SLAND)
GCB_sland <- data.frame(
  group = c("2024", "2025"),
  value = c(2.91, 2.23),
  error = c(0.73, 0.6),
  bar_position = 0.5 + c(0, 0.7)
)

# panel A, observational soil sink from this study (soil share of SLAND)
soil_sink <- data.frame(
  group = "Data-driven soil sink",
  value = 1.96,
  error = 0.24,
  bar_position = GCB_sland$bar_position[2] + 1.2 -0.15 #(-0.15 adjust to narrow 0.6 bar)
)


# panel B, land use soil emissions (soil share of ELUC)
soil_eluc <- data.frame(
  group = "Soil land-use change emissions",
  value = 0.57,
  error = 0.57,
  bar_position = soil_sink$bar_position + 1.7 # -0.15 adjust narrow GCB bars -0.25 adjust narrow eluc bar
)

# panel B, net soil flux (soil share of NetLAND)
soil_net_flux <- data.frame(
  group = "Data-driven net soil flux",
  value = 1.39,
  error = 0.62,
  bar_position = soil_eluc$bar_position + 0.8 # -0.25 means adjust position to the narrower bar
)

# panel C, obsevational biomass net flux (biomass share of NetLAND)
biomass_net_flux <- data.frame(
  group = "Data-driven net biomass flux",
  value = 0.02,
  error = 0.3,
  bar_position = soil_net_flux$bar_position + 1.7  # skip the first observational netland soil in panel c
)

# panel C, error of summed observational net land flux (NetLAND)
obs_net_land_flux <- data.frame(
  group = "Data-driven net land flux",
  value = 1.41,
  error = 0.69,
  bar_position =biomass_net_flux$bar_position + 0.8 # distance from middle of biomass bar
)

# panel C, GCB estimates of NetLAND
GCB_netland <- data.frame(
  group = c("DGVM-BK 2024", "DGVM 2024", "DGVM-BK 2025", "DGVM 2025"),
  value = c(1.49, 1.2, 0.53, 1.07),
  error = c(0.8, 0.47, 0.93, 0.47),
  bar_position = obs_net_land_flux$bar_position + 0.9 + seq(0, by = 0.7, length.out = 4)
)

# annotation dataframe
text_df <- data.frame(
  text = c("2024", "2025", "DGVM-BK-24", "DGVM-24", "DGVM-BK-25", "DGVM-25" ), #
  x_position = 0.07+ c(GCB_sland$bar_position,GCB_netland$bar_position),
  y_position = 0.04
)


#-------------- plot -----------------------------------------------------------
showtext_auto()
theme_set(theme_minimal(base_family = "Helvetica", base_size = 12))

p <- ggplot() +
  # panel A, GCB land sink bar from 2024 and 2025 report
  geom_bar(data = GCB_sland,
           aes(x=bar_position, y=value),
           stat="identity",
           color=NA, fill="#c8eaf2", width=0.6,
  ) +
  # panel A, SLANDsoil error bar
  geom_errorbar(data = GCB_sland,
                aes(x = bar_position,
                    ymin = value - error,
                    ymax = value + error),
                width = 0, color="black") +
  # panel A, soil sink bar
  geom_bar(data = soil_sink,
           aes(x = bar_position, y = value),
           stat = "identity",
           color=NA,fill = "chocolate4") +
 # linetype"11"= 1 unit on 1 unit off
  # panel A, add visual aid line
  geom_line(
    aes(x=c(GCB_sland$bar_position[1] - 0.3,
            soil_sink$bar_position - 0.45),
        y=c(soil_sink$value, soil_sink$value) ),
    size = 0.1, color = "chocolate4", linetype="22") +
  
  # panel B, soil LUC emission substracting from soil sink
  geom_rect(data=soil_eluc,
            aes(xmin = bar_position - 0.2, xmax = bar_position + 0.2,
                # position the bar to subtract from soil_sink
                ymin = soil_sink$value - value, ymax = soil_sink$value), 
            fill = "#FFBBC2", color = NA,
            # linetype 3 unit on 3 unit off
            #linetype="43", linewidth=0.28
  ) + 
  # panel B, add visual aid of SLANDsoil - ELUC
  geom_line(
    aes(x=c(soil_sink$bar_position-0.45, # left end of panel B Slandsoil bar 
            soil_eluc$bar_position + 0.2),
        y=c(soil_sink$value, soil_sink$value) ),
    size = 0.1, color = "darkgray") +
  # soil sink error bar
  geom_errorbar(data = soil_sink,
                aes(x = bar_position,
                    ymin = value - error,
                    ymax = value + error),
                width = 0, color="black") +
  # add visual aid of eluc_soil -> net soil flux
  geom_line(
    aes(x=c(soil_eluc$bar_position - 0.2, # left end of panel B Slandsoil bar 
            soil_net_flux$bar_position),
        y=c(soil_sink$value - soil_eluc$value, soil_sink$value - soil_eluc$value) ),
    size = 0.1, color = "darkgray") +
  # error bar of eluc soil
  geom_errorbar(data = soil_eluc,
                aes(x = bar_position,
                    ymin = soil_sink$value - soil_eluc$value - error,
                    ymax = soil_sink$value - soil_eluc$value + error),
                width = 0, color="black") +
  # panel B, net soil flux
  geom_bar(data = soil_net_flux,
           aes(x = bar_position, y = value),
           stat = "identity",
           color=NA,fill = "#a87c34") +
  # panel C, add visual aid of NetLANDsoil + NetLANDbiomass
  geom_line(
    aes(x=c(soil_net_flux$bar_position - 0.45, #- 0.45, 
            biomass_net_flux$bar_position + 0.2),
        y=c(soil_net_flux$value, soil_net_flux$value) ),
    size = 0.1, color = "darkgray") +
  # visual aid of biomass net flux -> observational netland
  geom_line(
    aes(x=c(biomass_net_flux$bar_position - 0.2, #- 0.45, 
            obs_net_land_flux$bar_position + 0.45),
        y=c(obs_net_land_flux$value, obs_net_land_flux$value) ),
    size = 0.1, color = "darkgray") +
# soil net flux errorbar
  geom_errorbar(data = soil_net_flux,
                aes(x = bar_position,
                    ymin = value - error,
                    ymax = value + error),
                width = 0, color="black") +
  # panel C, biomass net flux adding on top of the soil net flux
  geom_rect(data=biomass_net_flux,
            aes(xmin = bar_position - 0.2,
                xmax = bar_position + 0.2,
                # position the bar to subtract from soil_sink
                ymin = soil_net_flux$value, ymax = soil_net_flux$value + value),
            fill = "darkgreen", color = NA,
            # linetype 3 unit on 3 unit off
            #linetype="43", linewidth=0.28
  ) +

  # panel C, summed observational net land flux (soil + biomass)
  geom_bar(data = soil_net_flux,
           aes(x = obs_net_land_flux$bar_position, y = value),
           stat = "identity",
           color=NA,fill = "#a87c34") +
  geom_rect(data=biomass_net_flux,
            aes(xmin = obs_net_land_flux$bar_position - 0.45,
                xmax = obs_net_land_flux$bar_position + 0.45,
                # position the bar to subtract from soil_sink
                ymin = soil_net_flux$value, ymax = soil_net_flux$value + value),
            fill = "darkgreen", color = NA,
            # linetype 3 unit on 3 unit off
            #linetype="43", linewidth=0.28
  ) +
  # panel C biomass net land error bar
  geom_errorbar(data = biomass_net_flux,
                aes(x = bar_position,
                    ymin = soil_net_flux$value + value - error,
                    ymax = soil_net_flux$value + value + error),
                width = 0, color="black") +
  # # panel C obs netland flux error bar
  geom_errorbar(data = obs_net_land_flux,
                aes(x = bar_position,
                    ymin = value - error,
                    ymax = value + error),
                width = 0, color="black") +
  # panel C, GCB net land flux estimates
  geom_bar(data = GCB_netland,
           aes(x=bar_position, y=value),
           stat="identity",
           color=NA, fill="#c8eaf2", width=0.6,
           #linetype="11"
  ) + # linetype"11"= 1 unit on 1 unit off
  geom_errorbar(data = GCB_netland,
                aes(x = bar_position,
                    ymin = value - error,
                    ymax = value + error),
                width = 0, color="black") +
  # add anotation of GCB estimates
  geom_text(data = text_df,
            aes(x = x_position, y = y_position, label = text),
            angle = 90, hjust = 0, vjust = 1, color="#47B8D2",
            size = 9/2.845 ) +
  
  # overlay line and ribbon around observational net land flux
  geom_rect(data = obs_net_land_flux,
            aes(xmin = bar_position - 0.45, #- 0.2,
                xmax = GCB_netland$bar_position[4] + 0.3, # end at the edge of last bar
                ymin = value - error, ymax =value + error),
            fill = "#FFE2CC", alpha=0.2, #color = "#FFE2CC", 
            # linetype 4 unit on 4 unit off linetype="44"
  ) +
  geom_line(
    aes(x=c(obs_net_land_flux$bar_position - 0.45, 
            GCB_netland$bar_position[4] + 0.3),
        y=c(obs_net_land_flux$value, obs_net_land_flux$value) ),
    size = 0.5, color = "#F59A23") +
  geom_line(
    aes(x=c(obs_net_land_flux$bar_position - 0.45, 
            GCB_netland$bar_position[4] + 0.3),
        y=c(obs_net_land_flux$value - obs_net_land_flux$error,
            obs_net_land_flux$value - obs_net_land_flux$error) ),
    size = 0.5, color = "#fbd7a7", linetype="44") +
  geom_line(
    aes(x=c(obs_net_land_flux$bar_position - 0.45, 
            GCB_netland$bar_position[4] + 0.3),
        y=c(obs_net_land_flux$value + obs_net_land_flux$error, 
            obs_net_land_flux$value + obs_net_land_flux$error) ),
    size = 0.5, color = "#fbd7a7", linetype="44") +
  # format
  scale_x_continuous(breaks=1,labels=NULL, expand = expansion(mult = c(0.02, 0.02))) +
  geom_hline(yintercept=0) + 
  scale_y_continuous(breaks=c(0,1,2,3,4), limits = c(-0.5, 4)) +
  labs(x=NULL, y=expression(Pg~C~yr^{-1})) +
  theme_bw() +
  theme(
    legend.position = "none", #panel.border = element_blank(),
    panel.border = element_blank(),
    # Keep axis lines (left & bottom only)
    axis.line = element_line(color = "black"),
    # # if only plot y axis 0-3, uncomment next line
    # axis.line.y = element_blank(),
    axis.line.x.top = element_blank(),
    axis.line.y.right = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #axis.text.x = element_text(size = 10,angle = 90, vjust = 0.5, hjust = 1),
    #axis.title = element_text(size = 12)
  )

showtext_auto(FALSE)

# Display the plot
print(p)

# save image
ggsave("./figures/Figure4.png", plot = p,
       bg = "white", dpi = 600, width = 150 / 25.4, height = 120 / 25.4, units = "in")

# annotation is done in powerpoint
