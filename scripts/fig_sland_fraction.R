# plot in-domain land sink and global land sink compared with 
# in-domain SOC sink in old forests and grasslands
# for figure 4b
rm(list = ls(all = TRUE))
suppressMessages({library(dplyr); library(ggplot2); library(grid)})

## ---- palette --------------------------------------------------------------
SOIL    <- "#4E9C82"   # our soil sink (the sink green from Fig 2)
SOILDK  <- "#2F6B57"   # soil CrI whisker (darker green)
SLANDF  <- "#C4C1B7"   # modeled S_LAND bar (context grey)
SLANDDK <- "#4A4A45"   # S_LAND SD whisker (dark grey)

## ---- reference land sink from GCB (GCB25 DGVM) ------------------------------------
# GCB25 official global land sink
SLAND_GLOBAL_MEAN <- 2.23; SLAND_GLOBAL_SD <- 0.60

# in-domain S_LAND over predicted old+grass cells (cellsize_ha, same cells as SOC)
# result from compute_sland_indomain.R
SLAND_IND_MEAN    <- 0.601; SLAND_IND_SD  <- 0.202  

## ---- area each element spans (Gha) -- encoded by BAR WIDTH -----------------
# from compute_sland_indomain.R
# summed area for all pixels within the OLD+GRASS predicted domain (3900 Mha)
AREA_IND_GHA    <- 3.9
# modeled global land (per-model mean 13.7 Gha; "~14")
AREA_GLOBAL_GHA <- 14

## ---- in-domain SOC sink in old forests and grasslands ------------------
load("./results/find_CI_bayesian_cap2_exclude_ts_extent_numbers_timeseries_new.RData")

# get posterior estimates by type
ty <- sum_delta_combined_type; dc <- grep("^delta_total", names(ty))
# get posterior estimates for old forest and grasslands
oldr <- as.numeric(ty[ty$decade=="1992-2020" & ty$type=="old forest", dc])
grar <- as.numeric(ty[ty$decade=="1992-2020" & ty$type=="grassland",  dc])
draws <- (oldr + grar) / 28       # old+grassland, annualized
v  <- draws

ss <- list(mean=mean(v), med=median(v), lo66=quantile(v,.17), hi66=quantile(v,.83),
           lo95=quantile(v,.025), hi95=quantile(v,.975))
ss_mean <- mean(draws)
ss_sd <- sd(draws)
#percent of in-domain SOC sink in in-domain land sink and global land sink
pct_ind  <- 100*round(ss_mean,2)/round(SLAND_IND_MEAN,2)     # 73
pct_glob <- 100*round(ss_mean,2)/round(SLAND_GLOBAL_MEAN,2)  # 20
# uncertainty of percentage
pct_ind_uncertainty <- 100*round( (ss_mean/SLAND_IND_MEAN)*sqrt((ss_sd/ss_mean)^2 + (SLAND_IND_SD/SLAND_IND_MEAN)^2 ), 2)
pct_glob_uncertainty <- 100*round( (ss_mean/SLAND_GLOBAL_MEAN)*sqrt((ss_sd/ss_mean)^2 + (SLAND_GLOBAL_SD/SLAND_GLOBAL_MEAN)^2 ), 2)

## ---- geometry (uniform bar width; area stated in Gha brackets) -------------
bw <- 0.30 # half of bar width
XM <- 0.42 # soil value marker (in-domain), left
XI <- 1.05 # in-domain land-sink bar
XG <- 2.05 # global land-sink bar
y_hi <- 3.35  # y axis height

# function that plot stacked bar for soil sink at bottom and the rest of land sink on top
nest_bar <- function(x, total, pct, pct_uncertainty) list(
  # land sink bar
  annotate("rect", xmin=x-bw, xmax=x+bw, ymin=0, ymax=total, fill=SLANDF),
  # SOC sink bar
  annotate("rect", xmin=x-bw, xmax=x+bw, ymin=0, ymax=ss_mean, fill=SOIL),
  # white line on top of SOC sink 
  annotate("segment", x=x-bw, xend=x+bw, y=ss_mean, yend=ss_mean, color="white", linewidth=0.4),
  # annotate percentage
  annotate("text", x=x, y=ss_mean*0.5,
           label=sprintf("%.0f\u00B1%.0f%%", pct, pct_uncertainty),
           color="white", fontface="bold", size=3.5)
  )

p <- ggplot() +
  # ---- in-domain and global bars: total land sink (grey) + soil nested (green)
  nest_bar(XI, SLAND_IND_MEAN, pct_ind, pct_ind_uncertainty)  +
  nest_bar(XG, SLAND_GLOBAL_MEAN, pct_glob, pct_glob_uncertainty) +
  # error bar and estimate annotation: in-domain land sink
  annotate("linerange", x=XI, ymin=SLAND_IND_MEAN-SLAND_IND_SD, ymax=SLAND_IND_MEAN+SLAND_IND_SD,
           color=SLANDDK, linewidth=0.6) +
  annotate("text", x=XI, y=SLAND_IND_MEAN+SLAND_IND_SD+0.1, size=2.5, color=SLANDDK,
           label=sprintf("%.2f ± %.2f", SLAND_IND_MEAN, SLAND_IND_SD)) +
  # error bar and estimate annotation: global land sink
  annotate("linerange", x=XG, ymin=SLAND_GLOBAL_MEAN-SLAND_GLOBAL_SD, ymax=SLAND_GLOBAL_MEAN+SLAND_GLOBAL_SD,
           color=SLANDDK, linewidth=0.6) +
  annotate("text", x=XG, y=SLAND_GLOBAL_MEAN+SLAND_GLOBAL_SD+0.09, size=2.5, color=SLANDDK,
           label=sprintf("%.2f ± %.2f", SLAND_GLOBAL_MEAN, SLAND_GLOBAL_SD)) +
  
  # ---- soil value marker (in-domain) + posterior CrI, left ------------------
annotate("linerange", x=XM, ymin=ss$lo95, ymax=ss$hi95, color=SOILDK, linewidth=0.7) +
  annotate("linerange", x=XM, ymin=ss$lo66, ymax=ss$hi66, color=SOILDK, linewidth=1.9) +
  annotate("point", x=XM, y=ss$mean, size=2, shape=21, fill=SOIL, color=SOILDK, stroke=0.7) +
  annotate("text", x=XM, y=ss$hi95+0.06, vjust=0, size=2.4, color=SOILDK, fontface="bold", lineheight=0.85,
           label=sprintf("%.2f\n[%.2f, %.2f]", ss_mean, ss$lo95, ss$hi95)) +
  
  # ---- dashed line: SAME soil value across both bars (drawn in the gaps) -----
annotate("segment", x=XM,    xend=XI-bw, y=ss_mean, yend=ss_mean, linetype="22", color=SOILDK, linewidth=0.35) +
  annotate("segment", x=XI+bw, xend=XG-bw, y=ss_mean, yend=ss_mean, linetype="22", color=SOILDK, linewidth=0.35) +
  
  # ================= area brackets under the x-axis ==========================
# annotate one line segment at a time, and then annotate area text (in-domain area)
annotate("segment", x=XM-0.16, xend=XI+bw,  y=-0.24, yend=-0.24, color="grey35", linewidth=0.4) +
  annotate("segment", x=XM-0.16, xend=XM-0.16, y=-0.24, yend=-0.15, color="grey35", linewidth=0.4) +
  annotate("segment", x=XI+bw,   xend=XI+bw,   y=-0.24, yend=-0.15, color="grey35", linewidth=0.4) +
  annotate("text", x=(XM-0.16+XI+bw)/2, y=-0.30, vjust=1, size=2.5, color="grey15", lineheight=0.9,
           label="In-domain region\n3.9 Gha") +
  # do the same for global area
  annotate("segment", x=XG-bw, xend=XG+bw, y=-0.24, yend=-0.24, color="grey35", linewidth=0.4) +
  annotate("segment", x=XG-bw, xend=XG-bw, y=-0.24, yend=-0.15, color="grey35", linewidth=0.4) +
  annotate("segment", x=XG+bw, xend=XG+bw, y=-0.24, yend=-0.15, color="grey35", linewidth=0.4) +
  annotate("text", x=XG, y=-0.30, vjust=1, size=2.5, color="grey15", lineheight=0.9,
           label="Global land\n~14 Gha") +
  
  # ================= compact legend (top-left, free space) ===================
# legend line 1: soil sink rectangle and text
annotate("rect", xmin=XM-0.25, xmax=XM-0.25+0.14, ymin=3.04, ymax=3.04+0.16, fill=SOIL, color=SOILDK, linewidth=0.3) +
  annotate("text", x=XM-0.25+0.20, y=3.12, hjust=0, vjust=0.5, size=2.5, color="grey15",
           label="Soil sink, old forest + grassland (this study)") +
  # legend line 2: soil color + land sink color: Land sink
  annotate("rect", xmin=XM-0.25, xmax=XM-0.25+0.14, ymin=2.76, ymax=2.76+0.16, fill=SOIL, color=SOILDK, linewidth=0.3) +
  annotate("text", x=XM-0.25+0.16, y=2.84, hjust=0, vjust=0.5, size=2.5, color="grey15",
           label="+") +
  annotate("rect", xmin=XM-0.25+0.23, xmax=XM-0.25+0.14+0.23, ymin=2.76, ymax=2.76+0.16, fill=SLANDF, color=SLANDDK, linewidth=0.3) +
  annotate("text", x=XM-0.25+0.43, y=2.84, hjust=0, vjust=0.5, size=2.5, color="grey15",
           label="Land carbon sink (models)") +
  
  # ================= axes / theme ============================================
geom_hline(yintercept=0, color="grey40", linewidth=0.45) +
  scale_x_continuous(limits=c(XM-0.32, XG+bw+0.06), expand=c(0,0)) +
  scale_y_continuous(breaks=seq(0,3,1), expand=c(0,0)) +
  coord_cartesian(ylim=c(0, y_hi), clip="off") +
  labs(x=NULL, y=expression("Land carbon sink ("*Pg~C~yr^{-1}*")"), tag="b") +
  theme_classic(base_size=8) +
  theme(axis.line.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x=element_blank(),
        axis.line.y=element_line(color="grey30", linewidth=0.4),
        axis.ticks.y=element_line(color="grey30", linewidth=0.4),
        axis.title.y=element_text(size=8), axis.text.y=element_text(color="grey15", size=7.5),
        plot.tag=element_text(face="bold", size=10), plot.margin=margin(4,6,42,4))

ggsave("./figures/Fig_sland_fraction_2026September.png", p, width=8.6, height=7.4, units="cm", dpi=400, bg="white")
# ggsave("figures/Fig_sland_fraction.pdf", p, width=8.6, height=7.4, units="cm", bg="white")
cat(sprintf("S_LAND_soil = %.3f [%.2f, %.2f]\n  vs GLOBAL   S_LAND %.2f ± %.2f -> %.0f%%\n  vs IN-DOMAIN S_LAND %.2f ± %.2f -> %.0f%%\n",
            ss_mean, ss$lo95, ss$hi95, SLAND_GLOBAL_MEAN, SLAND_GLOBAL_SD, pct_glob,
            SLAND_IND_MEAN, SLAND_IND_SD, pct_ind))
cat("Wrote figures/Fig_sland_fraction.{png,pdf}  (S_LAND numbers PROVISIONAL — confirm with M. O'Sullivan)\n")
