# TRENDY process models vs the data-driven global SOC sink, 1992-2020,
# over the same in-domain area and period.
# for Figure 4a
rm(list = ls(all = TRUE))
suppressMessages({library(dplyr); library(purrr); library(ggplot2); library(grid)})
set.seed(1)

## ---- palette --------------------------------------------------------------
GLOBAL <- "#2B2B2B"   # data-driven total SOC sink
MEANC  <- "#3A3F44"   # TRENDY ensemble mean/SD summary (dark)
SINKLAB<- "#3F7361"   # "carbon sink" annotation green
soil_ramp <- colorRampPalette(c("#E2CE9F","#C89B5C","#9A6B3C","#6B4A2A","#402D1A"))  # pale tan -> dark brown (no green)

# sink/source gradient ramp + mapping, identical to Figure 2 so a given sink value
# reads as the same green in both figures.
SINK_v <- colorRampPalette(c("#FBFEFC","#4E9C82"))(101); SOURCE_v <- colorRampPalette(c("#FDFCFA","#D3A867"))(101)
SINK_b <- colorRampPalette(c("#FFFFFF","#E4EFE9"))(101);  SOURCE_b <- colorRampPalette(c("#FFFFFF","#F5ECDD"))(101)
# assign gradient color in half violin
div_col <- function(y, rp=SINK_v, rn=SOURCE_v, refv=1.35, ease=0.85) {
  i <- round(pmin(abs(y)/refv,1)^ease * 100) + 1; ifelse(y>=0, rp[i], rn[i])
}

# deterministic beeswarm: points that collide in y are pushed into side-by-side
# columns around a center x
beeswarm_x <- function(y, xc, ydiam, xstep, maxcol=40) {
  o <- order(y); px <- numeric(length(y)); py <- c(); pox <- c()
  offs <- xstep * c(0, as.numeric(rbind(1:maxcol, -(1:maxcol))))   # 0,+1,-1,+2,-2,...
  for (i in o) for (off in offs) if (all(!(abs(y[i]-py) < ydiam & abs(off-pox) < xstep*0.98))) {
    px[i] <- xc + off; py <- c(py, y[i]); pox <- c(pox, off); break }
  px
}

## ---- 1. data-driven total SOC sink on in-domain old forests and grasslands ----------------
load("./results/find_CI_bayesian_cap2_exclude_ts_extent_numbers_timeseries_new.RData")

# get old forest+grassland draws
old_forest_draws <- sum_delta_combined_type %>%
  filter(decade == "1992-2020" & type=="old forest") %>%
  dplyr::select(starts_with("delta_total")) %>% unlist(use.names = FALSE)
old_forest_draws <- old_forest_draws/28   # annualize (28-yr period)

grassland_draws <- sum_delta_combined_type %>%
  filter(decade == "1992-2020" & type=="grassland") %>%
  dplyr::select(starts_with("delta_total")) %>% unlist(use.names = FALSE)
grassland_draws <- grassland_draws/28  # annualize (28-yr period)

draws <- old_forest_draws + grassland_draws
                              
# summarize mean, CI, P_sink
dd <- list(mean  = mean(draws),
           lo66 = quantile(draws, .17),  hi66 = quantile(draws, .83),
           lo95 = quantile(draws, .025), hi95 = quantile(draws, .975),
           psink = mean(draws > 0))

## ---- 2. TRENDY per-model rates ----------------------------------------------
# download this excel from TRENDY analysis on github, to data folder:
# https://github.com/RuofeiJia/SOC_change_TRENDYv12/blob/main/results/TRENDY_v12_global_cSoil_S2_fraction.csv
trendy <- read.csv("./data/TRENDY_v12_global_cSoil_S2_fraction.csv") %>%
  mutate(model = sub(".*/","", sub(".*S2/","", model))) %>%
  filter(!(model %in% c("CARDAMOM","CLM5.0","CABLE-POP","YIBs"))) %>%   # 17 models kept
  mutate(rate = (X2020 - X1992)/28)
# multi model mean and standard deviation
tr_mean <- mean(trendy$rate); tr_sd <- sd(trendy$rate)

## ---- geometry -------------------------------------------------------------
# x anchors: TRENDY (left), data-driven (right)
XT <- 1; XD <- 2
y_lo <- -0.14; y_hi <- 1.30
# data-driven half-violin max width
DW <- 0.34

# half-violin = kernel density of the posterior DRAWS (not a Gaussian), sliced into
# horizontal gradient strips colored by the sink/source ramp
# the min ans max of draws is within y_lo and y_hi
den <- density(draws, n = 512, from = y_lo, to = y_hi) 
# stripes for plotting
strips <- tibble(y = den$x, w = den$y / max(den$y) * DW) %>%
  mutate(ymin = y, ymax = lead(y), xmax = XD + (w + lead(w)) / 2,
         fill = div_col((y + lead(y)) / 2)) %>%
  filter(!is.na(ymax))

# beeswarm x-position per model; point shade = rank of the model's rate (pale->dark)
trendy <- trendy %>%
  mutate(x    = beeswarm_x(rate, xc = XT, ydiam = 0.020, xstep = 0.032),
         rank = rank(rate, ties.method = "first"),
         dcol = soil_ramp(n())[rank])
# half width of multi model standard deviation rectangle
# to cover all the trendy points in beeswarm
sw_hw <- max(abs(trendy$x - XT)) + 0.02

# Supplementary table with estimate from each model
trendy %>% transmute(shade_rank = rank, model, rate_PgC_yr = round(rate, 4)) %>%
  arrange(shade_rank) %>%
  write.csv("./figures/Table_S_TRENDY_v12_models.csv", row.names = FALSE)

# background gradient grob
grad <- rasterGrob(matrix(div_col(seq(y_hi,y_lo,length.out=600), SINK_b, SOURCE_b), ncol=1),
                   width=unit(1,"npc"), height=unit(1,"npc"), interpolate=TRUE)
# annotation of P sink
psink_lab <- if (dd$psink > 0.995) "italic(P)[sink] > 99*'%'" else
  paste0("italic(P)[sink] == ", round(dd$psink*100), "*'%'")
# annotation for data-driven SOC sink estimate
dd_lab <- gsub("-","−", sprintf("%.2f [%.2f, %.2f]", dd$mean, dd$lo95, dd$hi95))
# annotation for trendy mean+ sd estimate
tr_lab <- sprintf("%.2f ± %.2f", tr_mean, tr_sd)


## ---- plot -----------------------------------------------------------------
p <- ggplot() +
  # background
  annotation_custom(grad, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  # x axis
  geom_hline(yintercept=0, color="grey40", linewidth=0.45) +
  annotate("text", x=XT-0.52, y=y_hi-0.02, label="Carbon sink", hjust=0, vjust=1,
           size=2.7, fontface="italic", color=SINKLAB) +
  # observational lower 95% bound, drawn across so every model sits below it
  annotate("segment", x=XT-0.52, xend=XD, y=dd$lo95, yend=dd$lo95,
           linetype="22", color="#3F7361", linewidth=0.4) +
  annotate("text", x=XT-0.50, y=dd$lo95+0.03, hjust=0, vjust=0, size=2.35,
           color="#3F7361", label="lower 95% bound, this study") +
  
  # TRENDY: SD band + mean bar (aligned with the beeswarm), soil dots on top -
  # trendy SD rectangle
  annotate("rect", xmin=XT-sw_hw, xmax=XT+sw_hw, ymin=tr_mean-tr_sd, ymax=tr_mean+tr_sd,
           fill="gray40", color="gray20", linewidth=0.2, alpha=0.15) +
  # single-model points
  geom_point(data=trendy, aes(x, rate, fill=dcol), shape=21, size=1.9, color="white", stroke=0.3) +
  # multi-model mean
  annotate("segment", x=XT-sw_hw, xend=XT+sw_hw, y=tr_mean, yend=tr_mean, color=MEANC, linewidth=0.8) +
  # estimate annotation
  annotate("text", x=XT, y=0.25, label=tr_lab, size=2.6, color=MEANC, fontface="bold") +
  
  # --- data-driven: real posterior violin + mean/66%/95% eye glyph ------------
# half violin
geom_rect(data=strips, aes(xmin=XD, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill)) +
  scale_fill_identity() +
  # 66CI, 95CI, mean
  annotate("linerange", x=XD, ymin=dd$lo95, ymax=dd$hi95, color=GLOBAL, linewidth=0.7) +
  annotate("linerange", x=XD, ymin=dd$lo66, ymax=dd$hi66, color=GLOBAL, linewidth=1.9) +
  annotate("point", x=XD, y=dd$mean, size=3.1, shape=21, fill=GLOBAL, color="black", stroke=0.8) +
  # value + P(sink), placed to the left of the flat violin edge (hjust=1) so they clear it
  annotate("text", x=XD-0.03, y=dd$hi95+0.055, label=dd_lab, hjust=1, size=2.7, color="grey12", fontface="bold") +
  annotate("text", x=XD-0.03, y=dd$hi95+0.165, label=psink_lab, hjust=1, parse=TRUE, size=2.7, color=SINKLAB) +
  
  scale_x_continuous(breaks=c(XT, XD),
                     labels=c("TRENDY-v12\n(process models)","This study\nold forest + grassland\n(repeat samplings)"),
                     limits=c(XT-0.55, XD+DW+0.06), expand=c(0,0)) +
  scale_y_continuous(limits=c(y_lo, y_hi), breaks=seq(0,1.2,0.2), expand=c(0,0)) +
  labs(x=NULL, y=expression("SOC sink ("*Pg~C~yr^{-1}*")"), tag="a") +
  theme_classic(base_size=8) +
  theme(axis.line.x=element_blank(), axis.ticks.x=element_blank(),
        axis.line.y=element_line(color="grey30", linewidth=0.4),
        axis.ticks.y=element_line(color="grey30", linewidth=0.4),
        axis.title.y=element_text(size=8), axis.text=element_text(color="grey15", size=7.5),
        axis.text.x=element_text(size=7.6, color="grey15"),
        plot.tag=element_text(face="bold", size=10),
        legend.position="none", plot.margin=margin(4,8,2,4))

ggsave("./figures/Fig_trendy_comparison_2026September.png", p, width=8.8, height=7.4, units="cm", dpi=400, bg="white")
cat(sprintf("Data-driven: %.2f [%.2f, %.2f]  P(sink)=%.3f | TRENDY: %.3f ± %.3f (n=%d) | ratio=%.1fx | models<95%%floor: %d/%d\n",
            dd$mean, dd$lo95, dd$hi95, dd$psink, tr_mean, tr_sd, nrow(trendy),
            dd$mean/tr_mean, sum(trendy$rate<dd$lo95), nrow(trendy)))
cat("Wrote figures/Fig_trendy_comparison.{png,pdf}\n")
