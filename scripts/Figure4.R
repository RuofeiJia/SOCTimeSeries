# Figure 4 — soils versus the modeled soil sink and land carbon sink (two panels).
# Panel a (fig_trendy_comparison.R): 
# TRENDY process-model cSoil change vs our old forest and grassland SOC sink, in-domain 1992-2020 (apples-to-apples).
# Panel b (fig_sland_fraction.R):
# our SOC sink compared with the in-domain land sink (on all in-domain old forest and grassland pixels) from  
# and global land sink reported in Global Carbon Budget

rm(list = ls(all = TRUE))

suppressMessages({library(ggplot2); library(patchwork)})

# run each panel script in its own environment and lift out its ggplot object
grab <- function(f){ e <- new.env(); sys.source(f, envir=e); e$p }

pA <- grab("./scripts/fig_trendy_comparison.R")
pB <- grab("./scripts/fig_sland_fraction.R")

fig4 <- pA + pB + plot_layout(widths=c(1.12, 1))

ggsave("./figures/Fig4_2026Sep15.png", fig4, width=18, height=7.8, units="cm", dpi=400, bg="white")