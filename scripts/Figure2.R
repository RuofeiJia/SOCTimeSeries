# Figure 2: SOC sink estimates in posterior violins by cover class and period
rm(list = ls(all = TRUE))
library(ggplot2); library(dplyr); library(grid)


# ------------------- set up ---------------------------------------------------
decade_label <- c("1990"="1992-2000","2000"="2000-2010",
                  "2010"="2010-2020","1992-2020"="1992-2020")
year_span    <- c("1992-2000"=8,"2000-2010"=10,
                  "2010-2020"=10,"1992-2020"=28)

# land cover class order
series_levels <- c("total","grassland","young forest","old forest")  
series_pal <- c("total"="#2B2B2B","grassland"="#E1A02A",   
                "young forest"="#3C8DC4","old forest"="#8064B0") 
# horizontal dodge within a period
series_off <- c("total"=-0.17,"grassland"=0.03,            
                "young forest"=0.23,"old forest"=0.43)
# horizontal locations
period_x   <- c("1992-2000"=1,"2000-2010"=2,"2010-2020"=3,"1992-2020"=4.4)
whole_period    <- "1992-2020"   # drawn as a separate panel on the right
divider_x       <- 3.85 # location of vertical divider for the 1992-2020 period

# violin widths and transparency
global_width <- 0.15
type_width <- 0.09
type_alpha <- 0.7

# plotting parameters
show_significance <- TRUE   # TRUE: fill points whose 95% CrI excludes 0; FALSE: all hollow
show_psink        <- TRUE   # P(sink) labels above the global violins
show_values       <- TRUE   # sink value + 95% CrI labels, above the P(sink) labels

# axis limit
y_lo <- -0.85
y_hi <- 2.06
x_lo <- 0.55
x_hi <- 5.00

# figure size as printed
W_CM <- 18.0; H_CM <- 11.2
## ===========================================================================

## ---- load -----------------------------------------------------------------
# SOC sink estimates
draws_file   <- "./results/find_CI_bayesian_cap2_exclude_ts_extent_numbers_timeseries_new.RData"
summary_file <- "./results/find_CI_bayesian_cap2_exclude_ts_extent_timeseries_new.RData"

load(draws_file)
load(summary_file)

## ---- sink/source color field ---------------------------------------------
# Violins keep their sink/source gradient: green above zero
# (sink), sand below (source), fading to white at the zero line.
# land cover class is carried by the markers/lines/legend (Figure 1 palette), not the violin fill.
# *_v = strong ramp for the violins;  *_b = very faint ramp for the background.

# make 101 gradient colors for violin ramps
SINK_v <- colorRampPalette(c("#FBFEFC","#4E9C82"))(101)
SOURCE_v <- colorRampPalette(c("#FDFCFA","#D3A867"))(101)
SINK_b <- colorRampPalette(c("#FFFFFF","#E4EFE9"))(101)
SOURCE_b <- colorRampPalette(c("#FFFFFF","#F5ECDD"))(101)

# divide color
div_col <- function(y, # SOC sink value on the y axis
                    ramp_pos=SINK_v, 
                    ramp_neg=SOURCE_v, 
                    ref_value=1.35, ease=0.85) {
  
  # map y value to index in color gradient list 
  i <- round(pmin(abs(y)/ref_value,1)^ease * 100) + 1
  # pick color list for sink or source
  ifelse(y>=0, ramp_pos[i], ramp_neg[i])
}

## ---- 1. Annualized posterior draws per series x period --------------------
# For each series x period, annualize its 3000 draws (delta / year-span). These
# drive BOTH the densities and the CrI quantiles.

# list of column names for posterior draws
draw_cols <- grep("^delta_total", names(sum_delta_global), value=TRUE)

# function that group SOC sink estimates for plotting
group_draws <- function(df, series_fixed=NULL) {
  # add period and series (land cover) columns to df
  df$period <- decade_label[as.character(df$decade)] # get corresponding decade label here
  df$series <- if (is.null(series_fixed)) as.character(df$type) else series_fixed
  
  # for each row
  lapply(seq_len(nrow(df)), function(i) {
    # extract series and period
    series <- as.character(df$series[i])
    period <- df$period[i]
    # divide all delta value by year_span
    v <- as.numeric(unlist(df[i, draw_cols])) / year_span[period]
    # get
    list(series = series, period = period, v = v)
  })
}

groups <- c(group_draws(sum_delta_global, "total"),
            group_draws(sum_delta_combined_type))

# location on x axis based on series (land cover) and period (decade)
xpos <- function(series, period)                 
  unname(period_x[period] + series_off[as.character(series)])

# collate dataframe for plotting mean and error bar
qtab <- sum_delta_global %>%
  dplyr::mutate(type = "total") %>%
  dplyr::bind_rows(sum_delta_combined_type) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    mean = mean(c_across(contains("delta_total")), na.rm = TRUE),
    median = median(c_across(contains("delta_total")), na.rm = TRUE),
    q025 = quantile(c_across(contains("delta_total")), 0.025, na.rm = TRUE),
    q17 = quantile(c_across(contains("delta_total")), 0.17, na.rm = TRUE),
    q83 = quantile(c_across(contains("delta_total")), 0.83, na.rm = TRUE),
    q975 = quantile(c_across(contains("delta_total")), 0.975, na.rm = TRUE),
    min = min(c_across(contains("delta_total")), na.rm = TRUE),
    max = max(c_across(contains("delta_total")), na.rm = TRUE),
    p_above = sum(c_across(contains("delta_total"))>0)/length(c_across(contains("delta_total")) )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-contains("delta_total")) %>%
  dplyr::rename(series = type) %>%
  dplyr::mutate(period = decade_label[as.character(decade)]) %>%
  dplyr::select(-decade) %>%
  dplyr::mutate(yearspan = year_span[period]) %>%
  dplyr::mutate(
    mean = mean / yearspan,
    median = median / yearspan,
    q025 = q025 / yearspan,
    q17 = q17 / yearspan,
    q83 = q83 / yearspan,
    q975 = q975 / yearspan,
    min = min / yearspan,
    max = max / yearspan,
  ) %>%
  dplyr::select(series, period, mean, median, min, q025, q17, q83, q975, max, p_above)

# format to plotting mean and error bar
est <- qtab %>%
  mutate(
    # point and intervals
    point = mean, 
    lo_thick = q17,
    hi_thick = q83,
    lo_thin = q025,
    hi_thin = q975,
    # Significance flags
    sig = p_above > 0.975 | p_above < 0.025,
    series = factor(series, levels = series_levels),
    x = xpos(series, period),
    ptfill = if (show_significance) 
      ifelse(sig, series_pal[as.character(series)], "white") 
    else "white"
  ) %>%
  select(series, period, mean,p_above, 
         q025, q17, q83, q975, point, lo_thick, hi_thick, 
         lo_thin, hi_thin, sig, x, ptfill)

## ---- 3. SOC sink estimate densities (half violins) ---------------------------------------------------------

# combine global (with type="total") and by-type data
df_posterior_long <- sum_delta_global %>%
  mutate(type = "total") %>%
  bind_rows(sum_delta_combined_type) %>%
  select(-type_area_ha) %>%
  dplyr::rename(series = type) %>%
  dplyr::mutate(period = decade_label[as.character(decade)]) %>%
  dplyr::select(-decade) %>%
  dplyr::mutate(yearspan = year_span[period]) %>%
  pivot_longer(cols = starts_with("delta"), 
               names_to = "sample", 
               values_to = "delta_value") %>%
  mutate(annual_value = delta_value/ yearspan)

# create density objects and extract x, y
dens <- df_posterior_long %>%
  group_by(series, period) %>%
  # combine data by group
  nest() %>%
  mutate(
    # make density function by each decade
    density_obj = map(data, ~density(.$annual_value, n = 512)),
    # extract posterior sink and density
    sink = map(density_obj, "x"),
    density = map(density_obj, "y")
  ) %>%
  select(-data, -density_obj) %>%
  # unnest to get each row with a sink and density
  unnest(cols = c(sink, density)) %>%
  ungroup() %>%
  mutate(block = ifelse(series == "total", "global", "type")) %>%
  group_by(period, block) %>%
  # get width of strip by normalizing density
  mutate(width = density / max(density) * 
           (if (first(block) == "global") global_width else type_width)) %>%
  ungroup() %>%
  # format x position and id (per violin) for plotting
  mutate(
    series = factor(series, levels = series_levels),
    x_center = xpos(series, period),
    id = as.integer(interaction(series, period, drop = TRUE))
  )

# clip to panel, trim thin tails (<3% of peak), slice into gradient strips
strips <- dens %>% group_by(id) %>%
  filter(density >= 0.03*max(density)) %>% # sink >= y_lo, sink <= y_hi, 
  arrange(sink, .by_group=TRUE) %>%
  # get values from next row for sink and width (to plot rectangle strips)
  mutate(
    sink2 = lead(sink), 
    width2 = lead(width)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(sink2)) %>%
  mutate(id, series, 
         is_global = series=="total",
         xmin = x_center - (width+width2)/2, 
         xmax = x_center,
         ymin = sink, ymax = sink2, 
         ymid = (sink+sink2)/2,
         .keep="none"
  ) %>%
  # assign colors gradient (green up, sand down) to all stripes
  mutate(fill = div_col(ymid) )

# violin outline for global esitmates
outline_g <- strips %>% filter(is_global) %>% arrange(id, ymid)

# faint outline on the CLASS violins too, so near-zero ones (e.g. 1992-2000
# grassland, 2010-2020 young/old forest) read against the background gradient
outline_a <- strips %>% filter(!is_global) %>% arrange(id, ymid)

## ---- SOC sink change (strengthen/weakening and annotations ------------------------
# global estimate
eg <- filter(est, series=="total")
# estimates of type
ea <- filter(est, series!="total")
# decadal global estimate
dec_g <- filter(eg, period!=whole_period)
# decadal estimate by type
dec_a <- filter(ea, period!=whole_period)

# name posterior draw
dvals <- setNames(lapply(groups, `[[`, "v"), vapply(groups, function(g) paste(g$series, g$period), ""))
decs3  <- c("1992-2000","2000-2010","2010-2020")

# summarize significance of SOC sink change between decades
# loop every type/globe and between every 2 consecutive decades
seg_sig <- do.call(rbind, lapply(series_levels, function(s) do.call(rbind, lapply(1:2, function(k) {
  # posterior predictions from earlier decade
  a <- dvals[[paste(s, decs3[k])]]
  # posterior predictions from later decade
  b <- dvals[[paste(s, decs3[k+1])]]
  if (is.null(a) || is.null(b)) return(NULL)
  # probability of decadal sink increased
  p_incr <- mean(b - a > 0)
  # construct one row on sink change significance between every two decade
  data.frame(series=s, 
             x=xpos(s,decs3[k]), 
             xend=xpos(s,decs3[k+1]),
             y=mean(a), 
             yend=mean(b), 
             sig = p_incr>=0.95 | p_incr<=0.05, # P>0.9 and P>0.95 yields same significance
             is_global = s=="total") }))))
# factorize series
seg_sig$series <- factor(seg_sig$series, levels=series_levels)
# show the significant changes
cat("significant decadal changes:\n"); print(seg_sig[seg_sig$sig, c("series","x","xend")])

# add annotations
eg$plab <- ifelse(eg$p_above > 0.995, "italic(P)[sink] > 99*'%'",
                  paste0("italic(P)[sink] == ", round(eg$p_above*100), "*'%'"))
# sink value + 95% CrI (Pg C/yr) per period, drawn on the top line, above the P(sink)
eg$vlab <- gsub("-", "−", sprintf("%.2f [%.2f, %.2f]", eg$point, eg$lo_thin, eg$hi_thin))

# background Grob with color grdient
grad <- rasterGrob(matrix(div_col(seq(y_hi,y_lo,length.out=600), SINK_b, SOURCE_b), ncol=1),
                   width=unit(1,"npc"), height=unit(1,"npc"), interpolate=TRUE)

## ---- 4. plot --------------------------------------------------------------
p <- ggplot() +
  annotation_custom(grad, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  annotate("segment", x=divider_x, xend=divider_x, y=y_lo, yend=y_hi,
           linetype="22", color="grey75", linewidth=0.35) +
  geom_hline(yintercept=0, color="grey40", linewidth=0.4) +
  annotate("text", x=x_hi-0.03, y=y_hi-0.06, label="Carbon sink",
           hjust=1, vjust=1, size=3.2, fontface="italic", color="#3F7361") +
  annotate("text", x=x_hi-0.03, y=y_lo+0.06, label="Carbon source",
           hjust=1, vjust=0, size=3.2, fontface="italic", color="#A67D3C") +
  
  geom_rect(data=filter(strips, !is_global),
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill), alpha=type_alpha) +
  geom_rect(data=filter(strips, is_global),
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=fill)) +
  scale_fill_identity() +
  geom_path(data=outline_a, aes(x=xmin, y=ymid, group=id), color="grey55", linewidth=0.14, alpha=0.55) +
  geom_path(data=outline_g, aes(x=xmin, y=ymid, group=id), color="grey50", linewidth=0.22, alpha=0.6) +
  
  # connecting lines per decade step: SOLID = significant change, DASHED = not
  geom_segment(data=filter(seg_sig, !is_global,  sig), aes(x, xend=xend, y=y, yend=yend, color=series), linewidth=0.43, alpha=0.7) +
  geom_segment(data=filter(seg_sig, !is_global, !sig), aes(x, xend=xend, y=y, yend=yend, color=series), linewidth=0.43, alpha=0.7, linetype="22") +
  geom_segment(data=filter(seg_sig,  is_global,  sig), aes(x, xend=xend, y=y, yend=yend, color=series), linewidth=0.78) +
  geom_segment(data=filter(seg_sig,  is_global, !sig), aes(x, xend=xend, y=y, yend=yend, color=series), linewidth=0.78, linetype="22") +
  # invisible point layer solely to drive the class-color legend as filled dots
  geom_point(data=est, aes(x, point, color=series), alpha=0, show.legend=TRUE) +
  
  geom_linerange(data=ea, aes(x, ymin=lo_thin,  ymax=hi_thin,  color=series), linewidth=0.39, show.legend=FALSE) +
  geom_linerange(data=ea, aes(x, ymin=lo_thick, ymax=hi_thick, color=series), linewidth=0.95, show.legend=FALSE) +
  geom_point(data=ea, aes(x, point, fill=ptfill), shape=21, size=2.0, stroke=0.43, color="black") +
  
  geom_linerange(data=eg, aes(x, ymin=lo_thin,  ymax=hi_thin,  color=series), linewidth=0.52, show.legend=FALSE) +
  geom_linerange(data=eg, aes(x, ymin=lo_thick, ymax=hi_thick, color=series), linewidth=1.4, show.legend=FALSE) +
  geom_point(data=eg, aes(x, point, fill=ptfill), shape=21, size=2.85, stroke=0.7, color="black") +
  { if (show_values) geom_text(data=eg, aes(x + 0.025, y_hi-0.24, label=vlab), size=2.8, color="grey12", fontface="bold") } +
  { if (show_psink)  geom_text(data=eg, aes(x + 0.025, y_hi-0.37, label=plab), parse=TRUE, size=2.85, color="#3F7361") } +
  
  scale_color_manual(values=series_pal, name=NULL, breaks=series_levels) +
  scale_x_continuous(breaks=period_x, labels=names(period_x), limits=c(x_lo, x_hi), expand=c(0,0)) +
  scale_y_continuous(limits=c(y_lo, y_hi), breaks=seq(-0.5,1.5,0.5), expand=c(0,0)) +
  labs(x=NULL, y=expression("SOC sink ("*Pg~C~yr^{-1}*")")) +
  guides(color=guide_legend(override.aes=list(shape=16, size=2.6, alpha=1))) +
  theme_classic(base_size=10) +
  theme(axis.line=element_line(color="grey30", linewidth=0.4),
        axis.ticks=element_line(color="grey30", linewidth=0.4),
        axis.text=element_text(color="grey10", size=9.5),
        axis.title.y=element_text(size=10.5, margin=margin(r=5)),
        legend.position="bottom", legend.text=element_text(size=9.5),
        legend.key.width=unit(1.1,"lines"), plot.margin=margin(7,8.5,3.5,7))

p

ggsave("./figures/Fig2_2026September.png", p, width=W_CM, height=H_CM, units="cm", dpi=450, bg="white")
