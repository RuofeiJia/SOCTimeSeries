# compute in-domain land sink, which is the land sink for all in-domain old forest and 
# grassland pixels from 7 models in O'Sullivan et al. 2026
# https://www.nature.com/articles/s41612-025-01302-7
# It yields in-domain land sink to be 0.6 ± PgC/yr during 1992-2020
# This result is pasted to fig_sland_fraction.R to plot Figure 4b

# BEFORE RUNNING: download "gridded_fluxes.nc" to "./data" folder from
# https://zenodo.org/records/17515195

suppressMessages(library(ncdf4))

map_dir <- "./data"
f   <- paste0(map_dir,"/gridded_fluxes.nc") 
if (!file.exists(f)) stop("Missing ", f, " -- download gridded_fluxes.nc from Zenodo 17515195 (v2); see header.")

secpyr <- 365.25 * 86400
Rearth <- 6371007.2 # authalic radius (m)

# make new "tab" that contain all pixels in-domain for old forests and grassland, not young forests
load("./results/find_CI_bayesian_cap2_exclude_ts_extent_pixel_summary_timeseries_new.RData")

tab <- rbind(summary_oldforest1992_2020, summary_grassland1992_2020) %>%
  dplyr::select(x,y,cellsize_ha,proportion) %>%
  group_by(x,y,cellsize_ha) %>%
  summarize(oldforest_grassland_proportion = sum(proportion, na.rm = T), .groups="drop") %>%
  mutate(area_oldgrass_ha = cellsize_ha*oldforest_grassland_proportion)
# 17100 in-domain old forest and grassland pixels

nc  <- nc_open(f)
names(nc$dim) # "lon"   "lat"   "model" "time" 
lon <- as.numeric(nc$dim$lon$vals); lat <- as.numeric(nc$dim$lat$vals)
tmv <- as.numeric(nc$dim$time$vals); yrs <- if (max(tmv) < 200) 1959 + tmv else tmv
tsel <- which(yrs >= 1992 & yrs <= 2020)
dlon <- abs(lon[2]-lon[1]); dlat <- abs(lat[2]-lat[1])
vars <- "SLAND_new" 

# full grid-cell area (m2) as a function of latitude row
area_lat <- Rearth^2 * (dlon*pi/180) *
  abs(sin((lat + dlat/2)*pi/180) - sin((lat - dlat/2)*pi/180))

# map each in-domain pixel center to a (lon,lat) grid index
xq <- tab$x; if (max(lon) > 181) xq <- xq %% 360
ix <- pmin(pmax(round((xq     - lon[1])/(lon[2]-lon[1])) + 1, 1), length(lon))
iy <- pmin(pmax(round((tab$y  - lat[1])/(lat[2]-lat[1])) + 1, 1), length(lat))

compute_var <- function(v){
  # get variable
  A <- ncvar_get(nc, v) # dims [lon, lat, model, time]
  # number of model (3rd dimension)
  nmod <- dim(A)[3]
  # row bind the results for each model
  do.call(rbind, lapply(1:nmod, function(m){
    # take mean over dimensions 1:dims (1 & 2) across the rest dimensions (year, dimension 3)
    mf   <- rowMeans(A[, , m, tsel], dims=2, na.rm=TRUE) # [lon,lat] 1992-2020 mean flux
    glob <- sum(colSums(mf, na.rm=TRUE) * area_lat) * secpyr * 1e-12
    fl   <- mf[cbind(ix, iy)] # flux at in-domain pixels
    data.frame(model = m,
               # all pixels 
               global = glob,
               # all in-domain old forest and grassland pixels
               indomain_fullcell = sum(fl * area_lat[iy] * secpyr * 1e-12, na.rm=TRUE))
  }))
}

all_res <- list()
for (v in vars){
  res <- compute_var(v); all_res[[v]] <- cbind(variable = v, res)
  cat(sprintf("\n===== %s (1992-2020 mean, %d models) =====\n", v, nrow(res)))
  print(round(res, 3))
  cat(sprintf("  gridded GLOBAL sum  = %.2f ± %.2f Pg C/yr   (authoritative: SLAND_new 2.18, SLAND 2.69)\n",
              mean(res$global), sd(res$global)))
  cat(sprintf("  in-domain FULL-CELL = %.3f ± %.3f Pg C/yr   <- option 2 (CHOSEN)\n",
              mean(res$indomain_fullcell), sd(res$indomain_fullcell)))
}
nc_close(nc)

# ===== SLAND_new (1992-2020 mean, 7 models) =====
#   model global indomain_fullcell
# 1     1  2.977             0.913
# 2     2  2.868             0.646
# 3     3  1.598             0.702
# 4     4  1.899             0.470
# 5     5  2.666             0.724
# 6     6  1.193             0.393
# 7     7  2.050             0.357
# gridded GLOBAL sum  = 2.18 ± 0.68 Pg C/yr (global DGVM land sink averaged over these 7 models. Close to GCB report)
# in-domain FULL-CELL = 0.601 ± 0.202 Pg C/yr (copy into fig_sland_fraction.R)

# get in-domain land sink area (sum of all in-domain old forests and grassland pixel areas)
fullcell_Mha <- sum(area_lat[iy], na.rm=TRUE)/1e10
cat(sprintf("\nin-domain pixels: %d | summed full-cell area = %.0f Mha",
            nrow(tab), fullcell_Mha))
