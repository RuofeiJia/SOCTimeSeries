# 1. download and process all spatial data needed for this project
# 2. process spatial data for analysis
# 3. convert some spatial data to dataframe to aid analysis
# All outputs in this script goes to "data" folder
# outputs from step 1 and 2 are not posted in the repo to save storage space
# outputs from step 3 are save in the "data" folder of this repo

# working directory should be set by default to the folder of entire repo (R project)
# set working directory to data folder
setwd("./data")

# libraries
library(terra)
library(gdalUtilities)
library(tictoc)
library(cruts)
library(raster)
library(dplyr)
library(ncdf4)

#----------- 1. download spatial data from internet ----------------------------

# download SoilGrids maps following this tutorial:
# https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/webdav_from_R.md

# soilgrids bullk density 0-5 cm (unit cg/cm^3)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/bdod/bdod_0-5cm_mean.vrt'),
               "./fine_bulk_density_0-5cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               projwin_srs =igh)
toc()

tic()
gdalwarp("./fine_bulk_density_0-5cm_SoilGrids2.vrt",
         "./fine_bulk_density_0-5cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("fine_bulk_density_0-5cm_SoilGrids2_ll.vrt",
               "./fine_bulk_density_0-5cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()

# soilgrids bullk density 5-15cm (unit cg/cm^3)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/bdod/bdod_5-15cm_mean.vrt'),
               "./fine_bulk_density_5-15cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./fine_bulk_density_5-15cm_SoilGrids2.vrt",
         "./fine_bulk_density_5-15cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("fine_bulk_density_5-15cm_SoilGrids2_ll.vrt",
               "./fine_bulk_density_5-15cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()

# soilgrids bullk density 15-30 cm (unit cg/cm^3)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/bdod/bdod_15-30cm_mean.vrt'),
               "./fine_bulk_density_15-30cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./fine_bulk_density_15-30cm_SoilGrids2.vrt",
         "./fine_bulk_density_15-30cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("fine_bulk_density_15-30cm_SoilGrids2_ll.vrt",
               "./fine_bulk_density_15-30cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()


# soilgrids bullk density 30-60 cm (unit cg/cm^3)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/bdod/bdod_30-60cm_mean.vrt'),
               "./fine_bulk_density_30-60cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./fine_bulk_density_30-60cm_SoilGrids2.vrt",
         "./fine_bulk_density_30-60cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("fine_bulk_density_30-60cm_SoilGrids2_ll.vrt",
               "./fine_bulk_density_30-60cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()


# soilgrids bullk density 60-100 cm (unit cg/cm^3)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/bdod/bdod_60-100cm_mean.vrt'),
               "./fine_bulk_density_60-100cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./fine_bulk_density_60-100cm_SoilGrids2.vrt",
         "./fine_bulk_density_60-100cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("fine_bulk_density_60-100cm_SoilGrids2_ll.vrt",
               "./fine_bulk_density_60-100cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()


# soilgrids bullk density 100-200 cm (unit cg/cm^3)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/bdod/bdod_100-200cm_mean.vrt'),
               "./fine_bulk_density_100-200cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./fine_bulk_density_100-200cm_SoilGrids2.vrt",
         "./fine_bulk_density_100-200cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("fine_bulk_density_100-200cm_SoilGrids2_ll.vrt",
               "./fine_bulk_density_100-200cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()

# Soilgrids coarse fragment 0-5 cm (unit cm^3/dm^3=vol‰)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/cfvo/cfvo_0-5cm_mean.vrt'),
               "./coarse_fragment_vol_0-5cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./coarse_fragment_vol_0-5cm_SoilGrids2.vrt",
         "./coarse_fragment_vol_0-5cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("coarse_fragment_vol_0-5cm_SoilGrids2_ll.vrt",
               "./coarse_fragment_vol_0-5cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()


# Soilgrids coarse fragment 5-15 cm (unit cm^3/dm^3=vol‰)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/cfvo/cfvo_5-15cm_mean.vrt'),
               "./coarse_fragment_vol_5-15cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./coarse_fragment_vol_5-15cm_SoilGrids2.vrt",
         "./coarse_fragment_vol_5-15cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("coarse_fragment_vol_5-15cm_SoilGrids2_ll.vrt",
               "./coarse_fragment_vol_5-15cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()


# Soilgrids coarse fragment 15-30 cm (unit cm^3/dm^3=vol‰)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/cfvo/cfvo_15-30cm_mean.vrt'),
               "./coarse_fragment_vol_15-30cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./coarse_fragment_vol_15-30cm_SoilGrids2.vrt",
         "./coarse_fragment_vol_15-30cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("coarse_fragment_vol_15-30cm_SoilGrids2_ll.vrt",
               "./coarse_fragment_vol_15-30cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()


# Soilgrids coarse fragment 30-60 cm (unit cm^3/dm^3=vol‰)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/cfvo/cfvo_30-60cm_mean.vrt'),
               "./coarse_fragment_vol_30-60cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./coarse_fragment_vol_30-60cm_SoilGrids2.vrt",
         "./coarse_fragment_vol_30-60cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("coarse_fragment_vol_30-60cm_SoilGrids2_ll.vrt",
               "./coarse_fragment_vol_30-60cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()


# Soilgrids coarse fragment 60-100 cm (unit cm^3/dm^3=vol‰)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/cfvo/cfvo_60-100cm_mean.vrt'),
               "./coarse_fragment_vol_60-100cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./coarse_fragment_vol_60-100cm_SoilGrids2.vrt",
         "./coarse_fragment_vol_60-100cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("coarse_fragment_vol_60-100cm_SoilGrids2_ll.vrt",
               "./coarse_fragment_vol_60-100cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()


# Soilgrids coarse fragment 100-200 cm (unit cm^3/dm^3=vol‰)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

tic()
gdal_translate(paste0(sg_url,'/cfvo/cfvo_100-200cm_mean.vrt'),
               "./coarse_fragment_vol_100-200cm_SoilGrids2.vrt",
               tr=c(1000,1000), # c(xres,yres) unit meter?
               #projwin=bb,
               projwin_srs =igh)
toc()

tic()
gdalwarp("./coarse_fragment_vol_100-200cm_SoilGrids2.vrt",
         "./coarse_fragment_vol_100-200cm_SoilGrids2_ll.vrt",
         s_srs=igh,
         t_srs="EPSG:4326",
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("coarse_fragment_vol_100-200cm_SoilGrids2_ll.vrt",
               "./coarse_fragment_vol_100-200cm_SoilGrids2.tif",
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()


# Soilgrids topsoil clay content 0-5 cm (unit g/kg)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

gdal_translate(paste0(sg_url,'clay/clay_0-5cm_mean.vrt'),
               "./clay05cm_0p25_SoilGrids2.vrt",
               tr=c(1000,1000),
               #projwin=bb,
               projwin_srs =igh)

tic()
gdalwarp("./clay05cm_0p25_SoilGrids2.vrt",
         "./clay05cm_0p25_SoilGrids2_ll.vrt", 
         s_srs=igh, 
         t_srs="EPSG:4326", 
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("clay05cm_0p25_SoilGrids2_ll.vrt",  
               "./clay05cm_0p25_SoilGrids2.tif", 
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()

# Soilgrids topsoil nitrogen 0-5 cm (unit cg/kg)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

gdal_translate(paste0(sg_url,'/nitrogen/nitrogen_0-5cm_mean.vrt'),
               "./n05cm_SoilGrids2.vrt",
               tr=c(1000,1000),
               projwin_srs =igh)

tic()
gdalwarp("./n05cm_SoilGrids2.vrt",
         "./n05cm_SoilGrids2_ll.vrt", 
         s_srs=igh, 
         t_srs="EPSG:4326", 
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("n05cm_SoilGrids2_ll.vrt",  
               "./n05cm_SoilGrids2.tif", 
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()


# Soilgrids topsoil pH 0-5 cm (unit pH*10)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection
sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"


gdal_translate(paste0(sg_url,'phh2o/phh2o_0-5cm_mean.vrt'),
               "./ph05cm_SoilGrids2.vrt",
               tr=c(1000,1000),
               #projwin=bb,
               projwin_srs =igh)

tic()
gdalwarp("./ph05cm_SoilGrids2.vrt",
         "./ph05cm_SoilGrids2_ll.vrt", 
         s_srs=igh, 
         t_srs="EPSG:4326", 
         of="VRT",
         overwrite = TRUE)
toc()

tic()
gdal_translate("ph05cm_SoilGrids2_ll.vrt",  
               "./ph05cm_SoilGrids2.tif", 
               co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
toc()

# Climate Research Unit Time Series (CRUTS 4.07) mean annual temperature and precipitation data
# The analysis was done with 4.07 data. method for downloading 4.07 and 4.08 data are identical but version number
# now CRUTS 4.08 has been releaced and supersede 4.07
# see comparison of 4.07 vs 4.08 here: https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.08/comparisons_with_other_releases/
# download directly from CRUTS website (link below) under "CRU TS v4.07 Data Variables"
# select tab "tmp" and download "cru_ts4.07.1901.2022.tmp.dat.nc.gz"
# select tab "pre" anmd download "cru_ts4.07.1901.2022.pre.dat.nc.gz"
# save them to data folder as 
# "cru_ts4.07.1901.2022.tmp.dat.nc" and "cru_ts4.07.1901.2022.pre.dat.nc"
# https://crudata.uea.ac.uk/cru/data/hrg/


# World forest age map (Besnard et al. 2021)
# Citation: https://doi.org/10.5194/essd-13-4881-2021
# download directly from online (link below)
# save in "data" folder as "202432118501222_BGIForestAgeMPIBGC1.0.0.nc"
# https://doi.org/10.17871/ForestAgeBGI.2021


# ESA CCI land cover maps
# download directly from link below
# Open webpage for ESA CCI land cover maps (link 1)
# Follow the section "land cover maps"
# click on "Copernicus Climate Change Service (C3S) Climate Data Store" (link 2), 
# after login, the download page should say 
# "Land cover classification gridded maps from 1992 to present derived from satellite observations"
# download year 1992, 2000, 2010, 2020
# decompress zip file and save .vc files in "data" folder as
# year 1992: "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992-v2.0.7cds.nc"
# year 2000: "ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7cds.nc"
# year 2010: "ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010-v2.0.7cds.nc"
# year 2020: "C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"
# CCI land cover link: https://maps.elie.ucl.ac.be/CCI/viewer/download.php
# Climate Data Store link: https://cds.climate.copernicus.eu/datasets/satellite-land-cover?tab=overview


# World administrative boundaries for visualization
# download the shapefile directly from link below, decompress the .zip file to get shapefile folder
# save in "data" folder as a folder "world-administrative-boundaries", containing "/world-administrative-boundaries.shp"
# https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/export/?flg=en-us


#----------- 2. process spatial data for analysis ------------------------------

## MAT_year layers (0.5 degree resolution)
# load CRUTS temperature .nc file
tmp_filename <- "./cru_ts4.07.1901.2022.tmp.dat.nc"

# calculate mean annual temperature (MAT) for year 1992, 2000, 2010, 2020
time_range_1992 <- c(paste0(1992,"-01-01"), paste0(1992,"-12-31"))
tmp_1992 <- cruts2raster(tmp_filename, timeRange=time_range_1992)
mat_1992 <- mean(tmp_1992)
raster::crs(mat_1992) <- "EPSG:4326"

time_range_2000 <- c(paste0(2000,"-01-01"), paste0(2000,"-12-31"))
tmp_2000 <- cruts2raster(tmp_filename, timeRange=time_range_2000)
mat_2000 <- mean(tmp_2000)
raster::crs(mat_2000) <- "EPSG:4326"

time_range_2010 <- c(paste0(2010,"-01-01"), paste0(2010,"-12-31"))
tmp_2010 <- cruts2raster(tmp_filename, timeRange=time_range_2010)
mat_2010 <- mean(tmp_2010)
raster::crs(mat_2010) <- "EPSG:4326"

time_range_2020 <- c(paste0(2020,"-01-01"), paste0(2020,"-12-31"))
tmp_2020 <- cruts2raster(tmp_filename, timeRange=time_range_2020)
mat_2020 <- mean(tmp_2020)
raster::crs(mat_2020) <- "EPSG:4326"

# save MAT rasters
writeRaster(mat_1992,'./mat_1992.tif',overwrite=TRUE,progress='text')
writeRaster(mat_2000,'./mat_2000.tif',overwrite=TRUE,progress='text')
writeRaster(mat_2010,'./mat_2010.tif',overwrite=TRUE,progress='text')
writeRaster(mat_2020,'./mat_2020.tif',overwrite=TRUE,progress='text')


## MAP_year layers (0.5 degree resolution)
# load CRUTS precipitation .nc file
prec_filename <- "./cru_ts4.07.1901.2022.pre.dat.nc"

# calculate mean annual precipitation (MAP) for year 1992, 2000, 2010, 2020
time_range_1992 <- c(paste0(1992,"-01-01"), paste0(1992,"-12-31"))
prec_1992 <- cruts2raster(prec_filename, timeRange=time_range_1992)
map_1992 <- sum(prec_1992)
raster::crs(map_1992) <- "EPSG:4326"

time_range_2000 <- c(paste0(2000,"-01-01"), paste0(2000,"-12-31"))
prec_2000 <- cruts2raster(prec_filename, timeRange=time_range_2000)
map_2000 <- sum(prec_2000)
raster::crs(map_2000) <- "EPSG:4326"

time_range_2010 <- c(paste0(2010,"-01-01"), paste0(2010,"-12-31"))
prec_2010 <- cruts2raster(prec_filename, timeRange=time_range_2010)
map_2010 <- sum(prec_2010)
raster::crs(map_2010) <- "EPSG:4326"

time_range_2020 <- c(paste0(2020,"-01-01"), paste0(2020,"-12-31"))
prec_2020 <- cruts2raster(prec_filename, timeRange=time_range_2020)
map_2020 <- sum(prec_2020)
raster::crs(map_2020) <- "EPSG:4326"

# save MAP rasters
writeRaster(map_1992,'./map_1992.tif',overwrite=TRUE,progress='text')
writeRaster(map_2000,'./map_2000.tif',overwrite=TRUE,progress='text')
writeRaster(map_2010,'./map_2010.tif',overwrite=TRUE,progress='text')
writeRaster(map_2020,'./map_2020.tif',overwrite=TRUE,progress='text')


## land_cover class layers with reassigned values (300m resolution)
# export land cover class and reassign values
# all forest classes as 300, grasslands as 400, shrublands as 500, other classes as NA
# save as rasters "lc<YEAR>_class_NA.tif" for year 1992, 2000, 2010, 2020

# 1992
filename_lc1992 <- "./ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992-v2.0.7cds.nc"
# export land cover class to raster
lc1992_class0 <- stack(filename_lc1992, varname="lccs_class")
lc1992_class_rast <- unstack(lc1992_class0)[[1]] # this is a raster file of with original CCI land cover vlass values (300m resolution)
writeRaster(lc1992_class_rast,'./lc1992_class.tif',overwrite=TRUE,progress='text') # save intermediate step
# load as terra rast
lc1992_class_rast <- rast("./lc1992_class.tif")
# reassign value
lc1992_class_rast[lc1992_class_rast<45 | lc1992_class_rast>135] <- NA # pixel with non forest/grass/shrub to be NA
lc1992_class_rast[lc1992_class_rast>45 & lc1992_class_rast<105] <- 300 # forest to 300 # this worked
lc1992_class_rast[lc1992_class_rast==110 | lc1992_class_rast==130] <- 400 # grassland to 400
lc1992_class_rast[lc1992_class_rast>115 & lc1992_class_rast<125] <- 500 # shrubland to 500
# convert back to raster and save
lc1992_class_NA <- raster(lc1992_class_rast)
writeRaster(lc1992_class_NA,'./lc1992_class_NA.tif',overwrite=TRUE,progress='text')

# 2000
filename_lc2000 <- "./ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7cds.nc"
# export land cover class to raster
lc2000_class0 <- stack(filename_lc2000, varname="lccs_class")
lc2000_class_rast <- unstack(lc2000_class0)[[1]]
writeRaster(lc2000_class_rast,'./lc2000_class.tif',overwrite=TRUE,progress='text') # save intermediate step
# load as terra rast
lc2000_class_rast <- rast("./lc2000_class.tif")
# reassign value
lc2000_class_rast[lc2000_class_rast<45 | lc2000_class_rast>135] <- NA # pixel with non forest/grass/shrub to be NA
lc2000_class_rast[lc2000_class_rast>45 & lc2000_class_rast<105] <- 300 # forest to 300 # this worked
lc2000_class_rast[lc2000_class_rast==110 | lc2000_class_rast==130] <- 400 # grassland to 400
lc2000_class_rast[lc2000_class_rast>115 & lc2000_class_rast<125] <- 500 # shrubland to 500
# convert back to raster and save
lc2000_class_NA <- raster(lc2000_class_rast)
writeRaster(lc2000_class_NA,'./lc2000_class_NA.tif',overwrite=TRUE,progress='text')

# 2010
filename_lc2010 <- "./ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010-v2.0.7cds.nc"
# export land cover class to raster
lc2010_class0 <- stack(filename_lc2010, varname="lccs_class")
lc2010_class_rast <- unstack(lc2010_class0)[[1]]
writeRaster(lc2010_class_rast,'./lc2010_class.tif',overwrite=TRUE,progress='text') # save intermediate step
# load as terra rast
lc2010_class_rast <- rast("./lc2010_class.tif")
# reassign value
lc2010_class_rast[lc2010_class_rast<45 | lc2010_class_rast>135] <- NA # pixel with non forest/grass/shrub to be NA
lc2010_class_rast[lc2010_class_rast>45 & lc2010_class_rast<105] <- 300 # forest to 300 # this worked
lc2010_class_rast[lc2010_class_rast==110 | lc2010_class_rast==130] <- 400 # grassland to 400
lc2010_class_rast[lc2010_class_rast>115 & lc2010_class_rast<125] <- 500 # shrubland to 500
# convert back to raster and save
lc2010_class_NA <- raster(lc2010_class_rast)
writeRaster(lc2010_class_NA,'./lc2010_class_NA.tif',overwrite=TRUE,progress='text')

# 2020
filename_lc2020 <- "./C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"
# export land cover class to raster
lc2020_class0 <- stack(filename_lc2020, varname="lccs_class")
lc2020_class_rast <- unstack(lc2020_class0)[[1]]
writeRaster(lc2020_class_rast,'./lc2020_class.tif',overwrite=TRUE,progress='text') # save intermediate step
# load as terra rast
lc2020_class_rast <- rast("./lc2020_class.tif")
# reassign value
lc2020_class_rast[lc2020_class_rast<45 | lc2020_class_rast>135] <- NA # pixel with non forest/grass/shrub to be NA
lc2020_class_rast[lc2020_class_rast>45 & lc2020_class_rast<105] <- 300 # forest to 300 # this worked
lc2020_class_rast[lc2020_class_rast==110 | lc2020_class_rast==130] <- 400 # grassland to 400
lc2020_class_rast[lc2020_class_rast>115 & lc2020_class_rast<125] <- 500 # shrubland to 500
# convert back to raster and save
lc2020_class_NA <- raster(lc2020_class_rast)
writeRaster(lc2020_class_NA,'./lc2020_class_NA.tif',overwrite=TRUE,progress='text')


## land_cover change layers (300m resolution)
# export land cover change and save as rasters for year 2000, 2010, 2020
# (1992 is the initial year for land cover map time series, so previuos change is not recorded)
filename_lc2000 <- "./ESACCI-LC-L4-LCCS-Map-300m-P1Y-2000-v2.0.7cds.nc"
lc2000_change0 <- stack(filename_lc2000, varname="change_count") # change count (if==0, no land cover has occured)
lc2000_change_rast <- unstack(lc2000_change0)[[1]]
writeRaster(lc2000_change_rast,'./lc2000_change.tif',overwrite=TRUE,progress='text')

filename_lc2010 <- "./ESACCI-LC-L4-LCCS-Map-300m-P1Y-2010-v2.0.7cds.nc"
lc2010_change0 <- stack(filename_lc2010, varname="change_count") # change count (if==0, no land cover has occured)
lc2010_change_rast <- unstack(lc2010_change0)[[1]]
writeRaster(lc2010_change_rast,'./lc2010_change.tif',overwrite=TRUE,progress='text')

filename_lc2020 <- "./C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.nc"
lc2020_change0 <- stack(filename_lc2020, varname="change_count") # change count (if==0, no land cover has occured)
lc2020_change_rast <- unstack(lc2020_change0)[[1]]
writeRaster(lc2020_change_rast,'./lc2020_change.tif',overwrite=TRUE,progress='text')


## resampled forest age map to match land cover resolution (300m resolution)
# this is an intermediate step to generate binary land cover layers 
# resample forest age map (1km resolution) to 300m resolution
filename <- "./202432118501222_BGIForestAgeMPIBGC1.0.0.nc"
# export to raster
world_forest_age_stack <- stack(filename, varname="ForestAge_TC010")
# use 10% tree cover correction (not consider pixels with <10% tree cover)
world_forest_age_rast <- unstack(world_forest_age_stack)[[1]]
writeRaster(world_forest_age_rast,'./world_forest_age_rast.tif',overwrite=TRUE,progress='text') # save intermediate step
# load as terra rast and load land cover map as rast
forest_age <- rast('./world_forest_age_rast.tif')
lc1992_class <- rast('./lc1992_class_NA.tif')
tic()
# resample
forest_age_resampled <- resample(forest_age, lc2009_class, method="near") # resample to mat_1992
toc()
# save raster
writeRaster(raster(forest_age_resampled),'./forest_age_resampled.tif',overwrite=TRUE,progress='text')


## binary young/old forest, grassland, shrubland layers (300m resolution)
# this is an intermediate step to generate proportion_decade_dfs (zonal statistics)
# this step uses "forest_age_resampled.tif" and processed land cover maps from above
# the output are 300m resolution layers such that 
# pixel value=1 if pixel is certain type at the beginning year and has not experienced
# major land cover change between start and end year

# define function (this function uses forest_age_resampled.tif)
calculate_binary <- function(lc_start=lc1992_class, 
                             lc_end=lc2000_class, 
                             lc_change_start=NA,
                             lc_change_end=lc2000_change,
                             start_yr=1992, 
                             type="young forest") {# lc1992_class, lc2000_class (with 300,400,500,NA)
  
  # this function returns land cover layer that value=1 where pixel is certain type at beginning year and has not experienced
  # major land cover change between start and end year
  
  # assign initial land cover class value
  binary_layer <- lc_start 
  
  # filter by different type
  if (type=="young forest") {
    # for young forest
    age_threshold <- 60 + 2010 - start_yr # define the age at 2010, if age=60 at beginning year
    binary_layer[binary_layer==300 & forest_age_resampled<age_threshold] <- 1 # all forest pixels that are <60 years in year of beginning
    binary_layer[binary_layer!=1 | is.na(binary_layer)] <- 0 # all other pixels to value 0
  } else if (type=="old forest") {
    # for old forest
    age_threshold <- 60 + 2010 - start_yr # define the age at 2010, if age=60 at beginning year
    binary_layer[binary_layer==300 & forest_age_resampled>=age_threshold] <- 1 # all forest pixels that are >=60 years in year of beginning
    binary_layer[binary_layer!=1 | is.na(binary_layer)] <- 0 # all other pixels to value 0
  } else if (type=="grassland") {
    # for grassland
    binary_layer[binary_layer==400] <- 1 # all grassland pixels
    binary_layer[binary_layer!=1 | is.na(binary_layer)] <- 0 # all other pixels to value 0
  } else if (type=="shrubland") {
    # for shrubland
    binary_layer[binary_layer==500] <- 1 # all grassland pixels
    binary_layer[binary_layer!=1 | is.na(binary_layer)] <- 0 # all other pixels to value 0
  } else {
    print("type string not recognized")
  }
  
  # exclude pixels with land cove change between start and end year
  if (start_yr==1992) {# if start year is 1992
    binary_layer[lc_change_end!=0] <- 0 # all pixels that are not stable in land cover change to value 0
  } else {
    binary_layer[lc_change_start!=lc_change_end] <- 0 # pixels that not different change count between start and end year
  }
  
  return(binary_layer)
}

# load land cover class (reassigned) and change as terra rast
lc1992_class <- rast('./lc1992_class_NA.tif')
lc2000_class <- rast('./lc2000_class_NA.tif')
lc2010_class <- rast('./lc2010_class_NA.tif')
lc2020_class <- rast('./lc2020_class_NA.tif')

lc2000_change <- rast('./lc2000_change.tif')
lc2010_change <- rast('./lc2010_change.tif')
lc2020_change <- rast('./lc2020_change.tif')

# load resampled forest age map
forest_age_resampled <- rast('./forest_age_resampled.tif')

# process and save binary layers as <landcovertype>_binary<year>.tif
  # young forest
tic()
youngforest_binary1990 <- calculate_binary(lc1992_class, lc2000_class, NA, lc2000_change, 1992, "young forest")
toc(); print("youngforest_binary1990 processed")
writeRaster(raster(youngforest_binary1990),'./youngforest_binary1990.tif',overwrite=TRUE,progress='text')
print("youngforest_binary1990.tif saved")

tic()
youngforest_binary2000 <- calculate_binary(lc2000_class, lc2010_class, lc2000_change, lc2010_change, 2000, "young forest")
toc(); print("youngforest_binary2000 processed")
writeRaster(raster(youngforest_binary2000),'./youngforest_binary2000.tif',overwrite=TRUE,progress='text')
print("youngforest_binary1990.tif saved")

tic()
youngforest_binary2010 <- calculate_binary(lc2010_class, lc2020_class, lc2010_change, lc2020_change, 2010, "young forest")
toc(); print("youngforest_binary2010 processed")
writeRaster(raster(youngforest_binary2010),'./youngforest_binary2010.tif',overwrite=TRUE,progress='text')
print("youngforest_binary2010.tif saved")

  # old forest
tic()
oldforest_binary1990 <- calculate_binary(lc1992_class, lc2000_class, NA, lc2000_change, 1992, "old forest")
toc(); print("oldforest_binary1990 processed")
writeRaster(raster(oldforest_binary1990),'./oldforest_binary1990.tif',overwrite=TRUE,progress='text')
print("oldforest_binary1990.tif saved")

tic()
oldforest_binary2000 <- calculate_binary(lc2000_class, lc2010_class, lc2000_change, lc2010_change, 2000, "old forest")
toc(); print("oldforest_binary2000 processed")
writeRaster(raster(oldforest_binary2000),'./oldforest_binary2000.tif',overwrite=TRUE,progress='text')
print("oldforest_binary2000.tif saved")

tic()
oldforest_binary2010 <- calculate_binary(lc2010_class, lc2020_class, lc2010_change, lc2020_change, 2010, "old forest")
toc(); print("oldforest_binary2010 processed")
writeRaster(raster(oldforest_binary2010),'./oldforest_binary2010.tif',overwrite=TRUE,progress='text')
print("oldforest_binary2010.tif saved")

  # grassland
tic()
grassland_binary1990 <- calculate_binary(lc1992_class, lc2000_class, NA, lc2000_change, 1992, "grassland")
toc(); print("grassland_binary1990 processed")
writeRaster(raster(grassland_binary1990),'./grassland_binary1990.tif',overwrite=TRUE,progress='text')
print("grassland_binary1990.tif saved")

tic()
grassland_binary2000 <- calculate_binary(lc2000_class, lc2010_class, lc2000_change, lc2010_change, 2000, "grassland")
toc(); print("grassland_binary2000 processed")
writeRaster(raster(grassland_binary2000),'./grassland_binary2000.tif',overwrite=TRUE,progress='text')
print("grassland_binary2000.tif saved")

tic()
grassland_binary2010 <- calculate_binary(lc2010_class, lc2020_class, lc2010_change, lc2020_change, 2010, "grassland")
toc(); print("grassland_binary2010 processed")
writeRaster(raster(grassland_binary2010),'./grassland_binary2010.tif',overwrite=TRUE,progress='text')
print("grassland_binary2010.tif saved")

tic()
grassland_binary1992_2020 <- calculate_binary(lc1992_class, lc2020_class, NA, lc2020_change, 1992, "grassland")
toc(); print("grassland_binary1992_2020 processed")
writeRaster(raster(grassland_binary1992_2020),'./grassland_binary1992_2020.tif',overwrite=TRUE,progress='text')
print("grassland_binary1992_2020.tif saved")


## mat_1992 polyon for zonal statistics
# this is an intermediate step to generate proportion_decade_df (zonal statistics)
# create a shapefile where each polygon (square) correspond to one 0.5-degree pixel in predictor layers
# load mat_1992.tif as terra rast (target 0.5 degree resolution)
mat_1992 <- rast("./mat_1992.tif")
# convert mat_1992 to polygon
tic()
mat_1992_polygon <- terra::as.polygons(mat_1992, dissolve=F)
toc()
# Save the polygons as a shapefile
writeVector(mat_1992_polygon, "./mat_1992_polygon/mat_1992_polygon.shp", filetype = "ESRI Shapefile",overwrite=TRUE)


## resampled N, clay, elev predictor layers to 0.5 degree resolution
# prepare target raster file to resample to
mat_1992_rast <- rast("./mat_1992.tif")

#soil nitrogen % in 0.5 degree resolution from soilgrids
  # import layer and match it to cruts resolution
n_sg_rast <- rast("./n05cm_SoilGrids2.tif")
tic()
n_sg_rast <- resample(n_sg_rast, mat_1992_rast, method="bilinear")
n_sg_rast <- n_sg_rast/1000 # soilgrids N unit is in cg/kg, convert to %
toc()
# save
writeRaster(raster(n_sg_rast),'./n_sg_halfdegree.tif',overwrite=TRUE,progress='text')

# soil clay % in 0.5 degree resolution from soilgrids
  # import layer and match it to cruts resolution
clay_sg_rast <- rast("./clay05cm_0p25_SoilGrids2.tif")
tic()
clay_sg_rast <- resample(clay_sg_rast, mat_1992_rast, method="bilinear")
clay_sg_rast <- clay_sg_rast/10 # soilgrids clay unit is in g/kg, convert to %
toc()
# save
writeRaster(raster(clay_sg_rast),'./clay_sg_halfdegree.tif',overwrite=TRUE,progress='text')

# soil pH in 0.5 degree resolution from soilgrids
  # import layer and match it to cruts resolution
ph_sg_rast <- rast("./ph05cm_SoilGrids2.tif")
tic()
ph_sg_rast <- resample(ph_sg_rast, mat_1992_rast, method="bilinear")
ph_sg_rast <- ph_sg_rast/10 # soilgrid pH is pH*10, convert to pH
toc()
# save
writeRaster(raster(ph_sg_rast),'./ph_sg_halfdegree.tif',overwrite=TRUE,progress='text')


#----------- 3. convert some spatial data to dataframe to aid analysis ---------

## sample MATs and MAPs of all sites and create "cru_df"
# so data processing is faster (see "function_extract_CRU" in functions.R)
load("./SOC_dataset.rda")
  # set min and max year
year_min <- as.double(min(df$observation_year)) # 1876
year_max <- as.double(max(df$observation_year)) # 2022
  # extract all locations
points <- dplyr::select(df, lat, long) %>% mutate(
  lat = as.double(lat),
  long = as.double(long)
) %>% distinct()
coordinates(points) <- c("long","lat")
  # set temp and prec directory
temp_file <- "./cru_ts4.07.1901.2022.tmp.dat.nc"
prec_file <- "./cru_ts4.07.1901.2022.pre.dat.nc"
  # create cru_df
cru_df <- data.frame(points)
for (year in max(1901,year_min):1902) {
  time_range <- c(paste0(year,"-01-01"), paste0(year,"-12-31"))
  
  temp <- cruts2raster(temp_file, timeRange=time_range)
  mat <- mean(temp)
  mat.df <- data.frame(mat=(raster::extract(mat,points)))
  colnames(mat.df) <- paste0("mat_", year)
  
  prec <- cruts2raster(prec_file, timeRange=time_range)
  map <- preSum <- sum (prec) # Sum precipitation
  map.df <- data.frame(map=(raster::extract(map,points)))
  colnames(map.df) <- paste0("map_", year)
  
  cru_df <- cbind(cru_df, mat.df, map.df)
}
  # save cru_df
save(cru_df, file="./cru_df.rda")


## sample forest age of all sites and create "world_forest_age_df" (not saved to save storage space)
# this used in function "extract_Besnard_forest_age()". See functions.R
# load and format product
filename <- "./202432118501222_BGIForestAgeMPIBGC1.0.0.nc"
world_forest_age <- stack(filename, varname="ForestAge_TC010")
# use 10% tree cover correction (not consider pixels with <10% tree cover)
crs(world_forest_age) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# extract product based on coordinate
# prepare points
df[c("lat","long")] <- sapply(df[c("lat","long")], as.double)
points <- df %>% filter(land_cover=="forest") %>% distinct(lat,long)  # add layer ID?
# create forest age df and convert points to spatial points
world_forest_age_df <- points
coordinates(points) <- c("long","lat")
# extract forest age to
tic()
world_forest_age_df$besnard_forest_age <- raster::extract(world_forest_age,points)
world_forest_age_df <- world_forest_age_df%>% mutate(
  besnard_forest_age=as.double(besnard_forest_age))
toc() # 1110.441 sec elapsed
# save world_forest_age.df
save(world_forest_age_df, file="./world_forest_age_df.rda")


## for each 0.5-degree pixel, calculate land cover areal fraction, cell size, and define region name
# format into a dataframe with each row representing one pixel
# save in "data" folder as "proportion_decade_df.rda"
# this step uses the vector object "mat_1992_polygon" and binary layers from step 2

# define functions
  # this function reporjects binary layers into equal area system and calculate zonal statistics (area fraction of a certain land cover in each pixel)
calculate_zonal <- function(binary_layer) {
  # project raster to the equal-area projection
  tic()
  binary_layer_aea <- project(binary_layer, equal_area_crs, method="near")
  unique(binary_layer_aea) # should return only 0 and 1
  toc()
  print("binary_layer_aea reprojection finished")
  
  # zonal statistics (make sure both have same projection)
  tic()
  binary_layer_zonal <- terra::extract(binary_layer_aea, mat_1992_polygon_aea, fun="mean", na.rm=T, weights=T) 
  toc()
  print("binary_layer_zonal finished")
  
  return(binary_layer_zonal)
}
  # this function converts a raster to df with columns x, y, value
raster2df <- function(layer) {
  # convert raster to spatial pixel data frame, then to regular dataframe
  layer_spdf <- as(layer, "SpatialPixelsDataFrame")
  layer_df <- as.data.frame(layer_spdf)
  colnames(layer_df) <- c("value", "x", "y")
  
  return(layer_df)
}

# create coordinates in mat_1992 pixels/polygons
mat_1992_coords <- raster2df(raster(mat_1992)) %>%
  dplyr::select(x,y)

# format mat_1992_polygon
  # Define an equal-area projection: EPSG:6933 projection (WGS 84 / NSIDC EASE-Grid 2.0 Global)
equal_area_crs <- "EPSG:6933"
  # project mat_1992_polygon to equal area projection
mat_1992_polygon_aea <- terra::project(mat_1992_polygon, equal_area_crs)

# calculate zonal statistics (area fraction of each land cover during each decade)
  # young forest
print("calculate youngforest_zonal1990")
youngforest_zonal1990 <- calculate_zonal(youngforest_binary1990)
print("calculate youngforest_zonal2000")
youngforest_zonal2000 <- calculate_zonal(youngforest_binary2000)
print("calculate youngforest_zonal2010")
youngforest_zonal2010 <- calculate_zonal(youngforest_binary2010)
print("calculate youngforest_zonal1992_2020")
youngforest_zonal1992_2020 <- calculate_zonal(youngforest_binary1992_2020)
  # save intermediate step
save(youngforest_zonal1990,youngforest_zonal2000,youngforest_zonal2010,
     youngforest_zonal1992_2020,
     file="./youngforest_zonal.rda")

  # old forest
print("calculate oldforest_zonal1990")
oldforest_zonal1990 <- calculate_zonal(oldforest_binary1990)
print("calculate oldforest_zonal2000")
oldforest_zonal2000 <- calculate_zonal(oldforest_binary2000)
print("calculate oldforest_zonal2010")
oldforest_zonal2010 <- calculate_zonal(oldforest_binary2010)
print("calculate oldforest_zonal1992_2020")
oldforest_zonal1992_2020 <- calculate_zonal(oldforest_binary1992_2020)
  # save intermediate step
save(oldforest_zonal1990,oldforest_zonal2000,oldforest_zonal2010,
     oldforest_zonal1992_2020,
     file="./oldforest_zonal.rda")

print("./oldforest_zonal.rda saved")

  # grassland
print("calculate grassland_zonal1990")
grassland_zonal1990 <- calculate_zonal(grassland_binary1990)
print("calculate grassland_zonal2000")
grassland_zonal2000 <- calculate_zonal(grassland_binary2000)
print("calculate grassland_zonal2010")
grassland_zonal2010 <- calculate_zonal(grassland_binary2010)
print("calculate grassland_zonal1992_2020")
grassland_zonal1992_2020 <- calculate_zonal(grassland_binary1992_2020)
  # save intermediate step
save(grassland_zonal1990,grassland_zonal2000,grassland_zonal2010,
     grassland_zonal1992_2020,
     file="./grassland_zonal.rda")

# organize zonal statistic (area fraction) results
proportion_decade_df <- data.frame(
  x = mat_1992_coords$x,
  y = mat_1992_coords$y,
  # these columns are area fractions of given land cover type without land use land cover change in given decade
  youngforest1990 = youngforest_zonal1990[,2],
  youngforest2000 = youngforest_zonal2000[,2],
  youngforest2010 = youngforest_zonal2010[,2],
  youngforest1992_2020 = youngforest_zonal1992_2020[,2],
  oldforest1990 = oldforest_zonal1990[,2],
  oldforest2000 = oldforest_zonal2000[,2],
  oldforest2010 = oldforest_zonal2010[,2],
  oldforest1992_2020 = oldforest_zonal1992_2020[,2],
  grassland1990 = grassland_zonal1990[,2],
  grassland2000 = grassland_zonal2000[,2],
  grassland2010 = grassland_zonal2010[,2],
  grassland1992_2020 = grassland_zonal1992_2020[,2]
)

# add averaged land use proportion for young and old forest 1992-2020 from decadal proportion
proportion_decade_df <- proportion_decade_df %>%
  dplyr::mutate(
    avg_youngforest1992_2020 = (8*youngforest1990+10*youngforest2000+10*youngforest2010)/28,
    avg_oldforest1992_2020 = (8*oldforest1990+10*oldforest2000+10*oldforest2010)/28
  )

# add cellsize
mat_1992_cellsize <- terra::cellSize(mat_1992, unit="ha") # calculate cell size of raster
cellsize_df <- raster2df(raster(mat_1992_cellsize)) %>% # convert to df
  rename(cellsize_ha = value)
proportion_decade_df <- merge(proportion_decade_df, cellsize_df, by=c("x","y"), all.x=T)

# add region names
# load assigned region names for each 0.5 degree pixel
# this file is provided in the "data" folder of the repo
# the column in interest is "pan_region_new2", 
# where the region name is assigned based on rcapp regions and 
# follows the divisions in Figure 1 of Pan et al. 2024 (doi: 10.1038/s41586-024-07602-x)
load("./region_division_df.rda")

proportion_decade_df <- merge(proportion_decade_df, rcapp_region_df[,c("x","y","pan_region_new2")], 
                              by=c("x","y"), all.x=T)

# save
save(proportion_decade_df, file="./proportion_decade_df.rda")

# set working directory back to root folder
setwd("..")
