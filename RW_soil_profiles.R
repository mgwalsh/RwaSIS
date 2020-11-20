# Rwanda "CPR" soil profiles (~2018)
# Soil profile location data courtesy of Rwanda Agricultural Bureau (RAB) 
# M. Walsh, November 2020

# install.packages(c("rgdal","raster","BalancedSampling","leaflet","htmlwidgets"), dependencies=T)
suppressPackageStartupMessages({
  require(rgdal)
  require(raster)
  require(sp)
  require(leaflet)
  require(htmlwidgets)
})

# Data setup --------------------------------------------------------------
# create a data folder in your current working directory
dir.create("RW_soil_profiles", showWarnings=F)
setwd("./RW_soil_profiles")

# download CPR profile locations
download.file("https://osf.io/y7bts?raw=1", "RW_soil_profiles.csv")
sprof <- read.table("RW_soil_profiles.csv", header = T, sep = ",")

# download GADM-L5 shapefile (courtesy: http://www.gadm.org)
download.file("https://www.dropbox.com/s/fhusrzswk599crn/RWA_level5.zip?raw=1", "RWA_level5.zip")
unzip("RWA_level5.zip", overwrite=T)
shape <- shapefile("gadm36_RWA_5.shp")

# download current GeoSurvey cropland mask
download.file("https://osf.io/bmysp?raw=1", "RW_CP_mask.zip")
unzip("RW_CP_mask.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)