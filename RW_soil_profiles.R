# Rwanda "CPR" soil profile locations (2018)
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

# create a data folder in your current working directory
dir.create("RW_soil_profiles", showWarnings=F)
setwd("./RW_soil_profiles")

# Data downloads ----------------------------------------------------------
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

# Data setup --------------------------------------------------------------
# attach GADM-L5 admin unit names from shape
coordinates(sprof) <- ~lon+lat
projection(sprof) <- projection(shape)
gadm <- sprof %over% shape
sprof <- as.data.frame(sprof)
sprof <- cbind(gadm[ ,c(4,6,8,10,12)], sprof)
colnames(sprof) <- c("region","district","sector","cell", "village","fid","profils","profils_id","profils_num","profils_type","lon","lat","planchette","commune")

# project soil profile coords to grid CRS
sprof.proj <- as.data.frame(project(cbind(sprof$lon, sprof$lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(sprof.proj) <- c("x","y")
sprof <- cbind(sprof, sprof.proj)
coordinates(sprof) <- ~x+y
projection(sprof) <- projection(grids)

# extract cropland mask at soil profile locations
sprofgrid <- extract(grids, sprof)
spdat <- as.data.frame(cbind(sprof, sprofgrid))

# Map widget --------------------------------------------------------------
# number of soil profiles
w <- leaflet() %>%
  setView(lng = mean(spdat$lon), lat = mean(spdat$lat), zoom = 9) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(spdat$lon, spdat$lat, clusterOptions = markerClusterOptions())
w ## plot widget 
saveWidget(w, 'RW_SP18.html', selfcontained = T) ## save widget


