# Rwanda "CPR" cropland soil profile locations (2018)
# Soil profile location data courtesy of Rwanda Agricultural Bureau (RAB) 
# M. Walsh, November 2020

# install.packages(c("rgdal","raster","leaflet","htmlwidgets"), dependencies=T)
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
# note I have dropped 5 of the origin profile locations because their georeference fell outside of Rwanda
download.file("https://osf.io/y7bts?raw=1", "RW_soil_profiles.csv")
sprof <- read.table("RW_soil_profiles.csv", header = T, sep = ",")

# download GADM-L5 shapefile (courtesy: http://www.gadm.org)
download.file("https://www.dropbox.com/s/fhusrzswk599crn/RWA_level5.zip?raw=1", "RWA_level5.zip")
unzip("RWA_level5.zip", overwrite=T)
shape <- shapefile("gadm36_RWA_5.shp")

# download GeoSurvey prediction layers
download.file("https://osf.io/u73pd?raw=1", "RW_GS_preds.zip")
unzip("RW_GS_preds.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# Data setup --------------------------------------------------------------
# attach GADM-L5 admin unit names from admin shape
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

# Define unique grid ID's (gid) -------------------------------------------
# Specify pixel scale (res.pixel, in m)
res.pixel <- 10000

# grid ID (gid) definition
xgid <- ceiling(abs(sprof$x)/res.pixel)
ygid <- ceiling(abs(sprof$y)/res.pixel)
gidx <- ifelse(sprof$x<0, paste("W", xgid, sep=""), paste("E", xgid, sep=""))
gidy <- ifelse(sprof$y<0, paste("S", ygid, sep=""), paste("N", ygid, sep=""))
gid <- paste(gidx, gidy, sep="")
sprof <- cbind(sprof, gid)

# Write data frame --------------------------------------------------------
# extract cropland mask at soil profile locations
coordinates(sprof) <- ~x+y
projection(sprof) <- projection(grids)
sprofgrid <- extract(grids, sprof)
spdat <- as.data.frame(cbind(sprof, sprofgrid))
spdat <- spdat[ which(spdat$CM20==1), ] ## selects profiles in only cropland mask locations
spdat <- spdat[c(1:5,11:12,15)] ## selects columns compatible with the GeoSurvey sampling frame
write.csv(spdat, "./soil_profiles.csv", row.names = F)

# Map widget --------------------------------------------------------------
# number of soil profiles
w <- leaflet() %>%
  setView(lng = mean(spdat$lon), lat = mean(spdat$lat), zoom = 9) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(spdat$lon, spdat$lat, clusterOptions = markerClusterOptions())
w ## plot widget 
saveWidget(w, 'RW_SP18.html', selfcontained = T) ## save widget

