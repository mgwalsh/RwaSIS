# Rwanda wet chemistry soil data setup
# M. Walsh, July 2020

# Required packages
suppressPackageStartupMessages({
  require(downloader)
  require(rgdal)
  require(raster)
  require(leaflet)
  require(htmlwidgets)
})

# Data downloads -----------------------------------------------------------
# Create a data folder in  your current working directory
dir.create("Wetchem", showWarnings=F)
setwd("./Wetchem")

# download soil data
download("https://osf.io/djcfz?raw=1", "RW_wetchem.zip", mode = "wb")
unzip("RW_wetchem.zip", overwrite = T)
prof <- read.table("Profiles.csv", header = T, sep = ",")
samp <- read.table("Samples.csv", header = T, sep = ",")
geos <- merge(prof, samp, by="pid")

# download raster stack
download("https://osf.io/hp6v7?raw=1", "RW_250m_2020.zip", mode = "wb")
unzip("RW_250m_2020.zip", overwrite = T)
download("https://osf.io/u73pd?raw=1", "RW_GS_preds.zip", mode = "wb")
unzip("RW_GS_preds.zip", overwrite = T)
glist <- list.files(pattern="tif", full.names = T)
grids <- stack(glist)

# Data setup --------------------------------------------------------------
# project GeoSurvey coords to grid CRS
geos.proj <- as.data.frame(project(cbind(geos$lon, geos$lat), "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"))
colnames(geos.proj) <- c("x","y")
geos <- cbind(geos, geos.proj)
coordinates(geos) <- ~x+y
projection(geos) <- projection(grids)

# extract gridded variables at survey locations
geosgrid <- extract(grids, geos)
gsdat <- as.data.frame(cbind(geos, geosgrid))

# Write data frame --------------------------------------------------------
dir.create("Results", showWarnings = F)
write.csv(gsdat, "./Results/RW_soil_data.csv", row.names = F)

# Soil sample locations ---------------------------------------------------
w <- leaflet() %>%
  setView(lng = mean(gsdat$lon), lat = mean(gsdat$lat), zoom = 9) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(gsdat$lon, gsdat$lat, clusterOptions = markerClusterOptions())
w ## plot widget 
saveWidget(w, 'RW_soil_sample_locs.html', selfcontained = T) ## save widget

