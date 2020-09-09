# Spatially balanced sampling setup for soil and cropping system surveys of Rwanda
# M. Walsh, August 2020

# install.packages(c("rgdal","raster","BalancedSampling","leaflet","htmlwidgets"), dependencies=T)
suppressPackageStartupMessages({
  require(rgdal)
  require(raster)
  require(sp)
  require(BalancedSampling)
  require(leaflet)
  require(htmlwidgets)
})

# Data setup --------------------------------------------------------------
# create a data folder in your current working directory
dir.create("RW_MS_sample", showWarnings=F)
setwd("./RW_MS_sample")

# download GADM-L5 shapefile (courtesy: http://www.gadm.org)
download.file("https://www.dropbox.com/s/fhusrzswk599crn/RWA_level5.zip?raw=1", "RWA_level5.zip")
unzip("RWA_level5.zip", overwrite=T)
shape <- shapefile("gadm36_RWA_5.shp")

# download cropland mask
download.file("https://osf.io/bmysp?raw=1", "RW_CP_mask.zip")
unzip("RW_CP_mask.zip", overwrite=T)
glist <- list.files(pattern="tif", full.names=T)
grids <- stack(glist)

# Sample setup ------------------------------------------------------------
# create a ROI image based on ROI mask
cpt <- 1    ## set land mask to 1
roi <- overlay(grids, fun=function(x) {return(ifelse(x[1] >= cpt, 1, 0))})
plot(roi, axes=F, legend=F)

# extract ROI coordinates
coord <- coordinates(roi)
index <- extract(roi, coord)
index <- as.data.frame(cbind(coord, index))
rmask <- index[which(index$index == 1),]

# Spatially balanced sampling ---------------------------------------------
# set sampling parameters
N <- nrow(rmask) ## ROI size (in 250 m pixels)
n <- round(N/16*0.15,0) ## set sample size (number of sampling locations)
p <- rep(n/N,N)  ## inclusion probabilities

# draw geographically balanced sample
set.seed(6405)                      ## sets repeatable randomization seed
B <- cbind(p, rmask[,1], rmask[,2]) ## specifies balancing variables
rsamp <- cube(p, B)                 ## samples from population

# Write files -------------------------------------------------------------
# extract sample coordinates
x <- rmask[rsamp,1]
y <- rmask[rsamp,2]
xy <- data.frame(cbind(x,y))

# Define unique grid ID's (GID)
# Specify pixel scale (res.pixel, in m)
res.pixel <- 1000

# Grid ID (GID) definition
xgid <- ceiling(abs(xy$x)/res.pixel)
ygid <- ceiling(abs(xy$y)/res.pixel)
gidx <- ifelse(xy$x<0, paste("W", xgid, sep=""), paste("E", xgid, sep=""))
gidy <- ifelse(xy$y<0, paste("S", ygid, sep=""), paste("N", ygid, sep=""))
GID <- paste(gidx, gidy, sep="")
xy <- cbind(GID, xy)

# attach GADM-L5 and above unit names from shape
coordinates(xy) <- ~x+y
crs(xy) <- "+proj=laea +ellps=WGS84 +lon_0=20 +lat_0=5 +units=m +no_defs"
sloc <- spTransform(xy, CRS(proj4string(shape)))
gadm <- sloc %over% shape
sloc <- as.data.frame(sloc)
samp <- cbind(gadm[ ,c(4,6,8,10,12)], sloc)
colnames(samp) <- c("province","district","sector","cell","village","gid","lon","lat")
write.csv(samp, "RW_MS_sample.csv", row.names = F)

# Sampling map widget -----------------------------------------------------
# render map
w <- leaflet() %>%
  setView(lng = mean(samp$lon), lat = mean(samp$lat), zoom = 9) %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
  addCircleMarkers(samp$lon, samp$lat, clusterOptions = markerClusterOptions())
w ## plot widget 
saveWidget(w, 'RW_MS_sample.html', selfcontained = T) ## save widget
