library(dplyr)
detach("package:raster", unload=TRUE)

setwd('D://Documents and Settings/mcooper/Documents/Food Security Paper/')

library(rgdal)
library(raster)
library(RCurl)

r <- raster('ESACCI-LC-L4-LCCS-Map-300m-P5Y-2010-v1.6.1.tif')
r <- crop(r, extent(-3.6, 40, -11, 37.5))
r <- projectRaster(r, crs = CRS('+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))

hs <- list.files(pattern = 'Hansen')
hs <- lapply(X=hs, FUN=raster)
h <- Reduce(function(x, y) merge(x, y, fun=max), hs)
h <- projectRaster(h, crs = CRS('+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))




source('production_connection.R')
con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

hh <- tbl(con, 'household') %>% 
  data.frame %>%
  dplyr::select(gpsse_lat, gpsse_long, hh_refno, round)



sp <- SpatialPointsDataFrame(coords = hh[ , c('gpsse_long', 'gpsse_lat')], data = hh, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spp <- spTransform(sp, CRS('+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))

ext <- extract(rclp, spp, fun=mean, buffer=7500, sp=TRUE)

data <- ext@data %>% dplyr::select(layer, Household.ID, Round)

write.csv(data, 'Landcover_hh.csv', row.names=F)