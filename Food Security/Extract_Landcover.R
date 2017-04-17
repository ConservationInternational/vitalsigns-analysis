library(dplyr)
detach("package:raster", unload=TRUE)

setwd('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security')

source('../production_connection.R')
con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

hh <- tbl(con, 'sp_hh') %>% 
  data.frame %>% 
  filter(inside_country=='') %>% 
  dplyr::select(gpsse_lat, gpsse_long, Household.ID=hh_refno, Round=round)

library(raster)

r <- raster('D:/Documents and Settings/mcooper/Desktop/Landscapes/ESACCI-LC-L4-LCCS-Map-300m-P5Y-2010-v1.6.1.tif')
r <- crop(r, extent(-3.6, 40, -11, 37.5))
rcl <- reclassify(r, rcl = matrix(c(10, 20, 30, 190, 200, 11,	12,	40,	50,	60,	61,	62,	70,	71,	72,	80,	81,	82,	90,	100,	110,	120,	121,	122,	130,	140,	150,	152,	153,	160,	170,	180,	201,	202,	210,	220,
                                    0,   0,  0,   0,   0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,   1,    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,    1),
                                  nrow=36, byrow = FALSE))

rclp <- projectRaster(rcl, crs = CRS('+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))

sp <- SpatialPointsDataFrame(coords = hh[ , c('gpsse_long', 'gpsse_lat')], data = hh, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spp <- spTransform(sp, CRS('+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))

ext <- extract(rclp, spp, fun=mean, buffer=7500, sp=TRUE)

data <- ext@data %>% dplyr::select(layer, Household.ID, Round)

write.csv(data, 'Landcover_hh.csv', row.names=F)
