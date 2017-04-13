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
r <- raster('TT_50K--SSA.asc')

sp <- SpatialPointsDataFrame(coords = hh[ , c('gpsse_long', 'gpsse_lat')], data = hh, proj4string = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
spp <- spTransform(sp, CRS('+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))

ext <- extract(r, spp, sp=TRUE)

data <- ext@data %>% dplyr::select(market=TT_50K..SSA, Household.ID, Round)

write.csv(data, 'Market_hh.csv', row.names=F)
