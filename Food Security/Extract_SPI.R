library(gdalUtils)
#need to instal GDAL
library(ggplot2)
library(RPostgreSQL)
library(raster)
library(dplyr)
library(SPEI)
library(foreach)
library(lubridate)

setwd('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security')

source('../production_connection.R')
con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

hh <- tbl(con, 'sp_hh') %>% 
  data.frame %>% 
  filter(inside_country=='') %>% 
  dplyr::select(gpsse_lat, gpsse_long, Household.ID=hh_refno, Round=round)

in_folder <- 'D://Documents and Settings/mcooper/Desktop/CHIRPS/'
tifs <- dir(in_folder, pattern='.tif$')
vrt_file <- extension(rasterTmpFile(), 'vrt')
gdalbuildvrt(paste0(in_folder, '*.tif'), vrt_file, separate=TRUE, verbose=T,
             overwrite=TRUE)

hh_spi <- foreach(n=1:nrow(hh), .combine=rbind) %do% {
  s <- gdallocationinfo(vrt_file, hh$gpsse_long[n], hh$gpsse_lat[n],
                        wgs84=TRUE, valonly=TRUE)
  print(n)
  s <- as.numeric(s)
  data.frame(Household.ID=hh$Household.ID[n],
             Round=hh$Round[n],
             date=seq(ymd('1981-01-01'), ymd('2017-02-01'), by='1 month'),
             spi6=as.numeric(spi(s, 6, na.rm=TRUE)$fitted),
             spi12=as.numeric(spi(s, 12, na.rm=TRUE)$fitted),
             spi24=as.numeric(spi(s, 24, na.rm=TRUE)$fitted),
             spi36=as.numeric(spi(s, 36, na.rm=TRUE)$fitted))
}

save(hh_spi, file='hh_spi.RData')
