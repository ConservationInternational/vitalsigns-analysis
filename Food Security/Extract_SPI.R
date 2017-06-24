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

ls <- tbl(con, 'landscape') %>%
  filter(country != 'KEN' & landscape_no != '000') %>%
  dplyr::select(country, landscape_no, centerpoint_latitude, centerpoint_longitude) %>%
  collect

in_folder <- 'D://Documents and Settings/mcooper/Desktop/CHIRPS/'
tifs <- dir(in_folder, pattern='.tif$')
vrt_file <- extension(rasterTmpFile(), 'vrt')
gdalbuildvrt(paste0(in_folder, '*.tif'), vrt_file, separate=TRUE, verbose=T,
             overwrite=TRUE)

ls_spi <- foreach(n=1:nrow(ls), .combine=rbind) %do% {
  s <- gdallocationinfo(vrt_file, ls$centerpoint_longitude[n], ls$centerpoint_latitude[n],
                        wgs84=TRUE, valonly=TRUE)
  print(n)
  s <- as.numeric(s)
  data.frame(country=ls$country[n],
             landscape_no = ls$landscape_no[n],
             date=seq(ymd('1981-01-01'), ymd('2017-02-01'), by='1 month'),
             spi6=as.numeric(spi(s, 6, na.rm=TRUE)$fitted),
             spi12=as.numeric(spi(s, 12, na.rm=TRUE)$fitted),
             spi24=as.numeric(spi(s, 24, na.rm=TRUE)$fitted),
             spi36=as.numeric(spi(s, 36, na.rm=TRUE)$fitted))
}

ls_spi <- ls_spi %>% filter(date > ymd('2011-01-01'))

save(ls_spi, file='ls_spi.RData')
