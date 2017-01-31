library(ggplot2)
library(RPostgreSQL)
library(raster)
library(dplyr)
library(SPEI)
library(foreach)
library(lubridate)

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)
vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)

landscapes <- tbl(vs_db, 'landscape') %>%
    dplyr::filter(!is.na(centerpoint_longitude), !is.na(centerpoint_latitude),
           !(country %in% c('KEN', 'GHA'))) %>%
    dplyr::select(country, ls_id=landscape_no,
           lon=centerpoint_longitude, lat=centerpoint_latitude) %>%
    collect()
write.csv(landscapes, file='landscapes.csv', row.names=FALSE)

in_folder <- 'O:/Data/CHIRPS-2.0/global-monthly'
tifs <- dir(in_folder, pattern='.tif$')
vrt_file <- extension(rasterTmpFile(), 'vrt')
gdalbuildvrt(file.path(in_folder, tifs), vrt_file, separate=TRUE, 
             overwrite=TRUE)

ls_spi <- foreach(n=1:nrow(landscapes), .combine=rbind) %do% {
    s <- gdallocationinfo(vrt_file, landscapes$lon[n], landscapes$lat[n],
                          wgs84=TRUE, valonly=TRUE)
    s <- as.numeric(s)
    data.frame(country=landscapes$country[n],
          ls_id=landscapes$ls_id[n],
          date=seq(ymd('1981-01-01'), ymd('2016-06-01'), by='1 month'),
          spi6=as.numeric(spi(s, 12, na.rm=TRUE)$fitted),
          spi12=as.numeric(spi(s, 12, na.rm=TRUE)$fitted),
          spi24=as.numeric(spi(s, 12, na.rm=TRUE)$fitted),
          spi36=as.numeric(spi(s, 12, na.rm=TRUE)$fitted))
}

save(ls_spi, file='ls_spi.RData')
