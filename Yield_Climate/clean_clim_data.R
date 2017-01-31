library(ggplot2)
library(RPostgreSQL)
library(raster)
library(dplyr)
library(SPEI)
library(foreach)

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

chirps <- stack('O:/Data/CHIRPS-2.0/global-monthly-combined/CHIRPS_global_monthly_198101-201412.tif')

ls_sp <- SpatialPointsDataFrame(cbind(landscapes$lon, landscapes$lat),
                                as.data.frame(landscapes),
                                proj4string=CRS(proj4string(chirps)))

ls_ppt <- extract(chirps, ls_sp, df=TRUE)

ls_spi <- foreach(n=1:nrow(ls_sp), .combine=rbind) {
    spi(as.numeric(ls_sp[n, :]), 12, na.rm=TRUE)$fitted
}

save(ls_spi, file='ls_spi.RData')
