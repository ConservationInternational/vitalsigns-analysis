setwd('D://Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security/WDPA_June2017-shapefile/')

library(rgdal)
library(raster)

sp <- readOGR(dsn='.', layer='WDPA_AfriClip')

sp_sel <- sp[sp@data$IUCN_CAT %in% c('Ia', 'Ib', 'II', 'III', 'IV') & 
               sp@data$ISO3 %in% c('TZA', 'UGA', 'RWA', 'GHA'), ]

sp_sel@data$field <- 1

r <- raster('D:/Documents and Settings/mcooper/Documents/VS Kenya/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7/scratch/ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif')

r_crop <- crop(r, extent(sp_sel))

out <- rasterize(sp_sel, r_crop, field='field')

out[is.na(out)] <- 0

exp <- extent(-4.2, xmax=40.18, ymin=-11.5, ymax=11.1)

new <- extend(out, exp, value=0)

writeRaster(new, 'protectedareas.tif', format='GTiff', overwrite=T)
