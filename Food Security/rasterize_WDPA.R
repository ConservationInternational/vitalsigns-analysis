setwd('D://Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security/WDPA_June2017-shapefile/')

library(rgdal)
library(raster)

sp <- readOGR(dsn='.', layer='WDPA_AfriClip')

sp_sel <- sp[!sp@data$IUCN_CAT %in% c('V', 'VI'), ]

sp_sel@data$field <- 1

r <- raster('D://Documents and Settings/mcooper/Desktop/Landscapes/ESACCI-LC-L4-LCCS-Map-300m-P5Y-2010-v1.6.1.tif/ESACCI-LC-L4-LCCS-Map-300m-P5Y-2010-v1.6.1.tif')

r_crop <- crop(r, extent(sp_sel))

out <- rasterize(sp_sel, r_crop, field='field')

writeRaster(out, 'protectedareas.tif', format='GTiff')
