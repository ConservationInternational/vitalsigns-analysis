setwd('D://Documents and Settings/mcooper/Desktop/RWA Analysis for IUCN/')

library(raster)
library(glmnet)


elev <- raster('Rwanda_SRTM30meters.tif')

pop <- raster('Rwanda_Population_Count_2015.tif')

precip <- raster('pre_mean--SSA.tif')

landcover <- raster('servir-rwanda_landcover_2010_scheme_i.tif')
lc_proj <- projectRaster(from=landcover, to=elev, method='ngb')
lc_proj[lc_proj==0] <- NA
lc_proj[lc_proj==6] <- 3

pop_rs <- resample(pop, elev)

precip_rs <- resample(precip, elev)

layers <- stack(lc_proj, pop_rs, precip_rs, elev)

#From TreeBiodiversity.R
ss <- read.csv('RWA-Biodiversity.csv')
ss <- SpatialPointsDataFrame(coords=ss[,c('longitude', 'latitude')], data=ss)

ss <- extract(layers, ss, sp=T)

mod <- lm(biodiversity~as.factor(layer) + Rwanda_Population_Count_2015, data=ss@data)

out <- predict(layers, mod)

##Try lasso regression
mat <- matrix(c(ss@data$layer==1, ss@data$layer==2, ss@data$layer==3, ss@data$layer==4, ss@data$layer==5, ss@data$Rwanda_Population_Count_2015,
              ss@data$pre_mean..SSA, ss@data$Rwanda_SRTM30meters), byrow=F, ncol=8)

mod2 <- glmnet(x=mat, y=ss@data$biodiversity, alpha=1)

##Try nonlinear regression
mod3 <- nlm(biodiversity~as.factor(layer) + Rwanda_Population_Count_2015, data=ss@data)


