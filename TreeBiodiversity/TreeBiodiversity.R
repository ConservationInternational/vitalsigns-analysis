library(dplyr)
detach("package:raster", unload=TRUE)

setwd('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/TreeBiodiversity')

source('../production_connection.R')
#source('../local_connection.R')

con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

shannon_entropy <- function(vect, base=exp(1)){
  vect <- vect[!is.na(vect)]
  tb <- table(vect)
  p <- tb/sum(tb)
  H <- -sum(p*log(p, base=base))
  return(H)
}

random_round <- function(vect, freq){
  n <- length(vect)*freq
  remainder <- n%%1
  true <- rep(TRUE, remainder*1000)
  false <- rep(FALSE, (1-remainder)*1000)
  value <- sample(c(true, false), 1)
  
  if (value){
    n <- ceiling(n)
  }else{
    n <- floor(n)
  }
  
  return(sample(vect, n))
}

bootstrap_diversity <- function(vector, freq, iter){
  freq <- unique(freq)
  
  ents <- NULL
  
  for (j in 1:iter){
    samp <- random_round(vector, freq)
    ents <- c(ents, shannon_entropy(samp))
  }
  
  return(mean(ents))
}

#The area of the plots are not the same, so we have to somehow adjust that.  Restrict to smallest plot size?
eplot <- tbl(con, 'flagging__eplot') %>% select(survey_uuid, Country, `Landscape #`,  `Eplot #`, latitude, longitude, `Subplot Radius`, flag) %>% data.frame %>%
  filter(!grepl('Subplot Radius', flag) & !is.na(Subplot.Radius))

radii <- unique(eplot$Subplot.Radius)
area <- pi*unique(eplot$Subplot.Radius)^2
ratio <- pi*2^2/area

subplot <- data.frame(Subplot.Radius=radii, area, ratio)

eplot <- merge(eplot, subplot)

species <- tbl(con, 'flagging__eplot_woody_plant') %>% select(survey_uuid, `Landscape #`, `Eplot #`, Country, Genus, Species) %>% data.frame

species <- merge(eplot, species)

species <- species %>% filter(Genus != 'Musa')

species_sum <- species %>% group_by(survey_uuid, Landscape.., Eplot.., latitude, longitude, Country) %>% 
  summarize(biodiversity=bootstrap_diversity(vector=paste0(Genus, Species), freq=ratio, iter=500))

# library(maps)
# library(mapdata)

# color.gradient <- function(x, colors=c("red","yellow","green"), colsteps=100) {
#   return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
# }
# 
# species_sum$color <- color.gradient(species_sum$biodiversity)
# 
# colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
# 
# plot(species_sum$longitude, species_sum$latitude, col=species_sum$color, 
#      xlim=c(30,40), ylim=c(-10.5,-6), xlab='', ylab='', axes = F)
# map('worldHires', 'Tanzania', add=T)

library(sp)
library(gstat)
library(raster)

species_sum <- as.data.frame(species_sum)
ss <- SpatialPointsDataFrame(coords=species_sum[species_sum$Country=='TZA' , c('longitude', 'latitude')], data=species_sum[species_sum$Country=='TZA', ])

x.range <- range(ss$longitude)
y.range <- range(ss$latitude)

x<-seq(x.range[1]-3, x.range[2]+3, length.out=250)
y<-seq(y.range[1]-3, y.range[2]+3, length.out=250)
grd<-expand.grid(x,y)

coordinates(grd) <- ~ Var1+Var2
gridded(grd) <- TRUE

proj4string(ss) <- CRS("+proj=longlat +datum=WGS84")
proj4string(grd) <- CRS("+proj=longlat +datum=WGS84")

write.csv(ss@data, 'RWA-Biodiversity.csv', row.names=F)

IDW <- gstat::idw(biodiversity~1, locations=ss, newdata=grd, idp=2.5)

plot(IDW)

#TZ <- readRDS('UGA_adm1.rds')
#TZ <- TZ[TZ$NAME_1!="Lake Victoria", ]

TZ <- readRDS('TZA_adm0.rds')

fc <- focal(raster(IDW), fun=mean, w=matrix(rep(1,49), nrow=7), pad=T)
cp <- mask(fc, TZ)
cp <- crop(cp, extent(ss))


plot(cp, labels=F, xaxt='n', yaxt='n',
     legend.args=list(text='Tree Biodiversity (Shannon Diversity Index)', side=4, font=2, line=2.5, cex=0.8))
plot(TZ, add=T)

library(rgdal)
writeRaster(cp, filename = 'TZA_Biodiversity.tif', format='GTiff')

write.csv(species_sum, 'Biodiversity.Eplot.csv', row.names=F)

############################
#Bring in landcover type
############################

species_sum$eplot_code <- with(species_sum, paste0(Country, '-', Landscape.., '-', Eplot..))

eplot_id <- tbl(con, 'eplot_id') %>% data.frame
eplot_id$eplot_code <- with(eplot_id, paste0(country, '-', landscape_no, '-', eplot_no))

species_lc <- merge(species_sum, eplot_id, by='eplot_code')


###############################
#Bring in soil nutrients
###############################

eplotsoils <- tbl(con, 'flagging__eplotsoils_processed') %>% data.frame %>%
  select(Eplot.Code, Country, Nitrogen = Nitrogen.content.for.acid.treated.sample.to.remove.carbonates..,
         Phosphorous=Phosphorus.by.Mehlich.3.extraction..mg.kg..1.,
         Potassium = Potassium.concentration.by.Mehlich.3.extraction..mg.kg..1.,
         Carbon = Carbon.content.for.acid.treated.sample.to.remove.carbonates....,
         Carbon2 = Total.Carbon.content....by.weight.,
         Nitrogen2 = Total.Nitrogen.content....by.weight.,
         Phosphorus2 = P.sorption.index..Bache.and.Williams.1971.)


eplot <- merge(species_lc, eplotsoils, by.x='eplot_code', by.y='Eplot.Code')

lm(biodiversity~Potassium + Phosphorous + Carbon + Nitrogen, data=eplot) %>% summary


library(maps)
library(mapdata)

color.gradient <- function(x, colors=c("red","yellow","green"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

eplot$color <- color.gradient(eplot$Potassium)

colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))

plot(eplot$longitude, eplot$latitude, col=eplot$color, 
     xlim=c(30,40), ylim=c(-10.5,-6), xlab='', ylab='', axes = F)
map('worldHires', 'Tanzania', add=T)
