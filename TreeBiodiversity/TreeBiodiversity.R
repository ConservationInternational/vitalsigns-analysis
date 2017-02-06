library(dplyr)

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
eplot <- tbl(con, 'flagging__eplot') %>% select(survey_uuid, `Subplot Radius`, flag) %>% data.frame %>%
  filter(!grepl('Subplot Radius', flag) & !is.na(Subplot.Radius))

radii <- unique(eplot$Subplot.Radius)
area <- pi*unique(eplot$Subplot.Radius)^2
ratio <- pi*2^2/area

subplot <- data.frame(Subplot.Radius=radii, area, ratio)

eplot <- merge(eplot, subplot)

species <- tbl(con, 'flagging__eplot_woody_plant') %>% data.frame

species <- merge(eplot, species)

species_sum <- species %>% group_by(survey_uuid, latitude, longitude) %>% 
  summarize(biodiversity=bootstrap_diversity(vector=paste0(Genus, Species), freq=ratio, iter=5000))






