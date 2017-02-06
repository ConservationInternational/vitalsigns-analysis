library(dplyr)

setwd('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/ValueOfNature/')

source('../production_connection.R')
#source('../local_connection.R')

con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

flagFilter <- function(df){
  #df must be a data frame (not a tbl), with a 'flag' column
  
  patterns <- names(df) %>% paste0(collapse='|')
  df <- df[!grepl(pattern = patterns, df$flag),]
  
  df <- df[ , names(df) != 'flag']
  
  df
}


#Firewood, somehow

#Natural Resource Value
hv2.1 <- tbl(con, 'flagging__household_secHV2') %>% 
  select(Country, `Landscape #`, hv2_14_01, hv2_14_02, hv2_14_03, hv2_14_04,
         hv2_14_05, hv2_14_06, hv2_14_07, hv2_14_08, hv2_14_09) %>%
  data.frame
  
hv2.1$sum <- rowSums(hv2.1[ , paste0('hv2_14_0', seq(1,9))], na.rm=T)
  
hv2.1 <- hv2.1 %>% group_by(Country, Landscape..) %>% summarize(Value=sum(sum, na.rm=T))
###need to normalize by US currency?

#Natural Resources Trajectories
hv2.2 <- tbl(con, 'flagging__household_secHV2') %>% 
  select(Country, `Landscape #`, hv2_15_01, hv2_15_02, hv2_15_03, hv2_15_04,
         hv2_15_05, hv2_15_06, hv2_15_07, hv2_15_08, hv2_15_09) %>%
  data.frame

hv2.2$hv2_15_01 <- as.numeric(hv2.2$hv2_15_01)
hv2.2$hv2_15_02 <- as.numeric(hv2.2$hv2_15_02)
hv2.2$hv2_15_03 <- as.numeric(hv2.2$hv2_15_03)
hv2.2$hv2_15_04 <- as.numeric(hv2.2$hv2_15_04)
hv2.2$hv2_15_05 <- as.numeric(hv2.2$hv2_15_05)
hv2.2$hv2_15_06 <- as.numeric(hv2.2$hv2_15_06)
hv2.2$hv2_15_07 <- as.numeric(hv2.2$hv2_15_07)
hv2.2$hv2_15_08 <- as.numeric(hv2.2$hv2_15_08)
hv2.2$hv2_15_09 <- as.numeric(hv2.2$hv2_15_09)

hv2.2$mean <- rowMeans(hv2.2[ , paste0('hv2_15_0', seq(1,9))], na.rm=T)

hv2.2 <- hv2.2 %>% group_by(Country, Landscape..) %>% summarize(Value=mean(mean, na.rm=T))

#Fuelwood Distance