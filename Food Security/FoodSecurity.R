library(lubridate)
library(dplyr)

source('../dssg-info.txt')
con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

library(aws.s3)
aws.signature::use_credentials()

read <- function(x){
  read.csv(text=rawToChar(get_object(x, bucket='vs-cdb-indicators')))
}


##Outcomes
#Ordinal FS CSI in past two weeks
#Also: Wasting

fs <- tbl(con, 'flagging__household_secI') %>% data.frame
fs <- fs[ , c('Country', 'Landscape..', 'Household.ID', 'Round', 'hh_i08', paste0('hh_i02_', seq(1,8)))]

fs$ord[fs$hh_i08=='2'] <- 1               #364
fs$ord[fs$hh_i08=='1'] <- 2               #174
fs$ord[which(fs$hh_i02_1 > 0)] <- 3       #13
fs$ord[which(fs$hh_i02_2 > 0)] <- 3       #17
fs$ord[which(fs$hh_i02_3 > 0)] <- 3       #14
fs$ord[which(fs$hh_i02_4 > 0)] <- 3       #127
fs$ord[which(fs$hh_i02_5 > 0)] <- 3       #4
fs$ord[which(fs$hh_i02_6 > 0)] <- 4       #20
fs$ord[which(fs$hh_i02_7 > 0)] <- 4       #49
fs$ord[which(fs$hh_i02_8 > 0)] <- 4       #22

fs$ord <- as.factor(fs$ord)

fs <- fs %>% select(Country, Landscape.., Household.ID, Round, ord, fs_year=hh_i08)


##Predictors
# ##Forest Cover
lc <- read.csv('hh_forest.csv')

hh <- read.csv('../../vs-indicators-calc/Combine/hh_level.csv') %>%
  select(Household.ID=Household_ID, Round, diversity, HH_Head_Gender, AgIncome, NonAgIncome, yield_quantile, CropCommercializationIndex, size, literate)
         
# Its about food security and forests is a predictor, one of many
# outcome = reporting insecuirty past year (Ordinal logit outcome) - look at rare events logistic regression
# predicot  = forest cover (break into types)
# precip = for year specified - SPI
# income/
#   agvalue/
#   yields/
#   agincome
# CCI
# labor/
#   nonfarm/
#   if they collected any of each
# diet diversity
# hhsize,
# drop RWA round 2


# NR Use
#If they collect each item
#Too hard to quantify for now
nr <- tbl(con, 'flagging__household_secHV2') %>%
  dplyr::select(WildMeat=hv2_10_01, WildInsects=hv2_10_02, Fish=hv2_10_03, NutsSeeds=hv2_10_04, 
         BuildingMaterials=hv2_10_05, MedicinalPlants=hv2_10_06, CeremonialItems=hv2_10_07,
         Honey=hv2_10_08, Other=hv2_10_09, Round, `Household ID`) %>%
  data.frame()

sel <- c("WildMeat", "WildInsects", "Fish", "NutsSeeds", "BuildingMaterials", 
         "MedicinalPlants", "CeremonialItems", "Honey", "Other")

nr[ , sel][is.na(nr[ , sel])] <- 2
nr[ , sel] <- nr[ , sel] == '1'

mkt <- read.csv('Market_hh.csv')

# SPI
load('hh_spi.RData')
hh_spi$date <- ymd(hh_spi$date)


# Months
date <- tbl(con, 'agric') %>% select(Household.ID=hh_refno, Round=round, date=date_of_interview) %>% data.frame
date$date <- ymd(date$date) %>% round_date(unit='month')
date <- date %>% filter(!is.na(date$date))

#Combining
spi_date <- merge(date, hh_spi, all.x=T, all.y=F)

df <- Reduce(merge, list(fs, lc, hh, nr, spi_date, mkt))

df <- df %>% filter(Round == 1) %>% na.omit

library(ordinal)

rescale <- c('AgIncome', 'NonAgIncome', 'size', 'literate', 'spi12', 'yield_quantile', 'market')

df[ , rescale] <- df[ , rescale]/apply(df[ , rescale], 2, max)

mod <- clmm(ord ~ layer + #diversity + 
              HH_Head_Gender + AgIncome + NonAgIncome + size + literate + spi12 + spi6 + spi24 + spi36 +
              yield_quantile + CropCommercializationIndex + WildMeat + WildInsects + Fish + BuildingMaterials + MedicinalPlants + 
              CeremonialItems + Honey + Other + market +
              (1|Landscape..) + (1|Country), data=df, link = "logit", Hess=T)
summary(mod)

df$any <- df$ord != '1'

library(lme4)
library(lmerTest)

mod2 <- glmer(any ~ layer + #diversity + 
               HH_Head_Gender + AgIncome + NonAgIncome + size + literate + spi12 + spi6 + spi24 + spi36 +
               yield_quantile + CropCommercializationIndex + WildMeat + WildInsects + Fish + BuildingMaterials + MedicinalPlants + 
               CeremonialItems + Honey + Other + market +
               (1|Landscape..) + (1|Country), data=df, link = "logit", family='binomial')
summary(mod2)


df$fs_year <- df$fs_year == "2"
mod3 <- glmer(fs_year ~ layer + #diversity + 
                 HH_Head_Gender + AgIncome + NonAgIncome + size + literate + spi12 + spi6 + spi24 + spi36 +
                 yield_quantile + CropCommercializationIndex + WildMeat + WildInsects + Fish + BuildingMaterials + MedicinalPlants + 
                 CeremonialItems + Honey + Other + market +
                 (1|Landscape..) + (1|Country), data=df, link = "logit", family='binomial')
summary(mod3)



#notes from Alex - check SPI at date of harvest
#value of agriculture
#population density
#mask out protected areas

