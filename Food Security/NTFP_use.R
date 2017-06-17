setwd('D://Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security/')

library(aws.s3)
library(dplyr)
aws.signature::use_credentials()

ee <- read.csv('ee_export.csv')

eeag <- paste0('cci_', c('10', '11', '12', '20', '30', '200'), '.0')
eenonag <- paste0('cci_', c('40', '50', '60', '61', '62', '80', '90', 
                            '100', '110', '120', '122', '130', '160', '170', 
                            '180', '190'), '.0')

ee$ag <- rowSums(ee[ , eeag], na.rm=T)/rowSums(ee[ , c(eenonag, eeag)], na.rm=T)
ee$nonag <- rowSums(ee[ , eenonag], na.rm=T)/rowSums(ee[ , c(eenonag, eeag)], na.rm=T)

ee$forest <- rowSums(ee[ , c('for_.1.0', 'for_1.0')], na.rm=T)

ee <- ee %>% select(ag, nonag, forest, fr_prod, ag_prod, 
                    market=market_dist_hrs, pop=pop15, hh_refno) %>%
  filter(!duplicated(hh_refno))

obj <- get_object('FoodSecurity_HH.csv', bucket = 'vs-cdb-indicators')
fs <- read.csv(text = rawToChar(obj)) %>%
  filter(round=='1')

obj <- get_object('HouseholdEconomy_HH.csv', bucket = 'vs-cdb-indicators')
he <- read.csv(text = rawToChar(obj)) %>%
  filter(round=='1') %>%
  select(hh_refno, total_nonag_income, cost_expenditures)

obj <- get_object('NaturalResources_HH.csv', bucket = 'vs-cdb-indicators')
nr <- read.csv(text = rawToChar(obj)) %>%
  filter(round == '1') %>%
  select(hh_refno, food, nonfood)

obj <- get_object('Ag_HH.csv', bucket = 'vs-cdb-indicators')
ag <- read.csv(text = rawToChar(obj)) %>%
  filter(round == '1') %>%
  select(hh_refno, total_ag_production_value, country, landscape_no)


all <- Reduce(f = merge, x=list(ee, fs, he, nr, ag))

rescale <- function(x){
  x/max(x, na.rm=T)
}

library(lme4)
library(lmerTest)

resc_vars <- c('ag', 'nonag', 'forest', 'fr_prod', 'ag_prod', 'market', 'pop', 'diversity', 'Food_As_Percent_Total_Spending', 'total_nonag_income', 'cost_expenditures', 'total_ag_production_value')

for (r in resc_vars){
  all[ , r] <- rescale(all[ , r])
}


foodmod <- glmer(food ~ forest + nonag + market + pop + diversity + Food_As_Percent_Total_Spending + total_nonag_income + cost_expenditures + total_ag_production_value + 
                   (1 | landscape_no) + (1 | country), data=all, family='binomial')
summary(foodmod)


nonfoodmod <- glmer(nonfood ~ forest + nonag + market + pop + diversity + Food_As_Percent_Total_Spending + total_nonag_income + cost_expenditures + total_ag_production_value + 
                   (1 | landscape_no) + (1 | country), data=all, family='binomial')
summary(nonfoodmod)


all$any <- all$food | all$nonfood
anymod <- glmer(any ~ forest + nonag + market + pop + diversity + Food_As_Percent_Total_Spending + total_nonag_income + cost_expenditures + total_ag_production_value + 
                      (1 | landscape_no) + (1 | country), data=all, family='binomial')
summary(anymod)


library(ggplot2)
ggplot(all) + geom_histogram(aes(x=ag, fill=country))
  