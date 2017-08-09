setwd('D://Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security/')


#TODO add hh size, head gender, age, education, 
#Exclude areas with bad GPS points (outside landscape and country)!
#Add Fuelwood 

#Framing stats for increasing/decreasing and overal vallue


library(aws.s3)
library(dplyr)
library(lubridate)
library(lme4)
library(lmerTest)
detach("package:raster", unload=TRUE)
aws.signature::use_credentials()

source('../production_connection.R')
#source('../local_connection.R')

con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

df <- tbl(con, 'c__household') %>%
  filter(round=='1') %>%
  select(country, landscape_no, hh_refno, hh_interview_date) %>%
  collect

ee <- read.csv('ee_export5.csv')

eeag <- paste0('cci_', c('10', '11', '12', '20', '30', '200'))
eenonag <- paste0('cci_', c('40', '50', '60', '61', '62', '80', '90', 
                            '100', '110', '120', '122', '130', '160', '170', 
                            '180', '190'))
eeagpa <- paste0('cciPA_', c('10', '11', '12', '20', '30', '200'), '.0')
eenonagpa <- paste0('cciPA_', c('40', '50', '60', '61', '62', '80', '90', 
                            '100', '110', '120', '122', '130', '160', '170', 
                            '180', '190'), '.0')

ee$ag <- rowSums(ee[ , eeag], na.rm=T)/rowSums(ee[ , c(eenonag, eeag)], na.rm=T)
ee$nonag <- rowSums(ee[ , eenonag], na.rm=T)/rowSums(ee[ , c(eenonag, eeag)], na.rm=T)

ee$agpa <- rowSums(ee[ , eeagpa], na.rm=T)/rowSums(ee[ , c(eenonagpa, eeagpa)], na.rm=T)
ee$nonagpa <- rowSums(ee[ , eenonagpa], na.rm=T)/rowSums(ee[ , c(eenonagpa, eeagpa)], na.rm=T)

ee$forest <- rowSums(ee[ , c('for_.1', 'for_1')], na.rm=T)
ee$forestpa <- rowSums(ee[ , c('forPA_.1.0', 'forPA_1.0')], na.rm=T)

ee <- ee %>% select(ag, nonag, forest, fr_prod, ag_prod,
                    agpa, nonagpa, forestpa,
                    market=market_dist_hrs, pop=pop15, hh_refno) %>%
  filter(!duplicated(hh_refno))

#TODO add hh size, head gender, age, education, 
ge <- tbl(con, 'c__household_individual') %>%
  filter(hh_b05=='Head' & round == '1') %>%
  select(hh_refno, head_gender=hh_b02)

obj <- get_object('Capital_HH.csv', bucket = 'vs-cdb-indicators')
hh <- read.csv(text = rawToChar(obj)) %>%
  filter(round=='1')
              
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

ph <- read.csv('precip_hunger.csv')

df$month <- as.numeric(substr(df$hh_interview_date, 6,7))

load('ls_spi.Rdata')

ls_spi$year <- substr(ls_spi$date, 1, 4) %>% as.numeric
ls_spi$month <- substr(ls_spi$date, 6, 7) %>% as.numeric

df$year <- substr(df$hh_interview_date, 1, 4) %>% as.numeric

all <- Reduce(f = function(x, y){merge(x, y, all.x=T, all.y=F)}, 
              x=list(ge, hh, ee, fs, he, nr, ag, 
                     df, ph, ls_spi))


rescale <- function(x){
  x/max(x, na.rm=T)
}

resc_vars <- c('ag', 'nonag', 'forest', 'fr_prod', 'ag_prod', 'market', 'pop', 'diversity', 'Food_As_Percent_Total_Spending', 
               'total_nonag_income', 'cost_expenditures', 'total_ag_production_value', 'forestpa', 'nonagpa', 'age', 'years', 'size')

for (r in resc_vars){
  all[ , r] <- rescale(all[ , r])
}

all$ls_cty <- paste0(all$landscape_no, all$country)
all$ls_cty <- factor(all$ls_cty)
all$country <- factor(all$country)


#Check for co-linearity
corls <- c('forest','nonag','forestpa','nonagpa','age','years',
           'size','market','pop','diversity',
           'cost_expenditures','total_ag_production_value','fr_prod','ag_prod',
           'spi12','precip','hunger')

cormat <- cor(na.omit(all[ , corls]))

cormat > 0.6



#Look at hunger, make sure you are getting it from the right time of year
foodmod <- glmer(food ~ forest + nonag + #forestpa + nonagpa + 
                   head_gender + age + years + size + market + pop + diversity + cost_expenditures + total_ag_production_value + #ag_prod + 
                   spi12 + precip + hunger + 
                   (1 | ls_cty) + (1 | country), data=all, family='binomial',
                 control=glmerControl(optCtrl=list(maxfun=2e4), optimizer='bobyqa'))
summary(foodmod)

nonfoodmod <- glmer(nonfood ~ forest + nonag + #forestpa + nonagpa + 
                      head_gender + age + years + size + market + pop + diversity + cost_expenditures + total_ag_production_value + ag_prod + 
                      spi12 + precip + hunger + 
                      (1 | ls_cty) + (1 | country), data=all, family='binomial',
                    control=glmerControl(optCtrl=list(maxfun=2e4), optimizer='bobyqa'))
summary(nonfoodmod)


all$any <- all$food | all$nonfood
anymod <- glmer(any ~ forest + nonag + #forestpa + nonagpa + 
                  head_gender + age + years + size + market + pop + diversity + cost_expenditures + total_ag_production_value + ag_prod + 
                  spi12 + precip + hunger + 
                  (1 | ls_cty) + (1 | country), data=all, family='binomial',
                control=glmerControl(optCtrl=list(maxfun=2e4), optimizer='bobyqa'))
summary(anymod)


library(ggplot2)
ggplot(all) + geom_histogram(aes(x=ag_prod, fill=country))
  