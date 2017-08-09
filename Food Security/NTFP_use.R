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

ee <- read.csv('ee_export.csv')

eeag <- paste0('cci_', c('10', '11', '12', '20', '30', '190', '200'))
eenonag <- paste0('cci_', c('40', '50', '60', '61', '62', '80', '90', 
                            '100', '110', '120', '122', '130', '160', '170', 
                            '180', '190'))
eeagpa <- paste0('cciPA_', c('10', '11', '12', '20', '30', '190', '200'), '.0')
eenonagpa <- paste0('cciPA_', c('40', '50', '60', '61', '62', '80', '90', 
                            '100', '110', '120', '122', '130', '160', '170', 
                            '180'), '.0')

ee$ag <- rowSums(ee[ , names(ee) %in% eeag], na.rm=T)/rowSums(ee[ , names(ee) %in% c(eenonag, eeag)], na.rm=T)
ee$nonag <- rowSums(ee[ , names(ee) %in% eenonag], na.rm=T)/rowSums(ee[ , names(ee) %in% c(eenonag, eeag)], na.rm=T)

ee$agpa <- rowSums(ee[ , names(ee) %in% eeagpa], na.rm=T)/rowSums(ee[ , names(ee) %in% c(eenonagpa, eeagpa)], na.rm=T)
ee$nonagpa <- rowSums(ee[ , names(ee) %in% eenonagpa], na.rm=T)/rowSums(ee[ , names(ee) %in% c(eenonag, eeag)], na.rm=T)

ee$area_protected <- (ee$ag - ee$agpa) + (ee$nonag - ee$nonagpa)

ee$forest <- rowSums(ee[ , c('for_.1', 'for_1')], na.rm=T)/
  rowSums(ee[ , c("for_.1", "for_0", "for_1", "for_2")], na.rm=T)
ee$forestpa <- rowSums(ee[ , c('forPA_.1.0', 'forPA_1.0')], na.rm=T)/
  rowSums(ee[ , c("for_.1", "for_0", "for_1", "for_2")], na.rm=T)

ee <- ee %>% select(ag, nonag, forest, fr_prod, ag_prod,
                    agpa, nonagpa, forestpa, area_protected,
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


sum <- all %>%
  na.omit %>%
  group_by(country) %>%
  summarize(min(food), max(food), mean(food),
            min(area_protected), max(area_protected), mean(area_protected),
            min(nonag), max(nonag), mean(nonag),
            min(forest), max(forest), mean(forest),
            mean(head_gender=="Male"),
            min(age), max(age), mean(age),
            min(years), max(years), mean(years),
            min(size), max(size), mean(size),
            min(literate), max(literate), mean(literate),
            min(market), max(market), mean(market),
            min(pop), max(pop), mean(pop),
            min(spi12), max(spi12), mean(spi12),
            min(diversity), max(diversity), mean(diversity),
            min(hunger), max(hunger), mean(hunger),
            min(total_ag_production_value), max(total_ag_production_value), mean(total_ag_production_value),
            min(total_nonag_income), max(total_nonag_income), mean(total_nonag_income),
            min(cost_expenditures), max(cost_expenditures), mean(cost_expenditures))

write.csv(t(sum), 'summary_stats.csv',
          col.names = F)

rescale <- function(x){
  (x - mean(x, na.rm=T))/max(x, na.rm=T)
}

resc_vars <- c('ag', 'area_protected', 'nonag', 'forest', 'fr_prod', 'ag_prod', 'market', 'pop', 
               'diversity', 'Food_As_Percent_Total_Spending', 
               'total_nonag_income', 'cost_expenditures', 'total_ag_production_value', 
               'forestpa', 'nonagpa', 'age', 'years', 'size',
               "agpa", "months_insecurity", "number_meals", "hfias", 
               "Nonfood.Spending", "Food.Consumption.Value", 
               "Food.Spending",
               "precip", "hunger", "spi6", "spi12", "spi24", "spi36")

for (r in resc_vars){
  all[ , paste0(r, '_orig')] <- all[ , r]
  all[ , r] <- rescale(all[ , r])
}

all$ls_cty <- paste0(all$landscape_no, all$country)
all$ls_cty <- factor(all$ls_cty)
all$country <- factor(all$country)


#Check for co-linearity
corls <- c('ag', 'nonag', 'forest', 'fr_prod', 'ag_prod', 'market', 'pop', 
           'diversity', 'Food_As_Percent_Total_Spending', 
           'total_nonag_income', 'cost_expenditures', 'total_ag_production_value', 
           'forestpa', 'nonagpa', 'age', 'years', 'size',
           "agpa", "months_insecurity", "number_meals", "hfias", 
           "Nonfood.Spending", "Food.Consumption.Value", 
           "Food.Spending",
           "precip", "hunger", "spi6", "spi12", "spi24", "spi36")

cormat <- cor(na.omit(all[ , corls]))

cormat > 0.6
#All SPIs are multicollinear - can only include one
#precip is multicollinear with forest vars?
#all pa vars corrlate with non pa counterparts
#Nonfood spending and expenditures covary
#Food spending and food consumption value covaries


foodmod <- glmer(food ~ 
                   #Land cover vars
                   area_protected + forest + nonag + #forestpa + nonagpa + 
                   #HH demographic vars
                   head_gender + age + years + size + literate + 
                   #Geographic control vars
                   market + pop + spi12 +
                   #Food secrity vars
                   diversity + hunger + shortage_year + hfias + 
                   #Income vars
                   total_ag_production_value + total_nonag_income + cost_expenditures +
                   #productivity vars
                   #ag_prod + #fr_prod
                   #Grouping vars
                   (1 | ls_cty) + (1 | country),
                   data=all, family='binomial',
                   control=glmerControl(optCtrl=list(maxfun=2e4), optimizer='bobyqa'))
summary(foodmod)



nonfoodmod <- glmer(nonfood ~
                      #Land cover vars
                      area_protected + forest + nonag + #forestpa + nonagpa +
                      #HH demographic vars
                      head_gender + age + years + size + literate +
                      #Geographic control vars
                      market + pop + spi12 +
                      #Food secrity vars
                      diversity + hunger + shortage_year + hfias +
                      #Income vars
                      total_ag_production_value + total_nonag_income + cost_expenditures +
                      #productivity vars
                      ag_prod + fr_prod + 
                      #Grouping vars
                      (1 | ls_cty) + (1 | country), data=all, family='binomial',
                    control=glmerControl(optCtrl=list(maxfun=2e4), optimizer='bobyqa'))
summary(nonfoodmod)


all$any <- all$food | all$nonfood
anymod <- glmer(any ~
                  #Land cover vars
                  area_protected + forest + nonag + #forestpa + nonagpa +
                  #HH demographic vars
                  head_gender + age + years + size + literate +
                  #Geographic control vars
                  market + pop + spi12 +
                  #Food secrity vars
                  diversity + hunger + shortage_year + hfias + 
                  #Income vars
                  total_ag_production_value + total_nonag_income + cost_expenditures +
                  #productivity vars
                  ag_prod + fr_prod + 
                  #Grouping vars
                  (1 | ls_cty) + (1 | country), data=all, family='binomial',
                control=glmerControl(optCtrl=list(maxfun=2e4), optimizer='bobyqa'))
summary(anymod)

#Normalize by density
#Get sum instead of mean
library(ggplot2)
ggplot(all) + geom_histogram(aes(x=ag_prod_orig, fill=ls_cty))
  

ggplot(all) + geom_histogram(aes(x=hunger, fill=ls_cty))


ggplot(all) + geom_histogram(aes(x=nonag_orig, fill=ls_cty))
