setwd('D://Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security/')

#Framing stats for increasing/decreasing and overal vallue

library(aws.s3)
library(dplyr)
library(lubridate)
library(lme4)
library(lmerTest)
library(influence.ME)
detach("package:raster", unload=TRUE)


aws.signature::use_credentials()

source('../production_connection.R')
#source('../local_connection.R')

con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

###############################
#Demographic data and indicators
##############################
df <- tbl(con, 'c__household') %>%
  filter(round=='1') %>%
  select(country, landscape_no, hh_refno, hh_interview_date) %>%
  collect

ge <- tbl(con, 'c__household_individual') %>%
  filter(hh_b05=='Head' & round == '1') %>%
  select(hh_refno, head_gender=hh_b02) %>%
  collect

obj <- get_object('Capital_HH.csv', bucket = 'vs-cdb-indicators')
hh <- read.csv(text = rawToChar(obj)) %>%
  filter(round=='1')
              
obj <- get_object('FoodSecurity_HH.csv', bucket = 'vs-cdb-indicators')
fs <- read.csv(text = rawToChar(obj)) %>%
  filter(round=='1') %>%
  select(hh_refno, shortage_year, months_insecurity, number_meals, hfias,
         diversity, Nonfood.Spending, Food.Consumption.Value, Food.Spending,
         Food_As_Percent_Total_Spending)

obj <- get_object('HouseholdEconomy_HH.csv', bucket = 'vs-cdb-indicators')
he <- read.csv(text = rawToChar(obj)) %>%
  filter(round=='1') %>%
  select(hh_refno, income_wage, net_income_business)

obj <- get_object('NaturalResources_HH.csv', bucket = 'vs-cdb-indicators')
nr <- read.csv(text = rawToChar(obj)) %>%
  filter(round == '1') %>%
  select(hh_refno, food, nonfood)

obj <- get_object('Ag_HH.csv', bucket = 'vs-cdb-indicators')
av <- read.csv(text = rawToChar(obj)) %>%
  filter(round == '1') %>%
  select(hh_refno, total_ag_production_value, country, landscape_no,
         number_of_fields, total_area_farmed, value_fieldcrop_harvest, value_permcrops, value_produced_livestock,
         value_livestockbyprod_produced, net_ag_income, crop_commodification_index, value_produced_cropbyprods)

obj <- get_object('Nutrition_HH.csv', bucket = 'vs-cdb-indicators')
nt <- read.csv(text = rawToChar(obj)) %>%
  filter(round == '1') %>%
  select(hh_refno, mean_zlen, mean_zwei, mean_zwfl)

########################
#SPI and pre-calculated seasonal vars
########################
ph <- read.csv('precip_hunger.csv')

df$month <- as.numeric(substr(df$hh_interview_date, 6,7))

load('ls_spi.Rdata')

ls_spi$year <- substr(ls_spi$date, 1, 4) %>% as.numeric
ls_spi$month <- substr(ls_spi$date, 6, 7) %>% as.numeric

df$year <- substr(df$hh_interview_date, 1, 4) %>% as.numeric

#####################
#Earth Engine Vars
#####################
ee <- read.csv('ee_export.csv')

ee[is.na(ee)] <- 0

ag <- paste0('cci_', c('10', '11', '12', '20', '190', '200'))
savanna <- paste0('cci_', c('120', '122', '130', '180'))
cciforest <- paste0('cci_', c('60', '61', '62', '80', '90', '160', '170'))
mos_ag_fr <- 'cci_30'
mos_fr_ag <- 'cci_40'
mos_fr_sv <- 'cci_100'
mos_sv_fr <- 'cci_110'

getPercetCover <- function(selcols, allcolmatch, df){
  if(length(selcols) > 1){
    selcolsum <- rowSums(df[ , selcols[selcols %in% names(df)]], na.rm=T)
  } else{
    selcolsum <- df[ , selcols]
  }
  allcolsum <- rowSums(df[ , grepl(allcolmatch, names(df))], na.rm=T)
  return(selcolsum/allcolsum)
}

ee$ag <- getPercetCover(c(ag, mos_ag_fr), 'cci_', ee)
ee$savanna <- getPercetCover(c(savanna, mos_sv_fr), 'cci_', ee)
ee$cciforest <- getPercetCover(c(cciforest, mos_fr_ag, mos_fr_sv), 'cci_', ee)
ee$mos_ag_fr <- getPercetCover(mos_ag_fr, 'cci_', ee)
ee$mos_fr_ag <- getPercetCover(mos_fr_ag, 'cci_', ee)
ee$mos_fr_sv <- getPercetCover(mos_fr_sv, 'cci_', ee)
ee$mos_sv_fr <- getPercetCover(mos_sv_fr, 'cci_', ee)

ee$area_protected <- getPercetCover('PA_0.0', 'PA_', ee)

ee$forest <- getPercetCover(c('for_.1', 'for_1'), 'for_', ee)

ee <- ee %>% select(ag, savanna, cciforest, mos_ag_fr,
                    mos_fr_ag, mos_fr_sv, mos_sv_fr,
                    forest, area_protected,
                    market, pop, hh_refno) %>%
  filter(!duplicated(hh_refno))

##################
#Combine
##################
all <- Reduce(f = function(x, y){merge(x, y, all.x=T, all.y=F)}, 
              x=list(ge, hh, ee, fs, he, nr, av, 
                     df, ph, nt, ls_spi))# %>% na.omit


all$type[!all$food & all$nonfood] <- 'None'
all$type[all$food & all$nonfood] <- 'Both'
all$type[all$food & !all$nonfood] <- 'Food'
all$type[!all$food & all$nonfood] <- 'Nonfood'

#####################
#Summarize by Country
#####################
# sum <- all %>%
#   na.omit %>%
#   group_by(country) %>%
#   summarize(min(food), max(food), mean(food),
#             min(area_protected), max(area_protected), mean(area_protected),
#             min(ag), max(ag), mean(ag),
#             min(cciforest), max(cciforest), mean(cciforest),
#             mean(head_gender=="Male"),
#             min(age), max(age), mean(age),
#             min(years), max(years), mean(years),
#             min(size), max(size), mean(size),
#             min(literate), max(literate), mean(literate),
#             min(market), max(market), mean(market),
#             min(pop), max(pop), mean(pop),
#             min(spi12), max(spi12), mean(spi12),
#             min(diversity), max(diversity), mean(diversity),
#             min(hunger), max(hunger), mean(hunger),
#             min(total_ag_production_value), max(total_ag_production_value), mean(total_ag_production_value),
#             min(total_nonag_income), max(total_nonag_income), mean(total_nonag_income),
#             min(net_income_business), max(net_income_business), mean(net_income_business),
#             n())
# write.csv(t(sum), 'summary_stats.csv',
#           col.names = F)

######################
#Rescale Vars and check co-linearity
#####################################
rescale <- function(x){
  (x - mean(x, na.rm=T))/max(x, na.rm=T)
}

all$lpop <- log(all$pop)

resc_vars <- c("years", "age", "size", "ag", "savanna",
               "cciforest", "mos_ag_fr", "mos_fr_ag", "mos_fr_sv", "mos_sv_fr", 
               "forest", "area_protected", "market", "pop", "shortage_year", 
               "months_insecurity", "number_meals", "hfias", "diversity", "Nonfood.Spending", 
               "Food.Consumption.Value", "Food.Spending", "Food_As_Percent_Total_Spending", 
               "income_wage", "net_income_business", "total_ag_production_value", 
               "number_of_fields", "total_area_farmed", "value_fieldcrop_harvest", 
               "value_permcrops", "value_produced_livestock", "value_livestockbyprod_produced", 
               "net_ag_income", "crop_commodification_index", "value_produced_cropbyprods", 
               "precip", "hunger", "spi6", "spi12", "spi24", "spi36", "lpop")

for (r in resc_vars){
  all[ , paste0(r, '_orig')] <- all[ , r]
  all[ , r] <- rescale(all[ , r])
}

all$ls_cty <- paste0(all$landscape_no, all$country)
all$ls_cty <- factor(all$ls_cty)
all$country <- factor(all$country)

# 
# #Check for co-linearity
# corls <- c("years", "age", "size", "ag", "savanna",
# "cciforest", "mos_ag_fr", "mos_fr_ag", "mos_fr_sv", "mos_sv_fr", 
# "forest", "area_protected", "market", "pop", "shortage_year", 
# "months_insecurity", "number_meals", "hfias", "diversity", "Nonfood.Spending", 
# "Food.Consumption.Value", "Food.Spending", "Food_As_Percent_Total_Spending", 
# "income_wage", "net_income_business", "total_ag_production_value", 
# "number_of_fields", "total_area_farmed", "value_fieldcrop_harvest", 
# "value_permcrops", "value_produced_livestock", "value_livestockbyprod_produced", 
# "net_ag_income", "crop_commodification_index", "value_produced_cropbyprods", 
# "precip", "hunger", "spi6", "spi12", "spi24", "spi36", "total_nonag_income")
# 
# cormat <- cor(na.omit(all[ , corls]))
# 
# View(cormat > .7)
#All SPIs are multicollinear - can only include one
#precip is multicollinear with forest vars?
#all pa vars corrlate with non pa counterparts
#Nonfood spending and expenditures covary
#Food spending and food consumption value covaries


######################
#Landcover Models
#######################

foodmod <- glmer(food ~ 
                   #Land cover vars
                   area_protected + cciforest + savanna + 
                   #HH demographic vars
                   head_gender + age + years + size + literate + 
                   #Geographic control vars
                   market + pop + spi12 + 
                   #Food secrity vars
                   precip + shortage_year + hfias + 
                   #Income vars
                   total_ag_production_value + net_income_business +
                   income_wage + Nonfood.Spending + Food.Spending + 
                   #Grouping vars
                   (1 | ls_cty) + (1 | country), data=all, family='binomial',
                 control=glmerControl(optCtrl=list(maxfun=2e4), optimizer='bobyqa'))
summary(foodmod)

nonfoodmod <- glmer(nonfood ~
                      #Land cover vars
                      area_protected + savanna + forest +
                      #HH demographic vars
                      head_gender + age + years + size + literate + 
                      #Geographic control vars
                      market + pop + spi12 + 
                      #Food secrity vars
                      precip + shortage_year + hfias + 
                      #Income vars
                      total_ag_production_value + net_income_business +
                      income_wage + Nonfood.Spending + Food.Spending + 
                      #Grouping vars
                      (1 | ls_cty) + (1 | country), data=all, family='binomial',
                    control=glmerControl(optCtrl=list(maxfun=2e4), optimizer='bobyqa'))
summary(nonfoodmod)


all$any <- all$food | all$nonfood
anymod <- glmer(any ~
                  #Land cover vars
                  area_protected + savanna + cciforest +
                  #HH demographic vars
                  head_gender + age + years + size + literate + 
                  #Geographic control vars
                  market + pop + spi12 + 
                  #Food secrity vars
                  precip + shortage_year + hfias + 
                  #Income vars
                  total_ag_production_value + net_income_business +
                  income_wage + Nonfood.Spending + Food.Spending + 
                  #Grouping vars
                  (1 | ls_cty) + (1 | country), data=all, family='binomial',
                control=glmerControl(optCtrl=list(maxfun=2e4), optimizer='bobyqa'))
summary(anymod)

temp <- all %>% group_by(ls_cty) %>%
  summarize(any=mean(any),
            nonfood=mean(nonfood),
            food=mean(food),
            area_protected=mean(area_protected_orig),
            savanna=mean(savanna_orig),
            forest=mean(cciforest_orig))


newdf <- all[names(residuals(foodmod)), c('country', 'hh_refno', 'landscape_no')]

newdf$resid <- residuals(foodmod)

newdf %>% group_by(country, landscape_no) %>% summarize(mean(resid)) %>% data.frame


tmp <- all %>% 
  group_by(country, landscape_no) %>% 
  summarize(food=mean(food), nonfood=mean(nonfood), any=mean(any),
            pop=mean(pop_orig, na.rm=T),
            market=mean(market_orig, na.rm=T), spi12=mean(spi12_orig),
            spi36=mean(spi36_orig), area_protected=mean(area_protected_orig, na.rm=T),
            cciforest=mean(cciforest_orig, na.rm=T), savanna=mean(savanna_orig, na.rm=T),
            forest=mean(forest_orig, na.rm=T)) %>% 
  data.frame
