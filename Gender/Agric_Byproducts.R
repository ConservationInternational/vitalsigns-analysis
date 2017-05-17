setwd("D:/Documents and Settings/tnjunge/Documents/VS Data Analysis")
setwd("D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Gender/")

library(dplyr)
library(ggplot2)

source("../production_connection.R")
con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

##Whether household have female heads
secb <- tbl(con, 'flagging__household_secB') %>% ##coerce to a table
  data.frame


secb_head <- secb[secb$hh_b05=='1' & secb$Round == '1',c('Country', 'Landscape..', 'Household.ID', "Round", 'hh_b02')]


secb_head <- secb %>%
  filter(hh_b05=='1' & Round == '1') %>%
  select(Country, Landscape.., Household.ID, Round, hh_b02)

##secb_head <- secb_head[!is.na(secb_head$hh_b05), ]

#Agric byproducts
agric_byprod <- tbl(con, 'flagging__agric_byprod') %>% 
  data.frame

#Combine them
head_byprod <- merge(secb_head, agric_byprod, all.x=TRUE, all.y=FALSE)

currency <- data.frame(Country=c('TZA', 'RWA', 'GHA', 'UGA'),
                       Rate = c(2236, 825.7, 4.225, 3643))

head_byprod <- merge(head_byprod, currency)

head_byprod$sales_usd <- head_byprod$ag09_08/head_byprod$Rate
#


#what are the byproducts?
res <- head_byprod %>% 
  group_by(hh_b02, Crop.ID) %>% 
  summarize(total=n())

ggplot(res %>% filter(hh_b02=='1' & !is.na(Crop.ID)), aes(x="", y=total, fill=Crop.ID)) +
  geom_bar(stat='identity') + coord_polar("y", start=0)

ggplot(res %>% filter(hh_b02=='2' & !is.na(Crop.ID)), aes(x="", y=total, fill=Crop.ID)) +
  geom_bar(stat='identity') + coord_polar("y", start=0)


#how much they earned?
sales_total <- head_byprod %>% group_by(Country, hh_b02, Household.ID, Round ) %>%
  summarize(sold=sum(ag09_08)) %>% filter(!is.na(sold))
fittedline <- lm(sold~hh_b02, data=sales_total)
summary(fittedline)# THE r VALUE IS 0.085 thus explain 85% of the variance so a good predictive model
a0v <- anova(fittedline)## CLARIFY HOW TO INTERPRET?? PIS 0.07 so no relationship?

summary(aov(sold ~ hh_b02 + Country, data=sales_total))


#what is the quantity of Maize produced in the last 12 months?
TotalQuantity <- head_byprod %>% 
  filter(Crop.ID=='Maize') %>%
  group_by(Country,hh_b02,Household.ID,Round)%>%
  summarise(quantity=sum(ag09_04_1)) %>% filter(!is.na(quantity))

fittedline2 <- aov(quantity~hh_b02+Country, data=TotalQuantity)
summary(fittedline2)

#what is the quantity of Cassava produced in the last 12 months?
TotalQuantity <- head_byprod %>% 
  filter(Crop.ID=='Cassava') %>%
  group_by(Country,hh_b02,Household.ID,Round)%>%
  summarise(quantity=sum(ag09_04_1)) %>% filter(!is.na(quantity))

fittedline3 <- aov(quantity~hh_b02+Country, data=TotalQuantity)
summary(fittedline3)


#what is the quantity sold in the last 12 months among *all* households?
TotalQuantitySoldAll <- head_byprod %>% 
  group_by(Country,hh_b02,Household.ID,Round)%>%
  summarise(quantitySold=sum(ag09_06_1, na.rm=T))

lm(quantitySold ~ hh_b02, data=TotalQuantitySoldAll) %>% summary
#Women sell more  

#what is the quantity sold in the last 12 months among households that sold any?
TotalQuantitySoldOnlySellers <- head_byprod %>% 
  group_by(Country,hh_b02,Household.ID,Round)%>%
  summarise(quantitySold=sum(ag09_06_1, na.rm=t)) %>% 
  filter(quantitySold > 0)

lm(quantitySold ~ hh_b02, data=TotalQuantitySoldOnlySellers) %>% summary
#Women sell less


#Of households that produce byprod, households that sell byprod
ProduceAndSell <- head_byprod %>% 
  filter(ag09_04_1 > 0 & !is.na(ag09_04_1)) %>%
  group_by(Country,hh_b02,Household.ID,Round) %>%
  summarise(quantitySold=sum(ag09_06_1, na.rm=T)) %>% 
  mutate(sold=quantitySold > 0)

lm(sold ~ hh_b02, data=ProduceAndSell) %>% summary
#No major difference, most households dont sell



#what is the quantity sold in the last 12 months?
TotalQuantitySold <- head_byprod %>% 
  group_by(Country,hh_b02,Household.ID,Round)%>%
  summarise(quantitySold=sum(ag09_06_1)) %>% 
  filter(!is.na(quantitySold))##significance level is 0.09 so no relationship?





##Total additional expenses
Totaladdexpenses <- head_byprod %>% group_by(Country,hh_b02,Household.ID,Round)%>%
  summarise(Totalexpenses=sum(ag09_11)) %>% filter(!is.na(Totalexpenses))

profit <- merge(sales_total, Totaladdexpenses, all.x=TRUE, all.y=FALSE)
profit$Profit <- profit$sold-profit$Totalexpenses
head(profit)
fittedline3 <- lm(Profit~hh_b02, data=profit)
summary(fittedline3)#no relationship?
#crossvalidation
#only the round ones
#currency  and unit differences
#why cant we extrapolate when we have sampled correcrly?

