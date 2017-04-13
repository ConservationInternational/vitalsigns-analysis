setwd('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security')

library(dplyr)
library(ggplot2)

source('../production_connection.R')
con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

##Whether household have female heads
secb <- tbl(con, 'flagging__household_secB') %>% 
  data.frame

secb_head <- secb[secb$hh_b05=='1', c('Country', 'Landscape..', 'Household.ID', 'Round', 'hh_b02')]
secb_head <- secb_head[!is.na(secb_head$hh_b05), ]

#Agric byproducts
agric_byprod <- tbl(con, 'flagging__agric_byprod') %>% 
  data.frame

#Combine them
head_byprod <- merge(secb_head, agric_byprod, all.x=TRUE, all.y=FALSE)

#what are the byproducts?
res <- head_byprod %>% 
  group_by(hh_b02, Crop.ID) %>% 
  summarize(total=n())

ggplot(res %>% filter(hh_b02=='1' & !is.na(Crop.ID)), aes(x="", y=total, fill=Crop.ID)) +
  geom_bar(stat='identity') + coord_polar("y", start=0)

ggplot(res %>% filter(hh_b02=='2' & !is.na(Crop.ID)), aes(x="", y=total, fill=Crop.ID)) +
  geom_bar(stat='identity') + coord_polar("y", start=0)


#how much they earned?
sales_total <- head_byprod %>% group_by(Country, hh_b02, Household.ID, Round) %>%
  summarize(sold=sum(ag09_08)) %>% filter(!is.na(sold))

aov(sold~hh_b02, data=sales_total %>% filter(Country == 'TZA')) %>% summary

#what is the quantity produced?

#what is the quantity sold




