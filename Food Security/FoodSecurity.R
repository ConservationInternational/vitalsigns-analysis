library(dplyr)
library(ggplot2)
library(reshape2)

setwd('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/TreeBiodiversity')

source('../production_connection.R')
#source('../local_connection.R')

con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

df <- tbl(con, 'flagging__household_secI') %>% data.frame

df1 <- df %>% select(Country, Landscape.., jan=hh_i09_2012_1,
            feb=hh_i09_2012_2, 
            mar=hh_i09_2012_3,
            apr=hh_i09_2012_4,
            may=hh_i09_2012_5,
            jun=hh_i09_2012_6, 
            jul=hh_i09_2012_7,
            aug=hh_i09_2012_8, 
            sep=hh_i09_2012_9, 
            oct=hh_i09_2012_10,
            nov=hh_i09_2012_11, 
            dec=hh_i09_2012_12)

df2 <-  df %>% select(Country, Landscape.., jan=hh_i09_2013_1,
            feb=hh_i09_2013_2,
            mar=hh_i09_2013_3,
            apr=hh_i09_2013_4,
            may=hh_i09_2013_5,
            jun=hh_i09_2013_6,
            jul=hh_i09_2013_7,
            aug=hh_i09_2013_8,
            sep=hh_i09_2013_9,
            oct=hh_i09_2013_10,
            nov=hh_i09_2013_11,
            dec=hh_i09_2013_12)

df <- bind_rows(df1, df2)

mean2 <- function(v){
  sum(v, na.rm=T)/length(v)
}

dfsum <- df %>% group_by(Country, Landscape..) %>%
  summarize(jan=mean2(jan), feb=mean2(feb),
            mar=mean2(mar), apr=mean2(apr),
            may=mean2(may), jun=mean2(jun),
            jul=mean2(jul), aug=mean2(aug),
            sep=mean2(sep), oct=mean2(oct),
            nov=mean2(nov), dec=mean2(dec)) %>%
  melt(id.vars=c('Country', 'Landscape..'))
  





