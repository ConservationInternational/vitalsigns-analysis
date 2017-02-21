library(dplyr)
library(reshape2)
library(ggplot2)

setwd('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Soils/')

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

#Nitrogen levels in eplot by type

eplot <- tbl(con, 'flagging__eplotsoils_processed') %>% 
  data.frame %>%
  select(Eplot.Code, 
         N=Nitrogen.content.for.acid.treated.sample.to.remove.carbonates..,
         C=Carbon.content.for.acid.treated.sample.to.remove.carbonates...., 
         pH=Soil.pH.in.water..soil.to.water.ratio.of.1.to.2.weight.to.volum, 
         K=Potassium.total.element.concentration.TXRF..mg.kg..1.,
         P=Phosphorus.total.element.concentration.TXRF..mg.kg..1.)

eplot_id <- tbl(con, 'eplot_id') %>% data.frame %>%
  select(country, landscape_no, eplot_no, landcover)

eplot_id$Eplot.Code <- with(eplot_id, paste(country, landscape_no, eplot_no, sep='-'))

eplot_soil <- merge(eplot, eplot_id, all.x=T, all.y=F) %>% na.omit

ggplot(eplot_soil) + geom_boxplot(aes(landcover, N)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('D://SAGCOT/NITROGEN.png')

ggplot(eplot_soil) + geom_boxplot(aes(landcover, P)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('D://SAGCOT/PHOSPHOROUS.png')

ggplot(eplot_soil) + geom_boxplot(aes(landcover, K)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('D://SAGCOT/POTASSIUM.png')

ggplot(eplot_soil) + geom_boxplot(aes(landcover, C)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('D://SAGCOT/CARBON.png')

ggplot(eplot_soil) + geom_boxplot(aes(landcover, pH)) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave('D://SAGCOT/PH.png')


#Nitrogen levels in fields with organic fert, inorganic fert, and none
#    ag3a_39	Did you use any ORGANIC FERTILIZER on this FIELD in the last completed Long Rainy Season/Major Cropping Season?
#    ag3a_45	Did you use any inorganic fertilizer on this FIELD in the last completed Long Rainy Season / Major Cropping Season?

agric <- tbl(con, 'flagging__agric_field_details')


ffs <- tbl(con, 'flagging__farmfieldsoils_processed')



#Somehow prove that nature make soils healthy?



