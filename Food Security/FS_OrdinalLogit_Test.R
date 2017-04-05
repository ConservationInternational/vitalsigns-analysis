library(dplyr)
library(reshape2)

setwd('D://Documents and Settings/mcooper/GitHub/vs-indicators-calc/FoodSecurity/')

pg_conf <- read.csv('../rds_settings', stringsAsFactors=FALSE)

vs_db <- src_postgres(dbname='vitalsigns', host=pg_conf$host,
                      user=pg_conf$user, password=pg_conf$pass,
                      port=pg_conf$port)



# hh_i031
# How many meals, including breakfast are taken per day in your household? (Adults: over 5 Years Old)
# 
# hh_i08
# In the last 12 months, have you been faced with a situation when you did not have enough food to feed the household?

fs <- tbl(vs_db, 'flagging__household_secI') %>% data.frame

fs <- fs[ , c('hh_i08', paste0('hh_i02_', seq(1,8)))]

# 1  Rely on less preferred foods? 
# 2  Limit the variety of foods eaten? 
# 3  Limit portion size at meal-times? 
# 4  Reduce number of meals eaten in a day? 
# 5  Restrict consumption by adults for small children to eat? 
# 6  Borrow food, or rely on help from a friend or relative? 
# 7  Have no food of any kind in your house-hold? 
# 8  Go a whole day and night without eating anything?

poa <- function(x){
  df <- data.frame(i=0, l=0)
  for (i in 1:(length(x)-1)){
    if (i == 1){
      l <- log(x[i]/sum(x[2:length(x)]))
      df <- rbind(df, data.frame(i=i, l=l))
    } else{
      l <- log(sum(x[1:i])/sum(x[(i + 1):length(x)]))
      df <- rbind(df, data.frame(i=i, l=l))
    }
  }
  return(df[2:nrow(df), ])
}

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

df <- poa(table(fs$ord))
plot(df$i, df$l)
table(fs$ord)




##This one meets the proportional odds assumption
# 
# fs$ord[fs$hh_i08=='2'] <- 1               #364
# fs$ord[fs$hh_i08=='1'] <- 2               #174
# 
# fs$ord[which(fs$hh_i02_1 > 0)] <- 3       #13
# fs$ord[which(fs$hh_i02_2 > 0)] <- 3       #17
# fs$ord[which(fs$hh_i02_3 > 0)] <- 3       #14
# fs$ord[which(fs$hh_i02_4 > 0)] <- 3       #127
# fs$ord[which(fs$hh_i02_5 > 0)] <- 3       #4
# fs$ord[which(fs$hh_i02_6 > 0)] <- 4       #20
# fs$ord[which(fs$hh_i02_7 > 0)] <- 4       #49
# fs$ord[which(fs$hh_i02_8 > 0)] <- 4      #22
# 
# df <- pos(table(fs$ord))
# plot(df$i, df$l)
# table(fs$ord)









