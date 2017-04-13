library(dplyr)
library(ggplot2)
library(reshape2)
detach("package:raster", unload=TRUE)

setwd('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/Food Security')

source('../production_connection.R')
#source('../local_connection.R')

con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)

#Which products?

#Where?

#Income from ag products?

#Percent of total income from ag products.