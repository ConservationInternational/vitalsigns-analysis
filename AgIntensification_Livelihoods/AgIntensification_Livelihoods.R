library(dplyr)

setwd('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/AgIntensification_Livelihoods/')

#source('../production_connection.R')
source('../local_connection.R')

con <- src_postgres(dbname = dbname, host = host, port = port, user = user, password = password)


flagFilter <- function(df){
  #df must be a data frame (not a tbl), with a 'flag' column
  
  patterns <- names(df) %>% paste0(collapse='|')
  df <- df[!grepl(pattern = patterns, df$flag),]
  
  df <- df[ , names(df) != 'flag']
  
  df
}

#####################################################
#Independant Vars indicative of Ag Intensification
#####################################################

iv_df <- tbl(con, "flagging__agric") %>%
  select(survey_uuid, Country, `Landscape #`, `Household ID`, flag) %>%
  data.frame %>%
  filter(!grepl('record is duplicated in the schema', flag)) %>%
  select(survey_uuid, Country, Landscape.., Household.ID)


# Field Size - agric_field_roster
#   ag2a_09 - GPS MEASUREMENT ((what is the area of the field taken by GPS tracking (Acres)?)

field_size <- tbl(con, "flagging__agric_field_roster") %>%
  select(survey_uuid, ag2a_04, flag) %>%
  data.frame %>% #flagFilter %>%
  group_by(survey_uuid) %>%
  summarize(average_field_size = mean(ag2a_04, na.rm=T), total_acreage = sum(ag2a_04, na.rm=T))

iv_df <- merge(iv_df, field_size, all.x=T)


# Intercropping  - agric_crops_by_field
#   ag4a_04 - Was cultivation intercropped? {1: 'Yes', 2: 'No'}

intercropping <- tbl(con, "flagging__agric_crops_by_field") %>%
  select(survey_uuid, ag4a_04, flag) %>%
  data.frame %>% flagFilter

intercropping$ag4a_04 <- as.numeric(intercropping$ag4a_04)
intercropping$ag4a_04[intercropping$ag4a_04==2] <- 0

intercropping <- intercropping %>% group_by(survey_uuid) %>%
  summarize(intercrop_rate = mean(ag4a_04, na.rm=T))

iv_df <- merge(iv_df, intercropping, all.x=T)

# Inputs - agric_field_details
#  ag3a_34 - What was the main type of pesticide/herbicide that you applied? {1: 'Pesticide', 2: 'Herbicide', 3: 'Fungicide'}

#  fd35_24a_* - Select all the types of inorganic fertilizer that  you used on this field
#   fd35_24a_dap
#   fd35_24a_urea
#   fd35_24a_tsp
#   fd35_24a_can
#   fd35_24a_sa
#   fd35_24a_npk
#   fd35_24a_mrp

inputs <- tbl(con, "flagging__agric_field_details") %>%
  select(survey_uuid, ag3a_34, fd35_24a_dap, fd35_24a_urea, fd35_24a_tsp,
         fd35_24a_can, fd35_24a_sa, fd35_24a_npk, fd35_24a_mrp, flag) %>%
  data.frame %>% flagFilter

inputs$pesticide <- inputs$ag3a_34==1
inputs$herbicide <- inputs$ag3a_34==2
inputs$fungicide <- inputs$ag3a_34==3

inputs[inputs=='t'] <- 1
inputs[inputs=='f'] <- 0

inputs$fd35_24a_urea <- as.numeric(inputs$fd35_24a_urea)
inputs$fd35_24a_dap <- as.numeric(inputs$fd35_24a_dap)
inputs$fd35_24a_tsp <- as.numeric(inputs$fd35_24a_tsp)
inputs$fd35_24a_can <- as.numeric(inputs$fd35_24a_can)
inputs$fd35_24a_sa <- as.numeric(inputs$fd35_24a_sa)
inputs$fd35_24a_npk <- as.numeric(inputs$fd35_24a_npk)
inputs$fd35_24a_mrp <- as.numeric(inputs$fd35_24a_mrp)

inputs <- inputs %>% group_by(survey_uuid) %>%
  summarize(pesticide = mean(pesticide, na.rm=T), herbicide = mean(herbicide, na.rm=T),
            fungicide = mean(fungicide, na.rm=T), dap = mean(fd35_24a_dap, na.rm=T),
            urea = mean(fd35_24a_urea, na.rm=T), tsp = mean(fd35_24a_tsp, na.rm=T),
            can = mean(fd35_24a_can, na.rm=T), sa = mean(fd35_24a_sa, na.rm=T),
            npk = mean(fd35_24a_npk, na.rm=T), mrp = mean(fd35_24a_mrp, na.rm=T))

iv_df <- merge(iv_df, inputs, all.x=T)

# Yields - get the Z score for crop-unit
#   ag4a_08 - Area (Acres) Farmers estimate
#   ag4a_15 - Amount
#   ag4a_15_unit - Unit  {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}

yields <- tbl(con, "flagging__agric_crops_by_field") %>%
  filter(!is.na(ag4a_15_unit) & !is.na(ag4a_08) & !is.na(ag4a_15) & ag4a_08 > 0) %>%
  select(survey_uuid, `Crop name`, ag4a_08, ag4a_15, ag4a_15_unit, flag) %>%
  data.frame %>% flagFilter

yields$yield <- yields$ag4a_15/yields$ag4a_08

for (c in unique(yields$Crop.name)){
  for (u in unique(yields$ag4a_15_unit)){
    sel <- yields$yield[yields$Crop.name==c & yields$ag4a_15_unit==u]
    zscore <- (sel - mean(sel))/sd(sel)
    yields$yield_zscore[yields$Crop.name==c & yields$ag4a_15_unit==u] <- zscore
  }
}

yields <- yields %>% group_by(survey_uuid) %>% summarize(yield_zscore = mean(yield_zscore, na.rm=T))

iv_df <- merge(iv_df, yields, all.x=T)

#Hired labor per Area
labor_hired <- tbl(con, "flagging__agric_field_details") %>%
  select(survey_uuid, flag,
         ag3a_38_1, ag3a_38_2, ag3a_38_21, ag3a_38_4, ag3a_38_5, 
         ag3a_38_51, ag3a_38_61, ag3a_38_62, ag3a_38_63, ag3a_38_7,
         ag3a_38_8, ag3a_38_81) %>%
  data.frame %>% flagFilter

labor_hired$labor_hired <- labor_hired$ag3a_38_1 + labor_hired$ag3a_38_4 + labor_hired$ag3a_38_61 + labor_hired$ag3a_38_7 + 
  labor_hired$ag3a_38_2 + labor_hired$ag3a_38_5 + labor_hired$ag3a_38_62 + labor_hired$ag3a_38_8 + 
  labor_hired$ag3a_38_21 + labor_hired$ag3a_38_51 + labor_hired$ag3a_38_63 + labor_hired$ag3a_38_81

labor_hired <- labor_hired %>% group_by(survey_uuid) %>%
  summarize(labor_hired = sum(labor_hired, na.rm=T))

iv_df <- merge(iv_df, labor_hired, all.x=T)

#Household labor per Area
labor_hh <- tbl(con, "flagging__agric_field_details_labor") %>%
  select(survey_uuid, flag, ag3a_70_preparing, ag3a_70_weeding,
         ag3a_70_fertilizing, ag3a_70_harvesting) %>%
  data.frame %>% flagFilter %>%
  group_by(survey_uuid) %>% 
  summarize(labor_hh=sum(ag3a_70_preparing + ag3a_70_weeding +
                         ag3a_70_fertilizing + ag3a_70_harvesting, na.rm=T))

iv_df <- merge(iv_df, labor_hh, all.x=T)

#Amount Sold
#   ag5a_01 - Did you sell any of the ${fd5_crop_name} produced
#   ag5a_02_1 - Amount
#   ag5a_02_2 - Unit {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}
sold <- tbl(con, "flagging__agric_crops_by_hh") %>%
  select(survey_uuid, `Crop name`, ag5a_01, ag5a_02_1, ag5a_02_2, flag) %>%
  data.frame %>% flagFilter

#   ag4a_08 - Area (Acres) Farmers estimate
#   ag4a_15 - Amount
#   ag4a_15_unit - Unit  {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}

yields <- tbl(con, "flagging__agric_crops_by_field") %>%
  filter(!is.na(ag4a_15_unit) & !is.na(ag4a_08) & !is.na(ag4a_15) & ag4a_08 > 0) %>%
  select(survey_uuid, `Crop name`, ag4a_08, ag4a_15, ag4a_15_unit, flag) %>%
  data.frame %>% flagFilter %>%
  group_by(survey_uuid, Crop.name, ag4a_15_unit) %>%
  summarize(ag4a_15=sum(ag4a_15, na.rm=T))

names(yields)[names(yields)=='ag4a_15_unit'] <- 'ag5a_02_2'

sold <- merge(sold, yields, all=T)

sold$ag5a_01 <- as.numeric(sold$ag5a_01)
sold$ag5a_01[sold$ag5a_01==2] <- 0

sold$percent_sold <- sold$ag5a_02_1/sold$ag4a_15
sold$percent_sold[sold$percent_sold > 1] <- NA

sold <- sold %>% group_by(survey_uuid) %>%
  summarize(avg_pct_harvest_sold = mean(percent_sold, na.rm=T),
            avg_pct_crops_any_sold = mean(ag5a_01, na.rm=T)) %>%
  data.frame

iv_df <- merge(iv_df, sold, all.x=T)

# Irrigaion


# Inorganic Fertilizers
#  fd33_18a_* - Select all types of organic fertilizer that you used
#    fd33_18a_1: 'Crop Residue', 
#    fd33_18a_2 'Animal Manure', 
#    fd33_18a_4 'Natural Fallow', 
#    fd33_18a_5 'Leguminous Tree Fallow', 
#    fd33_18a_6 'Leguminous Cover Crop', 
#    fd33_18a_7 'Biomass Transfer', 
#    fd33_18a_8 'Compost'

##############################################
#Get all dependant variables - Livelihoods
##############################################

# Income
#   Farming
#   Labor
# Nutrition
# Ratio of Laborers to Dependants
# Education
# Employment
# Nutrition

######################################
#Combine and Analyze
######################################

########################################
#Issues - 
########################################
#Figure out Area, too many ways to measure, they seem incongruous.
#Harmonize Education Vars
#Need to add schema flagging for hh-level records

#some households with no fields at all?

#How are there NAs in select multiple variables?

#Why are there NAs in the Units harvested? agric_crops_by_field.ag4a_15_unit

#lots of missing data in the labor area, lots of 0's and NAs.

#BIG data quality issue: 0s vs NAs

#We've removed flagged outliers.  But if they are real values and not outliers,
#  then we are missing vital info.
