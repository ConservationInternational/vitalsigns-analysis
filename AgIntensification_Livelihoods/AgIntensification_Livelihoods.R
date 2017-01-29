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

allvars_df <- tbl(con, "flagging__agric") %>%
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

allvars_df <- merge(allvars_df, field_size, all.x=T)


# Intercropping  - agric_crops_by_field
#   ag4a_04 - Was cultivation intercropped? {1: 'Yes', 2: 'No'}

intercropping <- tbl(con, "flagging__agric_crops_by_field") %>%
  select(survey_uuid, ag4a_04, flag) %>%
  data.frame %>% flagFilter

intercropping$ag4a_04 <- as.numeric(intercropping$ag4a_04)
intercropping$ag4a_04[intercropping$ag4a_04==2] <- 0

intercropping <- intercropping %>% group_by(survey_uuid) %>%
  summarize(intercrop_rate = mean(ag4a_04, na.rm=T))

allvars_df <- merge(allvars_df, intercropping, all.x=T)

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

allvars_df <- merge(allvars_df, inputs, all.x=T)

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

allvars_df <- merge(allvars_df, yields, all.x=T)

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

allvars_df <- merge(allvars_df, labor_hired, all.x=T)

#Household labor per Area
labor_hh <- tbl(con, "flagging__agric_field_details_labor") %>%
  select(survey_uuid, flag, ag3a_70_preparing, ag3a_70_weeding,
         ag3a_70_fertilizing, ag3a_70_harvesting) %>%
  data.frame %>% flagFilter %>%
  group_by(survey_uuid) %>% 
  summarize(labor_hh=sum(ag3a_70_preparing + ag3a_70_weeding +
                         ag3a_70_fertilizing + ag3a_70_harvesting, na.rm=T))

allvars_df <- merge(allvars_df, labor_hh, all.x=T)

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

allvars_df <- merge(allvars_df, sold, all.x=T)

# Irrigaion
#   ag3a_09 - Was this FIELD irrigated in the last completed

irrig <- tbl(con, 'flagging__agric_field_details') %>% 
  select(survey_uuid, flag, ag3a_09) %>%
  data.frame %>% flagFilter

irrig$ag3a_09 <- as.numeric(irrig$ag3a_09)
irrig$ag3a_09[irrig$ag3a_09==2] <- 0

irrig <- irrig %>% group_by(survey_uuid) %>%
  summarize(pct_fields_irrigated = mean(ag3a_09, na.rm=T))

allvars_df <- merge(allvars_df, irrig, all.x=T)

# Inorganic Fertilizers
#  fd33_18a_* - Select all types of organic fertilizer that you used
#    fd33_18a_1: 'Crop Residue', 
#    fd33_18a_2 'Animal Manure', 
#    fd33_18a_4 'Natural Fallow', 
#    fd33_18a_5 'Leguminous Tree Fallow', 
#    fd33_18a_6 'Leguminous Cover Crop', 
#    fd33_18a_7 'Biomass Transfer', 
#    fd33_18a_8 'Compost'

###############################################
#Get control variables
###############################################

#HH size
size <- tbl(con, "flagging__household_secB") %>%
  select(survey_uuid, Country, `Landscape #`, `Household ID`, flag) %>%
  data.frame %>%
  filter(!grepl('record is duplicated in the schema', flag)) %>%
  group_by(Household.ID) %>%
  summarize(hh_size=n())

allvars_df <- merge(allvars_df, size, all.x=T)

##############################################
#Get all dependant variables - Livelihoods
##############################################

# Income
#   Business Income - hh_e65_1 - income
#   
#   From - household_secE

hhe <- tbl(con, "flagging__household_secE") %>%
  select(hh_e65_1, hh_e65_2, #hh_e28, hh_e27, hh_e26,
         hh_e22_1, hh_e22_2, hh_e24_1, hh_e24_2,
         flag, `Household ID`) %>%
  data.frame %>% flagFilter

hhe$income_own <- 0
hhe$income_own[which(hhe$hh_e65_2=='week')] <- hhe$hh_e65_1[which(hhe$hh_e65_2=='week')]*52.14286
hhe$income_own[which(hhe$hh_e65_2=='month')] <- hhe$hh_e65_1[which(hhe$hh_e65_2=='month')]*12

times <- c(1, 2, 3, 4, 5, 6, 7, 8)
annualized <- c(8*356, 365, 52.14286, 26.07143, 12, 4, 2, 1)

hhe$income_wage <- mapply(hhe$hh_e22_1*annualized[match(as.numeric(hhe$hh_e22_2), times)],
                          hhe$hh_e24_1*annualized[match(as.numeric(hhe$hh_e24_2), times)],
                          0, FUN=sum, na.rm=T)

hhe <- hhe %>% group_by(Household.ID) %>% summarize(income_own=sum(income_own, na.rm=T),
                                                    income_wage=sum(income_wage, na.rm=T))

allvars_df <- merge(allvars_df, hhe, all.x=T)

# Ag Income - (((ag10a_21+ag10a_27)-ag10a_34) + ag7a_04 + ag10b_06 + (ag09_08 - ag09_11) + (ag5a_03 + ag5a_26)) * [[exchange rate]]
#  agric_crops_by_hh; 
#    ag5a_03
#    ag5a_26

inc1 <- tbl(con, 'flagging__agric_crops_by_hh') %>%
  select(`Household ID`, flag,
         ag5a_03, ag5a_26) %>%
  data.frame %>% flagFilter

inc1$income_crops <- mapply(inc1$ag5a_03, inc1$ag5a_26, FUN=sum, na.rm=T)

inc1 <- inc1 %>% group_by(Household.ID) %>% summarize(income_crops=sum(income_crops, na.rm=T))

allvars_df <- merge(allvars_df, inc1, all.x=T)

#  agric_perm_crop; 
#    ag7a_04

inc2 <- tbl(con, 'flagging__agric_perm_crop') %>%
  select(`Household ID`, flag, ag7a_04) %>%
  data.frame %>% flagFilter %>%
  group_by(Household.ID) %>%
  summarize(income_perm_crop=sum(ag7a_04, na.rm=T))

allvars_df <- merge(allvars_df, inc2, all.x=T)

#  agric_livestock_byproduct; 
#    ag10b_06

inc3 <- tbl(con, 'flagging__agric_livestock_byproduct') %>%
  select(`Household ID`, flag, ag10b_06) %>%
  data.frame %>% flagFilter %>%
  group_by(Household.ID) %>%
  summarize(income_lvstk_byprod=sum(ag10b_06, na.rm=T))

allvars_df <- merge(allvars_df, inc3, all.x=T)

#  agric_byproduct; 
#    ag09_08
#    ag09_11

inc4 <- tbl(con, 'flagging__agric_byprod') %>%
  select(`Household ID`, flag, ag09_08, ag09_11) %>%
  data.frame %>% flagFilter

inc4$ag09_11[is.na(inc4$ag09_11)] <- 0

inc4$income_byprod <- inc4$ag09_08 - inc4$ag09_11

inc4 <- inc4 %>% group_by(Household.ID) %>% summarize(income_byprod=sum(income_byprod, na.rm=T))

allvars_df <- merge(allvars_df, inc4, all.x=T)

#  agric_livestock
#    ag10a_21
#    ag10a_27
#    ag10a_34

inc5 <- tbl(con, 'flagging__agric_livestock') %>%
  select(`Household ID`, flag, ag10a_21, ag10a_27, ag10a_34) %>%
  data.frame %>% flagFilter

inc5$income_lvstk <- mapply(inc5$ag10a_21, inc5$ag10a_27, FUN=sum, na.rm=T)

inc5$ag10a_34[is.na(inc5$ag10a_34)] <- 0

inc5$income_lvstk <- inc5$income_lvstk - inc5$ag10a_34

inc5 <- inc5 %>% group_by(Household.ID) %>% summarize(income_lvstk = sum(income_lvstk, na.rm=T))

allvars_df <- merge(allvars_df, inc5, all.x=T)

# Ratio of Laborers to Dependants
hh_labor <- tbl(con, 'flagging__household_secE') %>%
  select(`Household ID`, `Individual ID`, flag, hh_e04) %>%
  data.frame %>% flagFilter

hh_labor$hh_e04 <- as.numeric(hh_labor$hh_e04)
hh_labor$hh_e04[hh_labor$hh_e04==2] <- 0

ag_labor <- tbl(con, 'flagging__agric_field_details_labor') %>%
  select(`Household ID`, `Individual ID`, flag,
         ag3a_70_weeding, ag3a_70_fertilizing, ag3a_70_harvesting,
         ag3a_70_preparing) %>%
  data.frame %>% flagFilter

ag_labor[is.na(ag_labor)] <- 0

ag_labor$work[which(ag_labor$ag3a_70_weeding==0 & ag_labor$ag3a_70_harvesting==0 &
                ag_labor$ag3a_70_preparing==0 & ag_labor$ag3a_70_fertilizing==0)] <- 0

ag_labor$work[which(ag_labor$ag3a_70_weeding>0 | ag_labor$ag3a_70_harvesting>0 |
                        ag_labor$ag3a_70_preparing>0 | ag_labor$ag3a_70_fertilizing>0)] <- 1

labor <- merge(hh_labor, ag_labor, all.x=F, all.y=F) %>% unique
labor$ag_or_hh <- labor$work | labor$hh_e04

labor <- labor %>% group_by(Household.ID) %>% summarize(labor_pct=mean(labor$ag_or_hh, na.rm=T))

allvars_df <- merge(allvars_df, labor, all.x=T)

# Education
hh_ed <- tbl(con, 'flagging__household_secC') %>%
  #filter(hh_c01!='2') %>%
  select(`Household ID`, flag, hh_c02, hh_c03) %>%
  data.frame %>% flagFilter

hh_ed$hh_c02 <- as.numeric(hh_ed$hh_c02)
hh_ed$hh_c03 <- as.numeric(hh_ed$hh_c03)

hh_ed$hh_c02[hh_ed$hh_c02 < 5] <- 1
hh_ed$hh_c02[hh_ed$hh_c02 == 5] <- 0

hh_ed$hh_c03[hh_ed$hh_c03==2] <- 0

hh_ed <- hh_ed %>% group_by(Household.ID) %>% summarize(any_ed_perc=mean(hh_c03, na.rm=T),
                                                        literate_perc=mean(hh_c02, na.rm=T))

allvars_df <- merge(allvars_df, hh_ed, na.rm=T)

# Employment Ratio
hh_employ <- tbl(con, 'flagging__household_secE') %>%
  select(`Household ID`, flag, hh_e08) %>%
  data.frame %>% flagFilter

hh_employ$hh_e08 <- as.numeric(hh_employ$hh_e08)
hh_employ$hh_e08[hh_employ$hh_e08==2] <- 0

hh_employ <- hh_employ %>% group_by(Household.ID) %>%
  summarize(employ_pct=mean(hh_e08, na.rm=T))

allvars_df <- merge(allvars_df, hh_employ, all.x=T)

# Nutrition
nutr <- tbl(con, 'indicators__nutrition') %>%
  group_by(`Household ID`) %>%
  summarize(mean_len_z=mean(zlen, na.rm=T),
            mean_wei_z=mean(zwei, na.rm=T),
            mean_wfl_z=mean(zwfl, na.rm=T))

allvars_df <- merge(allvars_df, nutr, all.x=T)

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

#BIG data quality issue: 0s vs NAs.  Need to address this!

#We've removed flagged outliers.  But if they are real values and not outliers,
#  then we are missing vital info.

#dealing with seasons and fields - right now we treat one field, two seasons as two fields

