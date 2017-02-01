library(dplyr)

setwd('D:/Documents and Settings/mcooper/GitHub/vitalsigns-analysis/AgIntensification_Livelihoods/')

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

#####################################################
#Independant Vars indicative of Ag Intensification
#####################################################

allvars <- tbl(con, "flagging__agric") %>%
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

allvars <- merge(allvars, field_size, all.x=T)

#####################
# Intercropping  - agric_crops_by_field
#   ag4a_04 - Was cultivation intercropped? {1: 'Yes', 2: 'No'}

intercropping <- tbl(con, "flagging__agric_crops_by_field") %>%
  select(survey_uuid, ag4a_04, flag) %>%
  data.frame %>% flagFilter

intercropping$ag4a_04 <- as.numeric(intercropping$ag4a_04)
intercropping$ag4a_04[intercropping$ag4a_04==2] <- 0

intercropping <- intercropping %>% group_by(survey_uuid) %>%
  summarize(intercrop_rate = mean(ag4a_04, na.rm=T))

allvars <- merge(allvars, intercropping, all.x=T)

#################################
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
  select(survey_uuid, ag3a_34, ag3a_33,
         fd35_24a_dap, fd35_24a_urea, fd35_24a_tsp,
         fd35_24a_can, fd35_24a_sa, fd35_24a_npk, fd35_24a_mrp, flag) %>%
  data.frame %>% flagFilter

inputs$pesticide <- inputs$ag3a_34==1
inputs$herbicide <- inputs$ag3a_34==2
inputs$fungicide <- inputs$ag3a_34==3

inputs$pesticide[inputs$ag3a_33=='2'] <- 0
inputs$herbicide[inputs$ag3a_33=='2'] <- 0
inputs$fungicide[inputs$ag3a_33=='2'] <- 0


#Fertilizer data is missing for all forms before May 2016:
#'
#'SELECT DISTINCT b.xform_name, a.fd35_24a_dap IS NULL FROM agric_field_details a
#'JOIN form_log b ON a.survey_uuid = b.survey_uuid
#'ORDER BY xform_name
#'
#'So leave out forms for now
# 
# inputs[inputs=='t'] <- 1
# inputs[inputs=='f'] <- 0
# 
# inputs$fd35_24a_urea <- as.numeric(inputs$fd35_24a_urea)
# inputs$fd35_24a_dap <- as.numeric(inputs$fd35_24a_dap)
# inputs$fd35_24a_tsp <- as.numeric(inputs$fd35_24a_tsp)
# inputs$fd35_24a_can <- as.numeric(inputs$fd35_24a_can)
# inputs$fd35_24a_sa <- as.numeric(inputs$fd35_24a_sa)
# inputs$fd35_24a_npk <- as.numeric(inputs$fd35_24a_npk)
# inputs$fd35_24a_mrp <- as.numeric(inputs$fd35_24a_mrp)

inputs[is.na(inputs)] <- 0

inputs <- inputs %>% group_by(survey_uuid) %>%
  summarize(pesticide = mean(pesticide, na.rm=T), herbicide = mean(herbicide, na.rm=T),
            fungicide = mean(fungicide, na.rm=T)#, 
#             dap = mean(fd35_24a_dap, na.rm=T),
#             urea = mean(fd35_24a_urea, na.rm=T), tsp = mean(fd35_24a_tsp, na.rm=T),
#             can = mean(fd35_24a_can, na.rm=T), sa = mean(fd35_24a_sa, na.rm=T),
#             npk = mean(fd35_24a_npk, na.rm=T), mrp = mean(fd35_24a_mrp, na.rm=T)
            )

allvars <- merge(allvars, inputs, all.x=T)

########################
# Yields - get the Z score for crop-unit
#   ag4a_08 - Area (Acres) Farmers estimate
#   ag4a_15 - Amount
#   ag4a_15_unit - Unit  {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}
yields <- tbl(con, "flagging__agric_crops_by_field") %>%
  filter(!is.na(ag4a_15) & !is.na(ag4a_08) & ag4a_08 > 0) %>%
  select(survey_uuid, `Crop name`, ag4a_08, ag4a_15, ag4a_15_unit, flag) %>%
  data.frame %>% flagFilter

yields$yield <- yields$ag4a_15/yields$ag4a_08

yields$cs <- paste0(yields$Crop.name, yields$ag4a_15_unit)

for (c in unique(yields$cs)){
  sel <- yields$yield[yields$cs==c]
  if (length(sel) > 4){
    sel <- sel + rnorm(length(sel), 0, .1) #slight jitter, because cut() doesnt work with repeats
    sel <- as.integer(cut(sel, quantile(sel, probs=0:4/4), include.lowest=TRUE))
    yields$yield_quantile[yields$cs==c] <- sel
  }else{
    yields$yield_quantile[yields$cs==c] <- NA
  }
}

yields <- yields %>% group_by(survey_uuid) %>% summarize(yield_quantile = mean(yield_quantile, na.rm=T))

allvars <- merge(allvars, yields, all.x=T)

########################
#Hired labor per Area
# ag3a_38_1	"Woman Days - During the last completed Long Rainy Season / Major Cropping Season, how many days did your household have hired labor for this field for LAND PREPARATION AND PLANTING?"
# ag3a_38_2	"Man Days - During the last completed Long Rainy Season / Major Cropping Season, how many days did your household have hired labor for this field for LAND PREPARATION AND PLANTING?"
# ag3a_38_21	"Child Days - During the last completed Long Rainy Season / Major Cropping Season, how many days did your household have hired labor for this field for LAND PREPARATION AND PLANTING?"
# ag3a_38_4	"Woman Days - During the last completed Long Rainy Season / Major Cropping Season, how many days did your household have hired labor for this field for WEEDING?"
# ag3a_38_5	"Man Days - During the last completed Long Rainy Season / Major Cropping Season, how many days did your household have hired labor for this field for WEEDING?"
# ag3a_38_51	"Child Days - During the last completed Long Rainy Season / Major Cropping Season, how many days did your household have hired labor for this field for WEEDING?"
# ag3a_38_61	"Man Days - During the last completed Long Rainy Season / Major Cropping Season, how many days did your household have hired labor for this field for RIDGING, FERTILIZING, OTHER-NON-HARVEST ACTIVITIES?"
# ag3a_38_62	"Woman Days - During the last completed Long Rainy Season / Major Cropping Season, how many days did your household have hired labor for this field for RIDGING, FERTILIZING, OTHER-NON-HARVEST ACTIVITIES?"
# ag3a_38_63	"Child Days - During the last completed Long Rainy Season / Major Cropping Season, how many days did your household have hired labor for this field for RIDGING, FERTILIZING, OTHER-NON-HARVEST ACTIVITIES?"

labor_hired <- tbl(con, "flagging__agric_field_details") %>%
  select(survey_uuid, flag,
         ag3a_38_1, ag3a_38_2, ag3a_38_21, ag3a_38_4, ag3a_38_5, 
         ag3a_38_51, ag3a_38_61, ag3a_38_62, ag3a_38_63, ag3a_38_7,
         ag3a_38_8, ag3a_38_81) %>%
  data.frame %>% flagFilter

labor_hired[is.na(labor_hired)] <- 0

labor_hired$labor_hired <- labor_hired$ag3a_38_1 + labor_hired$ag3a_38_4 + labor_hired$ag3a_38_61 + labor_hired$ag3a_38_7 + 
  labor_hired$ag3a_38_2 + labor_hired$ag3a_38_5 + labor_hired$ag3a_38_62 + labor_hired$ag3a_38_8 + 
  labor_hired$ag3a_38_21 + labor_hired$ag3a_38_51 + labor_hired$ag3a_38_63 + labor_hired$ag3a_38_81

labor_hired <- labor_hired %>% group_by(survey_uuid) %>%
  summarize(labor_hired = sum(labor_hired, na.rm=T))

allvars <- merge(allvars, labor_hired, all.x=T)
allvars$labor_hired <- allvars$labor_hired/allvars$total_acreage

###########################
#Household labor per Area
labor_hh <- tbl(con, "flagging__agric_field_details_labor") %>%
  select(survey_uuid, flag, ag3a_70_preparing, ag3a_70_weeding,
         ag3a_70_fertilizing, ag3a_70_harvesting) %>%
  data.frame %>% flagFilter

labor_hh[is.na(labor_hh)] <- 0

labor_hh <- labor_hh %>%
  group_by(survey_uuid) %>% 
  summarize(labor_hh=sum(ag3a_70_preparing + ag3a_70_weeding +
                         ag3a_70_fertilizing + ag3a_70_harvesting, na.rm=T))

allvars <- merge(allvars, labor_hh, all.x=T)
allvars$labor_hh <- allvars$labor_hh/allvars$total_acreage

#########################################################
#Amount Sold
#   ag5a_01 - Did you sell any of the ${fd5_crop_name} produced
#   ag5a_02_1 - Amount
#   ag5a_02_2 - Unit {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}
sold <- tbl(con, "flagging__agric_crops_by_hh") %>%
  select(survey_uuid, `Crop name`, Season, ag5a_01, ag5a_02_1, ag5a_02_2, flag) %>%
  data.frame %>% flagFilter

#   ag4a_08 - Area (Acres) Farmers estimate
#   ag4a_15 - Amount
#   ag4a_15_unit - Unit  {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}

# yields <- tbl(con, "flagging__agric_crops_by_field") %>%
#   filter(!is.na(ag4a_08) & !is.na(ag4a_15) & ag4a_08 > 0) %>%
#   select(survey_uuid, `Crop name`, Season, ag4a_08, ag4a_15, ag4a_15_unit, flag) %>%
#   data.frame %>% flagFilter %>%
#   group_by(survey_uuid, Crop.name, ag4a_15_unit) %>%
#   summarize(ag4a_15=sum(ag4a_15, na.rm=T))
# 
# names(yields)[names(yields)=='ag4a_15_unit'] <- 'ag5a_02_2'
# 
# sold <- merge(sold, yields, all=T)

sold$ag5a_01 <- as.numeric(sold$ag5a_01)
sold$ag5a_01[sold$ag5a_01==2] <- 0

# sold$percent_sold <- sold$ag5a_02_1/sold$ag4a_15
# sold$percent_sold[sold$percent_sold > 1] <- NA

sold <- sold %>% group_by(survey_uuid) %>%
  summarize(#avg_pct_harvest_sold = mean(percent_sold, na.rm=T),
            avg_pct_crops_any_sold = mean(ag5a_01, na.rm=T)) %>%
  data.frame

allvars <- merge(allvars, sold, all.x=T)

###########################################
# Irrigaion
#   ag3a_09 - Was this FIELD irrigated in the last completed

irrig <- tbl(con, 'flagging__agric_field_details') %>% 
  select(survey_uuid, flag, ag3a_09) %>%
  data.frame %>% flagFilter

irrig$ag3a_09 <- as.numeric(irrig$ag3a_09)
irrig$ag3a_09[irrig$ag3a_09==2] <- 0

irrig <- irrig %>% group_by(survey_uuid) %>%
  summarize(pct_fields_irrigated = mean(ag3a_09, na.rm=T))

allvars <- merge(allvars, irrig, all.x=T)

# Inorganic Fertilizers
#  fd33_18a_* - Select all types of organic fertilizer that you used
#    fd33_18a_1: 'Crop Residue', 
#    fd33_18a_2 'Animal Manure', 
#    fd33_18a_4 'Natural Fallow', 
#    fd33_18a_5 'Leguminous Tree Fallow', 
#    fd33_18a_6 'Leguminous Cover Crop', 
#    fd33_18a_7 'Biomass Transfer', 
#    fd33_18a_8 'Compost'

######################
#Seed Variety
# agric_crops_by_field
#   ag4a_19 - Did you purchase any SEED for ${fd4_crop_name} in the last completed Long Rainy Season / Major Cropping Season?
#   ag4a_21 - What type of seed did you purchase ?  {1: 'Traditional', 2: 'Purchased Improved Seeds', 3: 'Saved Improved Seeds'}
seed <- tbl(con, 'flagging__agric_crops_by_field') %>%
  select(survey_uuid, flag, ag4a_19, ag4a_21) %>%
  data.frame %>% flagFilter

seed$ag4a_19 <- as.numeric(seed$ag4a_19)
seed$ag4a_19[seed$ag4a_19==2] <- 0

seed <- seed %>% group_by(survey_uuid) %>% summarize(pct_buy_seed=mean(ag4a_19, na.rm=T))

allvars <- merge(allvars, seed, all.x=T)

#################
#Fallows
# agric_field_roster
#   ag2a_vs_2b1 - What was the use of this field during the last completed Long Rainy season?
#   ag2a_vs_2c - What was the use of this field during the last completed Short Rainy Season / Minor Cropping Season?

#   {1: 'Cultivated', 2: 'Rented Out', 3: 'Given Out', 4: 'Fallow', 5: 'Forest'}

fallow <- tbl(con, 'flagging__agric_field_roster') %>%
  select(survey_uuid, flag, ag2a_vs_2b1, ag2a_vs_2c) %>%
  data.frame %>% flagFilter

fallow$fallow <- mapply(fallow$ag2a_vs_2b1=='4', fallow$ag2a_vs_2c=='4', FUN=sum, na.rm=T)

fallow <- fallow %>% group_by(survey_uuid) %>% summarize(fallow=sum(fallow, na.rm=T)/n())

allvars <- merge(allvars, fallow, all.x=T)

#################
#Tools?
# agric_implement
#   Tool name
#   ag11_01 - How many ${fd11_tool_name} does the household own?

tools <- tbl(con, 'flagging__agric_implement') %>%
  select(survey_uuid, flag, `Tool name`, ag11_01) %>%
  data.frame %>% flagFilter

tools$tractor <- tools$Tool.name=="TRACTOR PLOUGH" | tools$Tool.name=="TRACTOR" | tools$Tool.name=="TRACTOR HARROW"

tools$ag11_01[!tools$tractor] <- 0

tools <- tools %>% group_by(survey_uuid) %>% summarize(tractor=sum(tractor, na.rm=T))

allvars <- merge(allvars, tools, all.x=T)

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

allvars <- merge(allvars, size, all.x=T)

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

allvars <- merge(allvars, hhe, all.x=T)

# Ag Income - (((ag10a_21+ag10a_27)-ag10a_34) + ag7a_04 + ag10b_06 + (ag09_08 - ag09_11) + (ag5a_03 + ag5a_26)) * [[exchange rate]]
#  agric_crops_by_hh; 
#    ag5a_03
#    ag5a_26

inc1 <- tbl(con, 'flagging__agric_crops_by_hh') %>%
  select(`Household ID`, flag,
         ag5a_03, ag5a_26) %>%
  data.frame# %>% flagFilter

inc1$income_crops <- mapply(inc1$ag5a_03, inc1$ag5a_26, FUN=sum, na.rm=T)

inc1[is.na(inc1)] <- 0

inc1 <- inc1 %>% group_by(Household.ID) %>% summarize(income_crops=sum(income_crops, na.rm=T))

allvars <- merge(allvars, inc1, all.x=T)
allvars$income_crops[is.na(allvars$income_crops)] <- 0

#  agric_perm_crop; 
#    ag7a_04

inc2 <- tbl(con, 'flagging__agric_perm_crop') %>%
  select(`Household ID`, flag, ag7a_04) %>%
  data.frame# %>% flagFilter

inc2[is.na(inc2)] <- 0
  
inc2 <- inc2 %>%
  group_by(Household.ID) %>%
  summarize(income_perm_crop=sum(ag7a_04, na.rm=T))

allvars <- merge(allvars, inc2, all.x=T)
allvars$income_perm_crop[is.na(allvars$income_perm_crop)] <- 0

#  agric_livestock_byproduct; 
#    ag10b_06

inc3 <- tbl(con, 'flagging__agric_livestock_byproduct') %>%
  select(`Household ID`, flag, ag10b_06) %>%
  data.frame# %>% flagFilter
  
inc3[is.na(inc3)] <- 0
  
inc3 <- inc3 %>%
  group_by(Household.ID) %>%
  summarize(income_lvstk_byprod=sum(ag10b_06, na.rm=T))

allvars <- merge(allvars, inc3, all.x=T)
allvars$income_lvstk_byprod[is.na(allvars$income_lvstk_byprod)] <- 0

#  agric_byproduct; 
#    ag09_08
#    ag09_11

inc4 <- tbl(con, 'flagging__agric_byprod') %>%
  select(`Household ID`, flag, ag09_08, ag09_11) %>%
  data.frame# %>% flagFilter

inc4$ag09_11[is.na(inc4$ag09_11)] <- 0

inc4$income_byprod <- inc4$ag09_08 - inc4$ag09_11

inc4[is.na(inc4)] <- 0

inc4 <- inc4 %>% group_by(Household.ID) %>% summarize(income_byprod=sum(income_byprod, na.rm=T))

allvars <- merge(allvars, inc4, all.x=T)
allvars$income_byprod[is.na(allvars$income_byprod)] <- 0

#  agric_livestock
#    ag10a_21
#    ag10a_27
#    ag10a_34

inc5 <- tbl(con, 'flagging__agric_livestock') %>%
  select(`Household ID`, flag, ag10a_21, ag10a_27, ag10a_34) %>%
  data.frame# %>% flagFilter

inc5$income_lvstk <- mapply(inc5$ag10a_21, inc5$ag10a_27, FUN=sum, na.rm=T)

inc5$ag10a_34[is.na(inc5$ag10a_34)] <- 0

inc5$income_lvstk <- inc5$income_lvstk - inc5$ag10a_34

inc5[is.na(inc5)] <- 0

inc5 <- inc5 %>% group_by(Household.ID) %>% summarize(income_lvstk = sum(income_lvstk, na.rm=T))

allvars <- merge(allvars, inc5, all.x=T)
allvars$income_lvstk[is.na(allvars$income_lvstk)] <- 0

# Ratio of Laborers to Dependants
hh_labor <- tbl(con, 'flagging__household_secE') %>%
  select(`Household ID`, `Individual ID`, flag, hh_e04) %>%
  data.frame %>% flagFilter

hh_labor$hh_e04 <- as.numeric(hh_labor$hh_e04)
hh_labor$hh_e04[hh_labor$hh_e04==2] <- 0

hh_labor[is.na(hh_labor)] <- 0

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

labor <- merge(hh_labor, ag_labor, all.x=F) %>% unique
labor$ag_or_hh <- labor$work | labor$hh_e04

labor <- labor %>% group_by(Household.ID) %>% summarize(labor_pct=mean(ag_or_hh, na.rm=T))

allvars <- merge(allvars, labor, all.x=T)

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

allvars <- merge(allvars, hh_ed, all.x=T)

# Employment Ratio
hh_employ <- tbl(con, 'flagging__household_secE') %>%
  select(`Household ID`, flag, hh_e08) %>%
  data.frame %>% flagFilter

hh_employ$hh_e08 <- as.numeric(hh_employ$hh_e08)
hh_employ$hh_e08[hh_employ$hh_e08==2] <- 0

hh_employ <- hh_employ %>% group_by(Household.ID) %>%
  summarize(employ_pct=mean(hh_e08, na.rm=T))

allvars <- merge(allvars, hh_employ, all.x=T)

####################################################
#Not Enough Food in Past Year
food <- tbl(con, 'flagging__household_secI') %>%
  select(`Household ID`, flag, hh_i08) %>%
  data.frame %>% flagFilter

food$food <- as.numeric(food$hh_i08)
food$food[food$food==2] <- 0

food$hh_i08 <- NULL

allvars <- merge(allvars, food, all.x=T)

#####################
# Nutrition
nutr <- tbl(con, 'indicators__nutrition') %>%
  select(Individual.ID, Household.ID, zlen, zwei, zwfl) %>%
  data.frame

allvars_ind <- merge(allvars, nutr, by='Household.ID', all.x=T)

######################################
#Combine and Analyze
######################################
library(lme4)
library(lmerTest)

plotIt <- function(mod, main){
  dat <- summary(mod)$coefficients %>% as.data.frame
  dat$Variables <- row.names(dat)
  ggplot(dat) + geom_point(aes(Estimate, Variables)) + 
    geom_errorbarh(aes(xmax=Estimate+`Std. Error`, xmin=Estimate-`Std. Error`, y=Variables, x=Estimate)) + 
    geom_vline(xintercept=0, linetype='longdash') + 
    ggtitle(main)
  ggsave(paste0(main, '.png'))
}

rescale <- function(df){
  for (i in names(df)[!names(df) %in% c('Individual.ID', 'Household.ID', 'survey_uuid', 'Country', 'Landscape..')]){
    #print(i)
    df[ , i] <- (df[ , i] + min(df[ , i], na.rm=T))/max(df[ , i], na.rm=T)
  }
  df
}

allvars$income <- allvars$income_lvstk + allvars$income_own + allvars$income_byprod +
  allvars$income_lvstk_byprod + allvars$income_perm_crop + allvars$income_crops + allvars$income_wage

allvars_rsc <- rescale(allvars)
allvars_ind_rsc <- rescale(allvars_ind)

#labor_pct
labor_pct <- lmer(labor_pct ~ yield_quantile + average_field_size + total_acreage + intercrop_rate + pesticide + herbicide + fungicide + labor_hired + labor_hh + avg_pct_crops_any_sold + pct_fields_irrigated + hh_size + pct_buy_seed + fallow + tractor + (1|Landscape..) + (1|Country), data=allvars_rsc)
summary(labor_pct)
plotIt(labor_pct, 'Labor_Ratio')

#any_ed_perc
any_ed_perc <- lmer(any_ed_perc ~ yield_quantile + average_field_size + total_acreage + intercrop_rate + pesticide + herbicide + fungicide + labor_hired + labor_hh + avg_pct_crops_any_sold + pct_fields_irrigated + hh_size + pct_buy_seed + fallow + tractor + (1|Landscape..) + (1|Country), data=allvars_rsc)
summary(any_ed_perc)
plotIt(labor_pct, 'Education')

#literate_perc
literate_perc <- lmer(literate_perc ~ yield_quantile + average_field_size + total_acreage + intercrop_rate + pesticide + herbicide + fungicide + labor_hired + labor_hh + avg_pct_crops_any_sold + pct_fields_irrigated + hh_size + pct_buy_seed + fallow + tractor + (1|Landscape..) + (1|Country), data=allvars_rsc)
summary(literate_perc)
plotIt(literate_perc, 'Literacy')

#employ_perc
employ_pct <- lmer(employ_pct ~ yield_quantile + average_field_size + total_acreage + intercrop_rate + pesticide + herbicide + fungicide + labor_hired + labor_hh + avg_pct_crops_any_sold + pct_fields_irrigated + hh_size + pct_buy_seed + fallow + tractor + (1|Landscape..) + (1|Country), data=allvars_rsc)
summary(employ_pct)
plotIt(employ_pct, 'Employment')

#income
income <- lmer(income ~ yield_quantile + average_field_size + total_acreage + intercrop_rate + pesticide + herbicide + fungicide + labor_hired + labor_hh + avg_pct_crops_any_sold + pct_fields_irrigated + hh_size + pct_buy_seed + fallow + tractor + (1|Landscape..) + (1|Country), data=allvars_rsc)
summary(income)
plotIt(income, 'Income')

#food
foodsec <- glmer(food ~ yield_quantile + average_field_size + total_acreage + intercrop_rate + pesticide + herbicide + labor_hired + labor_hh + avg_pct_crops_any_sold + pct_fields_irrigated + hh_size + pct_buy_seed + fallow + tractor + (1|Landscape..) + (1|Country), data=allvars_rsc, family = 'binomial')
summary(foodsec)
plotIt(foodsec, 'Food_Security')

#zlen
zlen <- lmer(zlen ~ yield_quantile + average_field_size + total_acreage + intercrop_rate + pesticide + herbicide + fungicide + labor_hired + labor_hh + avg_pct_crops_any_sold + pct_fields_irrigated + hh_size + pct_buy_seed + fallow + tractor + (1|survey_uuid) + (1|Landscape..) + (1|Country), data=allvars_ind_rsc)
summary(zlen)
plotIt(zlen, 'zlen')

#zwei
zwei <- lmer(zwei ~ yield_quantile + average_field_size + total_acreage + intercrop_rate + pesticide + herbicide + fungicide + labor_hired + labor_hh + avg_pct_crops_any_sold + pct_fields_irrigated + hh_size + pct_buy_seed + fallow + tractor + (1|survey_uuid) + (1|Landscape..) + (1|Country), data=allvars_ind_rsc)
summary(zwei)
plotIt(zwei, 'zwei')

#zwfl
zwfl <- lmer(zwfl ~ yield_quantile + average_field_size + total_acreage + intercrop_rate + pesticide + herbicide + fungicide + labor_hired + labor_hh + avg_pct_crops_any_sold + pct_fields_irrigated + hh_size + pct_buy_seed + fallow + tractor + (1|survey_uuid) + (1|Landscape..) + (1|Country), data=allvars_ind_rsc)
summary(zwfl)
plotIt(zwfl, 'zwfl')



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

#FERTILIZER DATA GAPS!!

#Yield units.  A lot of them in liters?
#Lots of missing yield units.  Should we assume kg?

