library(ggplot2)
library(RPostgreSQL)
library(dplyr)

source('production_connection.R')
vs_db <- src_postgres(dbname=dbname, host=host,
                      user=user, password=password,
                      port=port)


# Need to join back to agric using the parent_id
agric <- tbl(vs_db, 'c__household') %>%
  select(id, country, hh_refno, landscape_no) %>%
  collect()

ext <- tbl(vs_db, 'c__household_extension') %>%
  select(parent_id, source_name,
         ext_ag_prod=ag12a_02_1,
         ext_ag_proc=ag12a_02_2,
         ext_marketing=ag12a_02_3,
         ext_fish_prod=ag12a_02_4,
         ext_livestock_prod=ag12a_02_5,
         ext_livestock_disease=ag12a_02_6) %>%
  collect() %>%
  full_join(agric , by=c('parent_id'='id'))

# No processing extension except in Ghana
group_by(ext, country, landscape_no) %>%
  summarise(ext_ag_proc_frac=sum(ext_ag_proc==1, na.rm=TRUE)/n()) %>%
  ggplot() +
  geom_bar(aes(landscape_no, ext_ag_proc_frac, fill=country), stat='identity') +
  facet_grid(country~.)

# Good distribution of production extension
group_by(ext, country, landscape_no) %>%
  summarise(ext_ag_prod_frac=sum(ext_ag_prod==1, na.rm=TRUE)/n()) %>%
  ggplot() +
  geom_bar(aes(landscape_no, ext_ag_prod_frac, fill=country), stat='identity') +
  facet_grid(country~.)

# No marketing extension except Ghana, some areas of TZA
group_by(ext, country, landscape_no) %>%
  summarise(ext_marketing_frac=sum(ext_marketing==1, na.rm=TRUE)/n()) %>%
  ggplot() +
  geom_bar(aes(landscape_no, ext_marketing_frac, fill=country), stat='identity') +
  facet_grid(country~.)

# Collapse ext dataframe into a df with just an indicator of access to ag 
# production extension:
#
# TODO: Note that some records are duplicated in agric, so until this is 
# cleaned, the below could be throwing out the wrong data
ext_final <- group_by(ext, country, landscape_no, hh_refno) %>%
  summarise(ext_ag_prod=sum(ext_ag_prod==1, na.rm=TRUE) > 0)
group_by(ext_final, country, landscape_no) %>%
  summarise(ext_ag_prod_frac=sum(ext_ag_prod==1)/n()) %>%
  ggplot() +
  geom_bar(aes(landscape_no, ext_ag_prod_frac, fill=country), stat='identity') +
  facet_grid(country~.)

###############################################################################
### Inputs

#  ag3a_34 - What was the main type of pesticide/herbicide that you applied? 
#  {1: 'Pesticide', 2: 'Herbicide', 3: 'Fungicide'}

#  fd35_24a_* - Select all the types of inorganic fertilizer that  you used on this field
#   fd35_24a_dap
#   fd35_24a_urea
#   fd35_24a_tsp
#   fd35_24a_can
#   fd35_24a_sa
#   fd35_24a_npk
#   fd35_24a_mrp

inputs <- tbl(vs_db, "c__household_field_season") %>%
  select(id, parent_id, country, landscape_no,
         hh_refno, field_no, ag3a_59, 
         starts_with('fd33_18a_'), starts_with('fd35_24a_'), ag3a_09, 
         ag3a_06, flag) %>%
  collect()

# Was field irrigated in last season?
inputs$irrigation <- inputs$ag3a_09 == 1

inputs$soil_quality <- ordered(inputs$ag3a_06,
                               levels=c(3, 2, 1),
                               labels=c('bad', 'average', 'good'))

# What was the main type of pesticide/herbicide that you applied? 
inputs$pesticide <- inputs$ag3a_34 == 1
inputs$herbicide <- inputs$ag3a_34 == 2
inputs$fungicide <- inputs$ag3a_34 == 3

# Organic and inorganic fertilizer are coded with a text 't' or 'f'
inputs[inputs == 't'] <- 1
inputs[inputs == 'f'] <- 0

inputs$fd33_18a_1 <- as.numeric(inputs$fd33_18a_1)
inputs$fd33_18a_2 <- as.numeric(inputs$fd33_18a_2)
inputs$fd33_18a_3 <- as.numeric(inputs$fd33_18a_3)
inputs$fd33_18a_4 <- as.numeric(inputs$fd33_18a_4)
inputs$fd33_18a_5 <- as.numeric(inputs$fd33_18a_5)
inputs$fd33_18a_6 <- as.numeric(inputs$fd33_18a_6)
inputs$fd33_18a_7 <- as.numeric(inputs$fd33_18a_7)
inputs$fd33_18a_8 <- as.numeric(inputs$fd33_18a_8)
inputs$fert_org_any <- with(inputs, fd33_18a_1 | fd33_18a_2 | fd33_18a_3 | 
                              fd33_18a_4 | fd33_18a_5 | fd33_18a_6 | fd33_18a_7 | 
                              fd33_18a_8)
inputs$fert_org_any[is.na(inputs$fert_org_any)] <- FALSE

inputs$fd35_24a_urea <- as.numeric(inputs$fd35_24a_urea)
inputs$fd35_24a_dap <- as.numeric(inputs$fd35_24a_dap)
inputs$fd35_24a_tsp <- as.numeric(inputs$fd35_24a_tsp)
inputs$fd35_24a_can <- as.numeric(inputs$fd35_24a_can)
inputs$fd35_24a_sa <- as.numeric(inputs$fd35_24a_sa)
inputs$fd35_24a_npk <- as.numeric(inputs$fd35_24a_npk)
inputs$fd35_24a_mrp <- as.numeric(inputs$fd35_24a_mrp)
inputs$fert_inorg_any <- with(inputs, fd35_24a_urea | fd35_24a_dap | fd35_24a_tsp | 
                                fd35_24a_can | fd35_24a_sa | fd35_24a_npk | fd35_24a_mrp)
# Set NAs to FALSEs
inputs$fert_inorg_any[is.na(inputs$fert_inorg_any)] <- FALSE
inputs <- select(inputs, country, landscape_no, hh_id, field_id, survey_id, 
                 fert_org_any, fert_inorg_any, irrigation, pesticide, 
                 herbicide, fungicide, soil_quality)

###############################################################################
### Yields

# Yields - get the quantile for crop-unit
#   ag4a_08 - Area (Acres) Farmers estimate
#   ag4a_15 - Amount
#   ag4a_15_unit - Unit  {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}

# TODO: Need to work out units issue - a lot of units are missing
yields <- tbl(vs_db, "c__agric_field_season_crop") %>%
  filter(!is.na(ag4a_15_unit) & !is.na(ag4a_08) & !is.na(ag4a_15) & ag4a_08 > 0) %>%
  select(survey_id, country=Country, landscape_no=`Landscape #`,
         hh_id=`Household ID`, field_id=`Field ID`, survey_id,
         crop_name=`Crop name`, ag4a_08, ag4a_15, ag4a_15_unit,
         planting_date=ag4a_vs_5a, ag4a_19, ag4a_23) %>%
  collect()
yields$yield <- yields$ag4a_15/yields$ag4a_08

ggplot(yields) + geom_bar(aes(planting_date))
yields <- filter(yields, planting_date > as.Date('2013/1/1'))
ggplot(yields) + geom_bar(aes(planting_date))

# ag41_19: did you purchase any seed (1 is yes)
# ag4a_23: what type of seed did you purchase
yields$improved_seed <- yields$ag4a_19 == 1 & (yields$ag4a_23 %in% c(2, 3))

percentile <- function(x) {
  f <- ecdf(x)
  f(x)
}
yields <- group_by(yields, crop_name, ag4a_15_unit) %>%
  mutate(yield_percentile=percentile(yield)) %>%
  ungroup()

yields <- select(yields, country, landscape_no, hh_id, field_id, crop_name, 
                 yield, yield_percentile, yield_units=ag4a_15_unit,
                 improved_seed, planting_date)

###############################################################################
### Todos

# TODO: Add soil quality

###############################################################################
### Final merge


###############
# Final results

d <- inner_join(inputs, yields)
d <- inner_join(d, ext_final)

save(d, file='hh_data.RData')
