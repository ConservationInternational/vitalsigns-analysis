library(ggplot2)
library(RPostgreSQL)
library(dplyr)

source('../production_connection.R')
vs_db <- src_postgres(dbname=dbname, host=host,
                      user=user, password=password,
                      port=port)


# Need to join back to agric using the parent_id
agric <- tbl(vs_db, 'c__household') %>%
  filter(round=='1') %>%
  select(id, country, hh_refno, landscape_no) %>%
  collect()

ext <- tbl(vs_db, 'c__household_extension') %>%
  filter(round=='1') %>%
  select(parent_id, source_name,
         ext_ag_prod=ag12a_02_1,
         ext_ag_proc=ag12a_02_2,
         ext_marketing=ag12a_02_3,
         ext_fish_prod=ag12a_02_4,
         ext_livestock_prod=ag12a_02_5,
         ext_livestock_disease=ag12a_02_6) %>%
  collect() %>%
  full_join(agric, by=c('parent_id'='id'))

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
ext_final <- group_by(ext, country, landscape_no, hh_refno) %>%
  summarise(ext_ag_prod=sum(ext_ag_prod==1, na.rm=TRUE) > 0)
group_by(ext_final, country, landscape_no) %>%
  summarise(ext_ag_prod_frac=sum(ext_ag_prod==1)/n()) %>%
  ggplot() +
  geom_bar(aes(landscape_no, ext_ag_prod_frac, fill=country), stat='identity') +
  facet_grid(country~.)


###############################################################################
### Inputs
###############################
inputs <- tbl(vs_db, "c__household_field_season") %>%
  filter(round=='1') %>%
  select(id, hh_refno, irrigation=ag3a_34, ag3a_59,
         starts_with('ag3a_39_'), starts_with('ag3a_45_'), 
         soil_quality=ag3a_10) %>%
  collect()

# What was the main type of pesticide/herbicide that you applied? 
inputs$ag3a_59[is.na(inputs$ag3a_59)] <- 0

inputs$pesticide <- inputs$ag3a_59 == 1
inputs$herbicide <- inputs$ag3a_59 == 2
inputs$fungicide <- inputs$ag3a_59 == 3

inputs$fert_org_any <- with(inputs, ag3a_39_1 | ag3a_39_2 | 
                              ag3a_39_4 | ag3a_39_5 | ag3a_39_6 | ag3a_39_7 | 
                              ag3a_39_8)

inputs$fert_inorg_any <- with(inputs, ag3a_45_urea | ag3a_45_dap | ag3a_45_tsp | 
                                ag3a_45_can | ag3a_45_sa | ag3a_45_npk | ag3a_45_mrp)
# Set NAs to FALSEs
inputs$fert_org_any[is.na(inputs$fert_org_any)] <- FALSE
inputs$fert_inorg_any[is.na(inputs$fert_inorg_any)] <- FALSE

inputs <- select(inputs, id, hh_refno, fert_org_any, 
                 fert_inorg_any, irrigation, pesticide, 
                 herbicide, fungicide, soil_quality)

###############################################################################
### Yields

# Yields - get the quantile for crop-unit
#   ag4a_08 - Area (Acres) Farmers estimate
#   ag4a_15 - Amount
#   ag4a_15_unit - Unit  {1: 'Kg', 2: 'Liter', 3: 'Milliliter'}

yields <- tbl(vs_db, "c__household_field_season_fieldcrop") %>%
  filter(round=='1') %>%
  filter(!is.na(ag4a_15_unit) & !is.na(ag4a_08) & !is.na(ag4a_15) & ag4a_08 > 0) %>%
  select(parent_id, crop_name, ag4a_08, ag4a_15, ag4a_15_unit,
         planting_date=ag4a_5a, ag4a_19, ag4a_23) %>%
  collect()
yields$yield <- yields$ag4a_15/yields$ag4a_08

ggplot(yields) + geom_bar(aes(planting_date))
yields <- filter(yields, planting_date > as.Date('2013/1/1'))
ggplot(yields) + geom_bar(aes(planting_date))

# ag41_19: did you purchase any seed (1 is yes)
# ag4a_23: what type of seed did you purchase
yields$improved_seed <- yields$ag4a_19 & (yields$ag4a_23 %in% c('Purchased Improved Seeds', 'Saved Improved Seeds'))

percentile <- function(x) {
  f <- ecdf(x)
  f(x)
}
yields <- group_by(yields, crop_name, ag4a_15_unit) %>%
  mutate(yield_percentile=percentile(yield)) %>%
  ungroup()

yields <- select(yields, parent_id, crop_name, 
                 yield, yield_percentile, yield_units=ag4a_15_unit,
                 improved_seed, planting_date)

###############################################################################
### Todos

# TODO: Add soil quality

###############################################################################
### Final merge


###############
# Final results

d <- inner_join(inputs, yields, by=c('id'='parent_id'))
d <- inner_join(d, ext_final)

d <- filter(d, country != 'GHA')

save(d, file='hh_data.RData')
