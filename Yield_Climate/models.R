library(dplyr)
library(lme4)
library(lmerTest)
library(multcomp)
library(lubridate)
library(ggplot2)

load('hh_data.RData')
load('ls_spi.RData')

###############################################################################
### Join the SPI data based on planting date plus three months (to approximate 
### harvest date), rounded to nearest month.
d$harvest_date <- round_date(d$planting_date + months(3), 'month')

names(ls_spi)[names(ls_spi)=='date'] <- 'harvest_date'

d <- left_join(d, ls_spi)

ggplot(d) + geom_bar(aes(spi12))
table(is.na(d$spi12))
table(d$harvest_date %in% ls_spi$harvest_date)

head(as.data.frame(d[is.na(d$spi12), ]))

# Filter ridiculous yields
d <- d[d$yield < 3000, ]

# Convert soil quality to numeric for easier interpretation
d$soil_quality <- as.numeric(d$soil_quality)

###############################################################################
### Do one regression per crop for the top three crops cultivated across VS 
### landscapes

#d$ls_id <- paste(d$country, d$ls_id, sep='-')

group_by(d, country, crop_name) %>%
    summarise(n=n()) %>%
    group_by(country) %>%
    top_n(3, n) %>%
    mutate(Fraction=n/sum(n)) %>%
    ggplot() +
    geom_bar(aes(crop_name, Fraction, fill=crop_name), stat='identity') +
    facet_grid(country ~ .) + guides(fill=FALSE) +
    theme_bw(base_size=18) +
    theme(axis.title.x=element_blank()) +
    scale_fill_brewer(palette='Set2')
ggsave('crop_by_country_1.png', width=12, height=8)

group_by(d, country, crop_name) %>%
    summarise(n=n()) %>%
    group_by(country) %>%
    top_n(3, n) %>%
    mutate(Fraction=n/sum(n)) %>%
    ggplot() +
    geom_bar(aes(crop_name, Fraction, fill=country), stat='identity') +
    theme_bw(base_size=18) +
    theme(axis.title.x=element_blank()) +
    scale_fill_brewer(palette='Set2')
ggsave('crop_by_country_2.png', width=12, height=8)

ggplot(filter(d, crop_name %in% c('Beans', 'Maize'))) +
    geom_histogram(aes(yield, fill=country)) +
    facet_grid(crop_name~.) +
    theme_bw(base_size=18) +
    theme(axis.title.x=element_blank()) +
    scale_fill_brewer(palette='Set2')
ggsave('yield_by_country.png', width=12, height=8)

###############################################################################
### Maize models

# Convert units of percentile to be in deciles
d$yield_decile <- d$yield_percentile * 10

maize <- lmer(yield_decile ~ spi12 + fert_org_any + fert_inorg_any + irrigation + 
              improved_seed + ext_ag_prod + soil_quality + (1 | country/ls_id),
              data=filter(d, crop_name == 'Maize'))
summary(maize)

tmp <- as.data.frame(confint(glht(maize))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x=Comparison, y=Estimate, ymin=lwr, ymax=upr)) + 
    geom_errorbar(width=.5) +
    geom_point(size=2) +
    ylab('Effect') +
    geom_hline(aes(yintercept=0)) +
    theme_bw(base_size=18) +
    xlab('Variable') +
    coord_flip()
ggsave('maize_model_1.png', width=12, height=8)

###############################################################################
### Beans models

beans <- lmer(yield_decile ~ spi12 + fert_org_any + fert_inorg_any + irrigation + 
              improved_seed + ext_ag_prod + soil_quality + (1 | country/ls_id),
              data=filter(d, crop_name == 'Beans'))
summary(beans)

tmp <- as.data.frame(confint(glht(beans))$confint)
tmp$Comparison <- rownames(tmp)
ggplot(tmp, aes(x=Comparison, y=Estimate, ymin=lwr, ymax=upr)) + 
    geom_errorbar(width=.5) +
    geom_point(size=2) +
    ylab('Effect') +
    geom_hline(aes(yintercept=0)) +
    theme_bw(base_size=18) +
    xlab('Variable') +
    coord_flip()
ggsave('beans_model_1.png', width=12, height=8)

