# Introduction ------------------------------------------------------------

# Desmond Eviction Data Exploration
# April 2018
# Nich Tremper
# nichtremper@gmail.com

# Input: https://data-downloads.evictionlab.org/
# Output: DC specific data set, tidy data for further exploration
# Purpose: Figure out what's in the Desmond data set

# libraries
library(tidyverse)
library(stringr)

# load data, keep DC data, save that to repo, remove all_evict
all_evict <- read_csv('/Users/nichtremper/Google Drive/Data Sets/desmond_eviction.csv')
dc_evict <- all_evict %>%
  mutate(state_code = str_sub(GEOID, 1, 2)) %>%
  filter(state_code == '11') %>%
  write_csv('/Users/nichtremper/Google Drive/data_work/housing/dc_eviction.csv') %>%
  rename(
    parent_location = `parent-location`,
    poverty_rate = `poverty-rate`,
    pct_renter_occupied = `pct-renter-occupied`,
    median_gross_rent = `median-gross-rent`,
    median_household_income = `median-household-income`,
    median_property_value = `median-property-value`,
    rent_burden = `rent-burden`,
    pct_white = `pct-white`,
    pct_af_amer = `pct-af-am`,
    pct_hispanic = `pct-hispanic`,
    pct_nat_amer = `pct-am-ind`,
    pct_asian = `pct-asian`, 
    pct_pac_isl = `pct-nh-pi`,
    pct_mult = `pct-multiple`,
    pct_other = `pct-other`,
    renter_occupied_households = `renter-occupied-households`,
    eviction_filings = `eviction-filings`,
    eviction_rate = `eviction-rate`,
    eviction_filing_rate = `eviction-filing-rate`
  )
rm(all_evict)

# 2016 --------------------------------------------------------------------

## create 2016 dataset for DC and substring geoid, keep unique block group
dc_2016_block_group <- dc_evict %>%
  filter(year == 2016) %>%
  mutate(
    state = str_sub(GEOID, 1, 2),
    county = str_sub(GEOID, 3, 5),
    tract = str_sub(GEOID, 6, 11),
    block_group = str_sub(GEOID, 12, -1L)
  ) %>% 
  filter(block_group != "") %>%
  mutate(income_quartile = ntile(median_household_income, 4))

## gut check on a couple variables
sum(dc_2016_block_group$population)
### 647,484 people

nrow(dc_2016_block_group)
### 450 block groups

summary(dc_2016_block_group$rent_burden)
### 50% max rent burden, which is cap reported

## Eviction rates by block group median HH income
rate_sum_stats_HH_income <- dc_2016_block_group %>%
  group_by(income_quartile) %>%
  summarize(
    min_evic_rate = round(min(eviction_rate), 3),
    p25_evic_rate = round(quantile(eviction_rate, probs = .25), 3),
    med_evic_rate = round(median(eviction_rate), 3),
    p75_evic_rate = round(quantile(eviction_rate, probs = .75), 3),
    max_evic_rate = round(max(eviction_rate), 3),
    mean_evic_rate = round(mean(eviction_rate), 3)
  ) %>%
  ungroup

dc_2016_block_group <- left_join(dc_2016_block_group, rate_sum_stats_HH_income,
                                 by = c('income_quartile'))


