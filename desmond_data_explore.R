# Introduction ------------------------------------------------------------

# Desmond Eviction Data Exploration
# April 2018
# Nich Tremper
# nichtremper@gmail.com

# Input: https://data-downloads.evictionlab.org/
# Output: Initial charts showing data
# Purpose: Figure out what's in the Desmond data set

# libraries
library(tidyverse)
library(stringr)

output <- ('/Users/nichtremper/Google Drive/data_work/housing/output')

# functions
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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
  mutate(income_quartile = factor(ntile(median_household_income, 4))) %>%
  mutate(income_quartile_text = ifelse(
        income_quartile == 1, "25%", ifelse(
        income_quartile == 2, "50%", ifelse(
        income_quartile == 3, "75%", ifelse(
        income_quartile == 4, "100%", "NA"))))) %>%
  mutate(maj_white = factor(ifelse(pct_white >= 50, 1, 0))) %>%
  mutate(maj_white_text = ifelse(maj_white == 1, "Maj. white", "Maj. non-white"))

dc_2016_block_group$income_quartile_text <- 
  factor(dc_2016_block_group$income_quartile_text, levels = c("25%", "50%", "75%", "100%", "NA"))

dc_2016_block_group$maj_white_text <- 
  factor(dc_2016_block_group$maj_white_text, levels = c("Maj. white", "Maj. non-white"))

## gut check on a couple variables
sum(dc_2016_block_group$population)
### 647,484 people

nrow(dc_2016_block_group)
### 450 block groups

summary(dc_2016_block_group$rent_burden)
### 50% max rent burden, which is the reported cap 

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

## frequency graphs of eviction rates by HH income quartiles

dc_2016_block_group %>% 
  filter(income_quartile_text != "NA") %>%
  ggplot(aes(x = eviction_rate, y = (..count../sum(..count..) * 100))) +
    geom_freqpoly(breaks = seq(0, 30, by = 1), aes(color = income_quartile_text)) +
  theme(legend.position = 'bottom') +
  labs(
    title = "Eviction rate by block group and median household income",
    subtitle = "District of Columbia",
    y = "Percent of block groups",
    x = "Eviction rate (percent)",
    color = "Median household income quartile",
    caption = paste("Source: Matthew Desmond's Eviction Lab \n",
                    "www.evictionlab.org")
  )
setwd(output)
ggsave('eviction_hh_income.png', plot = last_plot(), 
       height = 6, width = 8, units = c("in"))

## eviction rate by race
dc_2016_block_group %>%
  ggplot(aes(x = eviction_rate, y = (..count../sum(..count..) * 100))) +
    geom_freqpoly(breaks = seq(0, max(dc_2016_block_group$eviction_rate), by = 1), 
                  aes(color = factor(dc_2016_block_group$maj_white_text))) +
    theme(legend.position = 'bottom') +
    labs(
      title = "Eviction rate by majority race in census block group",
      subtitle = "District of Columbia", 
      y = "Percent of census block groups",
      x = "Eviction rate (percent)",
      color = "Majority race in block group",
      caption = paste("Source: Matthew Desmond's Eviction Lab \n",
                      "www.evictionlab.org")
    )
setwd(output)
ggsave('eviction_race.png', plot = last_plot(), 
       height = 6, width = 8, units = c("in"))

## now, mult plots by income & race
dc_2016_block_group1 <- dc_2016_block_group %>% filter(income_quartile == 1)
g1 <- dc_2016_block_group1 %>%
  ggplot(aes(x = eviction_rate)) +
    geom_freqpoly(breaks = seq(0, max(dc_2016_block_group1$eviction_rate), by = 1),
                  aes(color = factor(dc_2016_block_group1$maj_white_text))) +
  theme(legend.position = 'none') +
  labs(
    title = "Median income quartile: 25%",
    y = "Number of census block groups",
    x = "",
    color = "Majority race in block group"
  )

dc_2016_block_group2 <- dc_2016_block_group %>% filter(income_quartile == 2)
g2 <- dc_2016_block_group2 %>%
  ggplot(aes(x = eviction_rate)) +
  geom_freqpoly(breaks = seq(0, max(dc_2016_block_group2$eviction_rate), by = 1),
                aes(color = factor(dc_2016_block_group2$maj_white_text))) +
  theme(legend.position = 'none') +
  labs(
    title = "Median income quartile: 50%",
    y = "",
    x = ""
  )

dc_2016_block_group3 <- dc_2016_block_group %>% filter(income_quartile == 3)
g3 <- dc_2016_block_group3 %>%
  ggplot(aes(x = eviction_rate)) +
  geom_freqpoly(breaks = seq(0, max(dc_2016_block_group3$eviction_rate), by = 1),
                aes(color = factor(dc_2016_block_group3$maj_white_text))) +
  theme(legend.position = 'none') +
  labs(
    title = "Median income quartile: 75%",
    y = "Number of census block groups",
    x = "Eviction rate (percent)"
  ) 

dc_2016_block_group4 <- dc_2016_block_group %>% filter(income_quartile == 4)
g4 <- dc_2016_block_group4 %>%
  ggplot(aes(x = eviction_rate)) +
  geom_freqpoly(breaks = seq(0, max(dc_2016_block_group4$eviction_rate), by = 1),
                aes(color = factor(dc_2016_block_group4$maj_white_text))) +
  theme(legend.position = 'none') +
  labs(
    title = "Median income quartile: 100%",
    y = "",
    x = "Eviction rate (percent)",
    color = "Majority race in block group"#,
    #caption = paste("Source: Matthew Desmond's Eviction Lab \n",
     #               "www.evictionlab.org")
  ) 

multiplot(g1, g3, g2, g4, cols = 2)

## Next step:
### 1) clean up grouped charts

