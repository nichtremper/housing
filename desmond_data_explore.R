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
library(modelr)
library(stringr)
library(ggplot2)
library(cowplot)
library(pushoverr)

output <- ('/Users/nichtremper/Google Drive/data_work/housing/output')

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
  ggplot(aes(x = eviction_rate, y = (..count../sum(..count..)) * 100)) +
    geom_freqpoly(breaks = seq(0, max(dc_2016_block_group1$eviction_rate), by = 1),
                  aes(color = factor(dc_2016_block_group1$maj_white_text))) +
  theme(legend.position = 'right') +
  ylim(0, 50) +
  xlim(0, 15) +
  labs(
    x = "",
    y = "",
    color = 'Race in block group'
  )

### subset the legend
legend <- get_legend(g1)

### now, recast without legend

y.size <- 1
x.size <- 1

g1 <- dc_2016_block_group1 %>%
  ggplot(aes(x = eviction_rate, y = (..count../sum(..count..)) * 100)) +
  geom_freqpoly(breaks = seq(0, max(dc_2016_block_group1$eviction_rate), by = 1),
                aes(color = factor(dc_2016_block_group1$maj_white_text))) +
  background_grid(major = c('x')) +
  theme(legend.position = 'none',
        axis.title.y = element_text(size = rel(y.size), angle = 90)) +
  ylim(0, 50) +
  xlim(0, 15) +
  labs(
    x = "",
    y = "Percent of block groups"
  )

dc_2016_block_group2 <- dc_2016_block_group %>% filter(income_quartile == 2)
g2 <- dc_2016_block_group2 %>%
  ggplot(aes(x = eviction_rate, y = (..count../sum(..count..)) * 100)) +
  geom_freqpoly(breaks = seq(0, max(dc_2016_block_group2$eviction_rate), by = 1),
                aes(color = factor(dc_2016_block_group2$maj_white_text))) +
  background_grid(major = c('x')) +
  theme(legend.position = 'none') + 
  ylim(0, 50) +
  xlim(0, 15) +
  labs(
    x = "",
    y = ""
  )

dc_2016_block_group3 <- dc_2016_block_group %>% filter(income_quartile == 3)
g3 <- dc_2016_block_group3 %>%
  ggplot(aes(x = eviction_rate, y = (..count../sum(..count..)) * 100)) +
  geom_freqpoly(breaks = seq(0, max(dc_2016_block_group3$eviction_rate), by = 1),
                aes(color = factor(dc_2016_block_group3$maj_white_text))) +
  background_grid(major = c('x')) +
  theme(legend.position = 'none',
        axis.title.y = element_text(size = rel(y.size), angle = 90),
        axis.title.x = element_text(size = rel(x.size), angle = 0)) +
  ylim(0, 50) +
  xlim(0, 15) +
  labs(
    x = "Eviction rate",
    y = "Percent of block groups"
  ) 

dc_2016_block_group4 <- dc_2016_block_group %>% filter(income_quartile == 4)
g4 <- dc_2016_block_group4 %>%
  ggplot(aes(x = eviction_rate, y = (..count../sum(..count..)) * 100)) +
  geom_freqpoly(breaks = seq(0, max(dc_2016_block_group4$eviction_rate), by = 1),
                aes(color = factor(dc_2016_block_group4$maj_white_text))) +
  background_grid(major = c('x')) +
  theme(legend.position = 'none',
        axis.title.x = element_text(size = rel(x.size), angle = 0)) +
  ylim(0, 50) +
  xlim(0, 15) +
  labs(
    x = "Eviction rate",
    y = ""
  ) 

# combine the graphs

title <- ggdraw() +
  draw_label("Eviction rate in block group by median household income quartile:
             Washington, DC 2016", fontface = 'bold')

note <- ggdraw() +
  draw_label("Data source: www.evictionlab.org", 
             y = .1, x = 0.25,  size = 12)

(graphs <- plot_grid(g1, g2, g3, g4, legend, note,
                    labels = c('First quartile', 'Second quartile',
                               'Third quartile', 'Fourth quartile'),
                    label_fontface = "plain",
                    nrow = 3, label_size = 10, align = 'h',
                    axis = 1, hjust = -1))

plot_grid(
  title, graphs, 
  nrow = 2, 
  rel_heights = c(.1, 1))

ggsave('eviction_rate_DC.png', plot = ggplot2::last_plot(), 
       width = 10, height = 10, units = c("in"), dpi = 400)

# Modeling ---------------------------------------------------------------

dc_2016_block_group <- dc_2016_block_group %>%
  mutate(maj_white = ifelse(pct_white >= 50, 1, 0))
 
# first, look at just the relationship between eviction rate and rent
## scatter plot
dc_2016_block_group %>%
  ggplot(aes(x = median_gross_rent, y = eviction_rate)) +
    geom_point()

## linear model
mod_1 <- lm(eviction_rate ~ median_gross_rent, data = dc_2016_block_group)

## vizualize the predictions
### create grid
grid <- dc_2016_block_group %>%
  data_grid(median_gross_rent)
grid

### now add predictions of eviction rate from mod_1
grid <- grid %>%
  add_predictions(mod_1)
grid

### visualize predictions
ggplot(dc_2016_block_group, aes(x = median_gross_rent)) +
  geom_point(aes(y = eviction_rate, color = maj_white)) +
  geom_line(aes(y = pred), data = grid, color = "red")

### now look at residuals
dc_2016_block_group <- dc_2016_block_group %>%
  add_residuals(mod_1) %>%
  rename(resid1 = resid)

### look at the spread of residuals
ggplot(dc_2016_block_group, aes(x = resid1)) +
  geom_freqpoly(binwidth = 1)

### plot residuals and median rent 
ggplot(dc_2016_block_group, aes(x = median_gross_rent, y = resid1)) +
  geom_ref_line(h = 0) +
  geom_point()

## linear model, controlling for maj white
mod_2 <- lm(eviction_rate ~ median_gross_rent + maj_white,
            data = dc_2016_block_group)

dc_2016_block_group <- dc_2016_block_group %>%
  add_residuals(mod_2) %>%
  rename(resid2 = resid)

### plot residuals and median rent 
ggplot(dc_2016_block_group, aes(x = median_gross_rent, y = resid2)) +
  geom_ref_line(h = 0) +
  geom_point()

### visualize
ggplot(dc_2016_block_group, aes(x = median_gross_rent)) +
  geom_point(aes(y = eviction_rate, color = maj_white)) +
  geom_line(aes(y = pred), data = grid2, color = "red")
