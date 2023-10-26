# Last updated: Oct 26, 2023

# clear environment
rm(list = ls())

# load packages + set seed
pacman::p_load(tidyverse,lubridate,stargazer)

set.seed(2090820)

ddir_matt <- './rcts/hw2/data/'
ddir <- ddir_matt

# import data

df <- read.csv(paste0(ddir,'brune_et_al_data.csv'))

# Randomization ----

## TA code ----
### complete rand ----
n <- df %>% nrow()

df$rand <- runif(n, 0, 1)
df$rank <- rank(df$rand)

df$treat <- ifelse(df$rank < n / 2, 1, 0)

### strat rand ----
df$rand <- runif(n, 0, 1)

for (village in 1:max(df$village_id)) {
  in_village <- (df$village_id == village)
  str_treat_i <- in_village * (df$rand < median(df$rand[in_village]))
  df$treat_strat <- df$treat_strat + str_treat_i
}
