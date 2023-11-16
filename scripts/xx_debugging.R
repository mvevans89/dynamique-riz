

ev_season <- read.csv("results/ev_rice_season.csv")
unique(ev_season$state)
sum(ev_season$state=="inactive")

inact <- filter(ev_season, state =="inactive")


## Nov 8 2023 #######################
#functions
source("scripts/source/summarizing-functions.R")
library(qs)

flood_sum <- read.csv("results/flood_rice_summary.csv")
flood_smooth <- qread("data/smooth/flood_smooth.qs")

long <- filter(flood_sum, dur_day_mean_season1>800)

head(long)

hist(flood_sum$dur_day_mean_season1)

test_id <- long$full_id[1]

test_id <- sample(flood_sum$full_id,1)


# tseries_test <- filter(ev_smooth, full_id == sample(rice_id$full_id,1))
tseries_test <- filter(flood_smooth, full_id == test_id)

# Identify peaks and valleys
tseries_deriv <- deriv_tseries(tseries_test, print_plot = T)
# tseries_deriv


# the output of this is the raw data that we can then aggregate
#' it is by rice field and describes each season
rice_stat_season <- estimate_season_stats(tseries_deriv, print_plot = T)
rice_stat_season

#one value for each stat per rice field
rice_stat_avg <- calc_rice_avg(rice_stat_season)
rice_stat_avg

## Older #####################

map_df(flood_out_list,2) -> test_map

#maybe add something to drop those with errors? but it is dropping a lot of them unnecessarily
# need to drop shoot what is causing the error, something with counting the seasons per year
#not sure which id is throwing the issue though

missing <- rice_id$full_id[!(rice_id$full_id %in% test_map$full_id)]

rlang::last_trace()

all_missing <- filter(flood_smooth, full_id %in%missing) |>
  group_by(full_id) |>
  group_split()


for(i in 100:110) {
  wrap_rice(all_missing[[i]])
  print(paste(all_missing[[i]]$full_id[1], "completed (i=", i, ")"))
}

err_id <- as.data.frame((all_missing[[105]]))

deriv1 <- deriv_tseries(err_id, print_plot = T)
stat1 <- estimate_season_stats(deriv1, print_plot = T)
avg1 <- calc_rice_avg(stat1)

## 2023-11-08 ############

