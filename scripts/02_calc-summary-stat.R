# Calculate summary statistics
# Projet:
# Authors:

# Michelle V Evans
# Github: mvevans89
# Email: mv.evans.phd@gmail.com

# Script originated August 2023

# Description of script and instructions ###############

#' This script calculates the summary statistics at the the scale of the
#' rice field by year and also aggregates it to the scale of the fokontany.
#' 
#' To calculate these statistics, a numerical derivative is applied to each
#' time series. Then the following are calculated:
#' 
#' - the date of local minimums within a year (start and end of seasons) for each year
#' - the number of local minimums within a year (number of seasons) for each year
#' - the amplitude of flooding (difference between minimum and maximum) within a year
#' - whether a year is in use (if the amplitude exceeds 20%)

# Packages and Options ###############################

options(stringsAsFactors = FALSE, scipen = 999)

#reads and writes qs files
library(qs)

#plotting
library(ggplot2); theme_set(theme_bw())
library(patchwork)

library(parallel)

#data wrangling
library(tidyr)
library(lubridate)
library(purrr)

library(dplyr)

#functions
source("scripts/source/summarizing-functions.R")


# Load the Data ####################################

rice_id <- qread("data/rice_id_above400.qs")

test_smooth <- qread("data/smooth/above400/flood-test_smooth.qs") #smaller df of 1000 rice fields for testing

ev_smooth <- qread("data/smooth/above400/ev_smooth.qs")
flood_smooth <- qread("data/smooth/above400/flood_smooth.qs")

# Run tests with functions ###############

# tseries_test <- filter(ev_smooth, full_id == "w685365201")
tseries_test <- filter(ev_smooth, full_id == sample(rice_id$full_id,1))

# Identify peaks and valleys
tseries_deriv <- deriv_tseries(tseries_test, print_plot = T)

# the output of this is the raw data that we can then aggregate
#' it is by rice field and describes each season
rice_stat_season <- estimate_season_stats(tseries_deriv, print_plot = T)
rice_stat_season

#one value for each stat per rice field
rice_stat_avg <- calc_rice_avg(rice_stat_season[[1]])
rice_stat_avg

#within the wrapped function
wrap_rice(filter(flood_smooth, full_id == "w569996661"), return_intermediate = TRUE)

# Map function over full dataset #############################

## Test mapping on subset ###################

#create list of dataframes to map over
test_list <- test_smooth |>
  filter(full_id %in% sample(full_id,100)) |>
  group_by(full_id) |>
  group_split()

system.time({
  
  list_out <- mclapply(test_list, wrap_rice, return_intermediate = TRUE, mc.cores = 8)
  
  #seperate intermediate and final results
  test_out <- list_out |>
    transpose() |>
    map(bind_rows) 
  
  head(test_out[[1]])
  head(test_out[[2]])
  
}) #takes 18 seconds for 100 rice fields on 1 core (23 minutes for all on one core)

#without getting intermediate data
system.time({
  test_out2 <- bind_rows(mclapply(test_list, wrap_rice, mc.cores = 8))
}) #takes 3 seconds for 100 rice fields on 8 cores (6 minutes on 8 cores for all)

## Vegetated Water ####################

ev_list <- ev_smooth |>
  group_by(full_id) |>
  group_split()

# ev_out_test <- lapply(ev_list[100:400], wrap_rice, return_intermediate = TRUE)

ev_out_list <- mclapply(ev_list, wrap_rice, mc.cores = 10, return_intermediate = TRUE)

#seperate intermediate and final results
ev_out_split <- ev_out_list |>
  transpose() |>
  map(bind_rows)

#full detail for each season of ricefield
ev_stat_season <- ev_out_split[[1]]

#bind with metadata
ev_stat_summary <- ev_out_split[[2]] |>
  left_join(rice_id, by = "full_id")

#assess how many had errors
sum(is.na(ev_stat_summary$num_season)) #22

#save as a csv
write.csv(ev_stat_season, "results/above400/ev_rice_season.csv", row.names = FALSE)
write.csv(ev_stat_summary, "results/above400/ev_rice_summary.csv", row.names = FALSE)

## All Water #########################

flood_list <- flood_smooth |>
  group_by(full_id) |>
  group_split()

flood_out_list <- mclapply(flood_list, wrap_rice, mc.cores = 10, return_intermediate = TRUE)

#seperate intermediate and final results
flood_out_split <- flood_out_list |>
  transpose() |>
  map(bind_rows)

#full detail for each season of ricefield
flood_stat_season <- flood_out_split[[1]]

#bind with metadata
flood_stat_summary <- flood_out_split[[2]] |>
  left_join(rice_id, by = "full_id")

sum(is.na(flood_stat_summary$num_season))

#save as a csv
write.csv(flood_stat_season, "results/above400/flood_rice_season.csv", row.names = FALSE)
write.csv(flood_stat_summary, "results/above400/flood_rice_summary.csv", row.names = FALSE)
