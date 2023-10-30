# Export data for malaria model
# Projet:
# Authors:

# Michelle V Evans
# Github: mvevans89
# Email: mv.evans.phd@gmail.com

# Script originated Oct 2023

# Description of script and instructions ###############

#' This script cleans, aggregates, and saves the data on rice field flooding
#' for use in the malaria model in another repo.

# Packages and Options ###############################

options(stringsAsFactors = FALSE, scipen = 999)

#plotting
library(ggplot2); theme_set(theme_bw())

library(qs)

#data wrangling
library(tidyr)
library(lubridate)

library(dplyr)

# Load Data ###########################################

rice_data <- read.csv("data/RiceField_data_for_lissage.csv")

rice_id <- select(rice_data, full_id, perc_max, osm_id, rcf_superficie, comm_fkt) |>
  distinct()

ev_long <- select(rice_data, full_id, starts_with("EV")) |>
  pivot_longer(starts_with("EV"), names_to = "date", values_to = "perc") |>
  mutate(date = substr(date, 3, 10)) |>
  mutate(date = as.Date(date, format = "%Y%m%d"))

flood_long <- select(rice_data, full_id, starts_with("sum")) |>
  pivot_longer(starts_with("sum"), names_to = "date", values_to = "perc") |>
  mutate(date = substr(date, 7, 14)) |>
  mutate(date = as.Date(date, format = "%Y%m%d"))

# Get values by fokontany #########################
#' Rather than smoothing each rice field, I want to exact at the level of
#' fokontany and see whether the data at this aggregation is still so noisy it
#' needs to be completely smoothed or if we can just smooth outliers

ev_fkt <- ev_long |>
  left_join(rice_id, by = "full_id") |>
  group_by(date, comm_fkt) |>
  summarise(prop_ev = weighted.mean(perc, w = rcf_superficie)/100) |>
  ungroup()

flood_fkt <- flood_long |>
  left_join(rice_id, by = "full_id") |>
  group_by(date, comm_fkt) |>
  summarise(prop_flood = weighted.mean(perc, w = rcf_superficie)/100) |>
  ungroup()

#visually inspect to get an idea of smoothness
ev_fkt |>
  filter(comm_fkt %in% sample(comm_fkt, 8)) |>
  ggplot(aes(x = date, y = prop_ev)) +
  geom_line() +
  facet_wrap(~comm_fkt)

flood_fkt |>
  filter(comm_fkt %in% sample(comm_fkt, 8)) |>
  ggplot(aes(x = date, y = prop_flood)) +
  geom_line() +
  facet_wrap(~comm_fkt)

flood_fkt |>
  filter(comm_fkt %in% sample(comm_fkt, 8)) |>
  mutate(month = month(date),
         year = year(date)) |>
  group_by(comm_fkt, year, month) |>
  summarise(prop_flood = mean(prop_flood)) |>
  ungroup() |>
  mutate(date = as.Date(paste(year, month,  "01", sep = "-"))) |>
  ggplot(aes(x = date, y = prop_flood)) +
  geom_line() +
  facet_wrap(~comm_fkt)

## Compare to using smoothed time series ####################

flood_smooth <- qread("data/smooth/flood_smooth.qs")

flood_fkt_smooth <- flood_smooth |>
  left_join(rice_id, by = "full_id") |>
  group_by(date, comm_fkt) |>
  summarise(prop_flood = weighted.mean(smooth, w = rcf_superficie)/100) |>
  ungroup()

flood_fkt_smooth |>
  filter(comm_fkt %in% sample(comm_fkt, 8)) |>
  ggplot(aes(x = date, y = prop_flood)) +
  geom_line() +
  facet_wrap(~comm_fkt)

flood_fkt_smooth |>
  filter(comm_fkt %in% sample(comm_fkt, 6)) |>
  mutate(month = month(date),
         year = year(date)) |>
  group_by(comm_fkt, year, month) |>
  summarise(prop_flood = mean(prop_flood)) |>
  ungroup() |>
  mutate(date = as.Date(paste(year, month,  "01", sep = "-"))) |>
  ggplot(aes(x = date, y = prop_flood)) +
  geom_line() +
  facet_wrap(~comm_fkt)

#' this is much smoother

flood_fkt_smooth_month <- flood_fkt_smooth |>
  mutate(month = month(date),
         year = year(date)) |>
  group_by(comm_fkt, year, month) |>
  summarise(prop_flood = mean(prop_flood)) |>
  ungroup() |>
  mutate(date = as.Date(paste(year, month,  "01", sep = "-"))) 

# Save for export ##############################

write.csv(flood_fkt_smooth_month, "results/model-flood.csv", row.names = FALSE)
