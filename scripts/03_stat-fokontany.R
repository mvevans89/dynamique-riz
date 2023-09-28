# Example of Calculating Fokontany-Level Stats
# Projet:
# Authors:

# Michelle V Evans
# Github: mvevans89
# Email: mv.evans.phd@gmail.com

# Script originated Sep 2023

# Description of script and instructions ###############

#' This script is an example of how to calculate fokontany level
#' statistics from the rice field values.

# Packages and Options ###############################

options(stringsAsFactors = FALSE, scipen = 999)

#plotting
library(ggplot2); theme_set(theme_bw())
library(sf)

#data wrangling
library(tidyr)

library(dplyr)

# Load Data ##############

flood_out <- read.csv("results/ev_rice_summary.csv")
fkt_poly <- st_read("data/ifd_fokontany.gpkg") |>
  mutate(comm_fkt = paste(new_commune, fokontany, sep = "_"))

# Create Plots ##################

flood_out |>
  group_by(comm_fkt) |>
  summarise(mean_duration = mean(dur_day_mean_season1, na.rm = T)) |>
  ungroup() |>
  left_join(fkt_poly) |>
  st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = mean_duration)) +
  ggtitle("Durée Moyenne de Premier Saison")

flood_out |>
  group_by(comm_fkt) |>
  summarise(mean_start = mean(start_month_mode_season1, na.rm = T)) |>
  ungroup() |>
  left_join(fkt_poly) |>
  st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = mean_start)) +
  ggtitle("Moyenne mois de commencement de premier saison")

flood_out |>
  group_by(comm_fkt) |>
  summarise(mean_start = mean(start_month_mode_season2, na.rm = T)) |>
  ungroup() |>
  left_join(fkt_poly) |>
  st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = mean_start)) +
  ggtitle("Moyenne mois de commencement\nde deuxieme saison")

#' proportion of rice fields with 2 seasons at least once
flood_out |>
  group_by(comm_fkt) |>
  summarise(prop_2season = mean(year_2season>1, na.rm = T)) |>
  ungroup() |>
  left_join(fkt_poly) |>
  st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = prop_2season))
