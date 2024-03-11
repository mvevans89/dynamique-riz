# Create Figures for Methodo Figure
# Projet:
# Authors:

# Michelle V Evans
# Github: mvevans89
# Email: mv.evans.phd@gmail.com

# Script originated March 2024

# Description of script and instructions ###############

#' This script creates figures to put inside of the methodology figure.
#' This includes:
#' 
#' - figures showing the smoothing method
#' - performing numerical derivation
#' - identifying seasons

# Packages and Options ###############################

options(stringsAsFactors = FALSE, scipen = 999)

#reads and writes qs files
library(qs)

#plotting
library(ggplot2); theme_set(theme_bw())
library(patchwork)

#data wrangling
library(tidyr)
library(lubridate)

library(dplyr)

#functions
source("scripts/source/summarizing-functions.R")

# Load Data ################################

smooth_subset <- qread("data/smooth/flood-test_smooth.qs") #smaller df of 1000 rice fields for testing

flood_raw <- read.csv("data/RiceField_data_for_lissage.csv") |>
  filter(full_id %in% unique(smooth_subset$full_id)) |>
  select(full_id, starts_with("sum")) |>
  pivot_longer(starts_with("sum"), names_to = "date", values_to = "perc") |>
  mutate(date = substr(date, 7, 14)) |>
  mutate(date = as.Date(date, format = "%Y%m%d"))

rice_id <- qread("data/rice_id.qs") |>
  filter(full_id %in% unique(smooth_subset$full_id))

# Smoothing Figure #########################
# here we plot the raw data as points and the smoothing as a line

figure_id <- c("w669066456", "w669946863")
# figure_id <- sample(rice_id$full_id,2)

smooth_plot <- filter(smooth_subset, full_id %in% figure_id) |>
  select(date, full_id, smooth) |>
  full_join(filter(flood_raw, full_id %in% figure_id), by = c("full_id", "date")) |>
  mutate(full_id = paste("Ricefield", full_id)) |>
  ggplot(aes(x = date)) +
  geom_point(aes(y = perc), alpha = 0.5, size = 1) +
  geom_line(aes(y = smooth), color = "red") +
  xlab("Date") +
  ylab("Percent Flooded Area") +
  facet_wrap(~full_id, nrow = 2, scales = "free") +
  theme(strip.background = element_blank())

#pour changer, utiliser theme(axis.text = element_text(size = 14))

#it is better to use a script to save figures, this ensures the resolution is 
#always the same
png("figures/lissage.png", res = 300, width = 1200, height = 1000)
smooth_plot
dev.off()

# Derivation Figure #######################
tseries_test <- filter(smooth_subset, full_id == "w669946863")
# tseries_test <- filter(smooth_subset, full_id %in% sample(rice_id$full_id,1))

# Identify peaks and valleys
tseries_deriv <- deriv_tseries(tseries_test, print_plot = F)

deriv_plot <- ggplot(tseries_deriv, aes(x = date, y = perc_flood)) +
  geom_point(aes(color = curve)) +
  geom_point(data = filter(tseries_deriv, curve_location != "inter_season"), size = 2) +
  ylab("Percent Flooded Area") +
  xlab("Date") +
  guides(color = "none")

png("figures/derivative.png", res = 300, width = 1200, height = 600)
deriv_plot
dev.off()


#Identification of Season Figure ##############

# the output of this is the raw data that we can then aggregate
#' it is by rice field and describes each season
rice_stat_season <- estimate_season_stats(tseries_deriv, print_plot = T)
rice_stat_season[[1]]

season_plot <- rice_stat_season[[2]] +
  ylab("Percent Flooded Area") +
  xlab("Date") 

png("figures/seasons.png", res = 300, width = 1200, height = 600)
season_plot
dev.off()

