# Supplementary Table of Best Fit Smoothing
# Project:
# Authors:

# Michelle V Evans
# Github: mvevans89
# Email: mv.evans.phd@gmail.com

# Script originated August 2024

# Description of script and instructions ###############

#' This script creates the tables of the best fit of each curve-type
#' for the appendix of the manuscript.

## Load Package #############################

options(stringsAsFactors = FALSE, scipen = 999)

#reads and writes qs files
library(qs)


library(dplyr)

# Load Data ############################

ev_rss <- qread("data/smooth/above400/ev_rss.qs")

flood_rss <- qread("data/smooth/above400/flood_rss.qs")

# Create Tables #######################

ev_rss |>
  group_by(full_id) |>
  arrange(rss) |>
  slice(1) |>
  group_by(fit) |>
  summarise(num = n())

flood_rss |>
  group_by(full_id) |>
  arrange(rss) |>
  slice(1) |>
  group_by(fit) |>
  summarise(num = n())
