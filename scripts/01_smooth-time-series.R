# Smoothing Rice Field Radar Data
# Projet:
# Authors:

# Michelle V Evans
# Github: mvevans89
# Email: mv.evans.phd@gmail.com

# Script originated August 2023

# Description of script and instructions ###############

#' This script creates a smoothed time series of the percent of flooded
#' area for each rice field in our dataset.

# Packages and Options ###############################

options(stringsAsFactors = FALSE, scipen = 999)

#plotting
library(ggplot2); theme_set(theme_bw())

library(qs) #better way to save intermediary large files
library(splines)

#data wrangling
library(tidyr)
library(purrr)
library(dplyr)

# Load Data #####################################

#extraire les donnees de zip si ce n'est pas encore fais
unzip("data/RiceField_data_for_lissage.zip", exdir = "data")

rice_data <- read.csv("data/RiceField_data_for_lissage.csv")

# Define Functions ##############################

#' Create smoothed data
#' @param this_rice id of rice field that corresponds to full_id
#' @param rice_df data.frame of rice data. must have columns of full_id, date, and perc
#' @param print_plot whether to print plot of final fit, default = FALSE
#' @param return_details whether to return RSS of fits, default = FALSE
#' @returns data.frame of smoothed % inonde by month. Or if return details is TRUE, a list containing that dataframe and a dataframe of RSS by smoothing model.
get_smooth <- function(this_rice, rice_df, print_plot = FALSE, return_details = FALSE){
  #predict over a date range of every week
  date_range <- seq.Date(range(rice_df$date)[1], range(rice_df$date)[2],
                         by = "1 week")
  date_range_num <- as.numeric(date_range, origin = "1970-01-01")
  
  this_subset <- filter(rice_df, full_id == this_rice) %>%
    #create numeric date
    mutate(date_num = as.numeric(date, origin = "1970-01-01"))
  
  ## Cubic-Spline Fit #############
  #or set the df to 4 per year (one every 3 months)
  num_year <- as.numeric(range(rice_df$date)[2] - range(rice_df$date)[1])/365
  num_df <- floor(num_year*4)
  #identify best smooth based on cv
  fit_cubic <- smooth.spline(this_subset$date_num, this_subset$perc, cv = T,
                             df = num_df, keep.data = TRUE)
  cubic_rss <-  sum((predict(fit_cubic, x = this_subset$date_num)$y - this_subset$perc)^2)
  
  ## Polynomial Fit ################
  fit_poly <- lm(perc~ poly(date_num, num_df/2), data = this_subset)
  poly_rss <- sum((predict(fit_poly, this_subset) - this_subset$perc)^2)
  # plot(this_subset$date_num, predict(fit_poly, this_subset) )
  
  ## Basis Spline Fit ##############
  fit_basis <- lm(perc~ bs(date_num,num_df), data = this_subset)
  basis_rss <- sum((predict(fit_basis, this_subset) - this_subset$perc)^2)
  # plot(this_subset$date_num, predict(fit_basis, this_subset) )
  
  ## Natural Spline Fit ############
  fit_nats <- lm(perc~ ns(date_num,num_df), data = this_subset)
  nats_rss <- sum((predict(fit_nats, this_subset) - this_subset$perc)^2)
  # plot(this_subset$date_num, predict(fit_nats, this_subset) )
  
  ## Select Best Fit and Predict #######
  model_list <- list(fit_cubic, fit_basis, fit_poly, fit_nats)
  best_fit_name <- c("cubic", "basis", "poly", "nats")[which.min(c(cubic_rss, basis_rss, poly_rss, nats_rss))]
  best_fit <- model_list[[which.min(c(cubic_rss, basis_rss, poly_rss, nats_rss))]]
  
  #plot best fit
  best_smooth <- predict(best_fit, data.frame(date_num = date_range_num))
  if(best_fit_name == "cubic"){
    best_smooth <- predict(best_fit, date_range_num)$y
  }
  #create new df with these both
  #predict over full date set, like we do with indicators
  smooth_df <- data.frame(date_num = date_range_num,
                          smooth = best_smooth,
                          model = best_fit_name,
                          full_id = this_rice) |>
    mutate(date = as.Date(date_num, origin = "1970-01-01"))
  if(print_plot){
    #vis to explore
    p1 <- ggplot(smooth_df, aes(x = date)) +
      geom_line(aes(y = smooth), color = "red") +
      geom_point(data = this_subset, aes(y = perc))  +
      ggtitle(paste("riziere", this_rice, ",best smooth:", best_fit_name))
    print(p1)
  }
  
  if(return_details){
    rss_df <- data.frame(fit = c("cubic", "basis", "poly", "nats"),
                         rss = c(cubic_rss, basis_rss, poly_rss, nats_rss))
    return(list(smooth_df, rss_df))
  } else {
    return(smooth_df)
  }
  
}

# Clean and Format Rice Data ###################
#' we want to create a long dataset of vegetated water (EV) and a long dataset of
#' all water (sumELEV)
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

# Smooth #############
#takes about 20 minutes to do each smooth
#test one first
get_smooth(this_rice = unique(flood_long$full_id)[8001],
           rice_df = flood_long,
           print_plot = TRUE)

system.time({
  flood_test <- bind_rows(map(.x = unique(flood_long$full_id)[1:1000], .f = get_smooth,
                               rice_df = flood_long))
})

#should probably parallize this?
ev_smooth <- bind_rows(map(.x = unique(ev_long$full_id), .f = get_smooth,
                           rice_df = ev_long))
flood_smooth <- bind_rows(map(.x = unique(flood_long$full_id), .f = get_smooth,
                              rice_df = flood_long))

#save as qs objects
qsave(rice_id, "data/rice_id.qs")
qsave(flood_test, "data/smooth/flood-test_smooth.qs", nthreads = 4)
qsave(ev_smooth, "data/smooth/ev_smooth.qs", nthreads = 4)
qsave(flood_smooth, "data/smooth/flood_smooth.qs", nthreads = 4)