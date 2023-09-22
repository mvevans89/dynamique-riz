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

library(dplyr)

# Define the Functions ###############################

#' calculate central numeric derivative from a vector
#' @param vec vector of timeseries to calculate derivative on
c_num_deriv <- function(vec){
  new_vec <- rep(NA, length(vec))
  for(i in 2:(length(new_vec)-1)){
    new_vec[i] <- (vec[i+1] - vec[i-1])/2
  }
  return(new_vec)
}
#' Estimate derivation of timeseries of flooding
#' @param tseries data.frame of timeseries with columns date_num, smooth (smooth flooded percentage), and full_id in any order
#' @param print_plot whether to print the plot outputs. Default = FALSE
#' @returns a dataframe of the location of minimum, maximums, and inter-season by date

deriv_tseries <- function(tseries, print_plot = FALSE){
  
  this_full_id <- unique(tseries$full_id)
  full_x <- seq(from = min(tseries$date_num), to = max(tseries$date_num), by = 1)
  full_smooth <- approx(tseries$date_num, tseries$smooth, xout = full_x)$y
  #interpolate to days so we can find exactly when it crosses zero
  deriv_out <- data.frame(date_num = full_x,
                            deriv_val = c_num_deriv(full_smooth)) |>
    na.omit() 
  
  deriv_out$deriv2_val <- c_num_deriv(deriv_out$deriv_val)
  
  deriv_day <- data.frame(date_num = full_x,
                          smooth = full_smooth) |>
    left_join(deriv_out) |>
    #identify as when it actually crosses zero
    mutate(sign_class = sign(deriv_val)) |>
    mutate(sign_diff = c(NA, diff(sign_class))) |>
    mutate(deriv_class = ifelse(abs(sign_diff) >0 ,"zero", NA)) |>
    #identify whether it is a sink or a hill (convex vs concave) using f"
    mutate(curve = case_when(
      abs(deriv2_val)<0.000005 ~ as.character(NA),
      deriv2_val>0 ~ "sink",
      deriv2_val<0 ~ "hill"
    )) |>
    fill(curve, .direction = "down") |>
    mutate(date = as.Date(date_num, origin = as.Date("1970-01-01")))
  
  if(print_plot){
  p_smooth <- deriv_day |>
    ggplot(aes(x = date)) +
    geom_point(aes(y = smooth, color = curve)) +
    geom_point(aes(y = smooth, shape = deriv_class)) +
    ylab("Percent Flooded") +
    ggtitle(paste("Rice field", this_full_id)) +
    guides(shape = 'none', color = 'none')
  
  p_deriv <- deriv_day |>
    ggplot(aes(x = date)) +
    geom_line(aes(y = deriv_val)) +
    geom_point(aes(y = deriv_val, shape = deriv_class), size = 2) +
    ylab("Change in flooding\n(numerical derivative)") +
    guides(shape = 'none')
  
  print(p_smooth / p_deriv)
  }
  
  #identify if the point is a minimum or maximum
  season_pts <- deriv_day |>
    #drop first and last month for this calculation
    filter(date>=(min(date) %m+% months(1))) |>
    filter(date<=(max(date) %m-% months(1))) |>
    #if sign_diff is pos, this is a minimum. If it is negative, it is a maximum
    #basically the same as the second derivative we calculate above
    mutate(curve_location = case_when(
      sign_diff<0 ~ "max_season",
      sign_diff>0 ~ "min_season",
      TRUE ~ "inter_season"
    )) |>
    mutate(full_id = this_full_id) |>
    select(full_id, date, perc_flood = smooth, curve_location, curve)
  
  return(season_pts)
}
#' Estimate season stats
#' This function identifies the seasons of a rice field and calculates 
#' statistics by season.
#' @param tseries_deriv data.frame of time series with derivative info. output of deriv_tseries function
#' @returns dataframe of statistics by rice season
estimate_season_stats <- function(tseries_deriv, print_plot = FALSE){

  #' Steps
  #' 1. Identify amplitude between each min and maximum (stationary point via f')
  #' 2. Calculate threshold of 10% of maximum amplitude
  #' 3. If amplitude change is less than that, the point is dropped as a min or max
  #' 4. Group points into seasons based on two minimums with a maximum between that is above 
  #'    the threshold of amplitude difference. Points that are only local minimums or maximums
  #'    are grouped so that they fall within a season and do not start a new one.
  #' 5. Inactive seasons are characterized as those without a maximum more than the threshold,
  #'    or two minimums next to each other
  #' 5. Estimate values for each season
  
  #find first minimum and drop everything before that
  first_min_date <- tseries_deriv$date[min(which(tseries_deriv$curve_location == "min_season"))]
  
  stat_times <- tseries_deriv |>
    filter(date>=first_min_date) |>
    filter(curve_location != "inter_season") |>
    mutate(amp = diff(c(0, perc_flood))) |>
    mutate(abs_amp = abs(amp)) |>
    mutate(curve_location_correct = NA) 
  
  stat_times_orig <- stat_times
  
  #save first one
  stat_times$curve_location_correct[1] <- stat_times$curve_location[1]
           
  this_amp_threshold <- 0.1*max(stat_times$abs_amp)
  
  #' function to correct for small anomalies
  #' @param stat_times dataframe of stat_times from above
  #' @param amp_threshold value of amplitude threshold
  #' @return new datafrom of stat times with some rows removed to clean
  correct_amp <- function(this_stat_times, amp_threshold = this_amp_threshold){
    for(i in 1:nrow(this_stat_times)){
      #skip if already corrected
      if(!(is.na(this_stat_times$curve_location_correct[i]))) next
      #if below threshold (just a wiggle), drop
      if((this_stat_times$abs_amp[i] < this_amp_threshold)){
          this_stat_times <- this_stat_times[-i,]
          break
      }  else
          this_stat_times$curve_location_correct[i] <- this_stat_times$curve_location[i]
      } 
    
    #recalculate thresholds
    this_stat_new <- this_stat_times |>
      mutate(amp = diff(c(0, perc_flood))) |>
      mutate(abs_amp = abs(amp))
    
    return(this_stat_new)
  }
  
  #iterate through to adjust for low amplitude
  num_NA <- sum(is.na(stat_times$curve_location_correct))
  #use a while to clear out the errors
  while(num_NA>0){
    stat_times <- correct_amp(this_stat_times = stat_times, amp_threshold = this_amp_threshold)
    num_NA <- sum(is.na(stat_times$curve_location_correct))
  }
  
  #identify change in seasons from corrected data
  stat_times2 <- select(stat_times_orig, -curve_location_correct) |>
    left_join(select(stat_times, full_id, date, curve_location_correct))
  #replace correct location NAs to help with logicals
  stat_times2$curve_location_correct[is.na(stat_times2$curve_location_correct)] <- "inter"
  
  #identify inactive seasons by creating two minimums next to each other
  last_min_position <- min(which(stat_times2$curve == "sink"))
  current_min <- stat_times2$perc_flood[last_min_position]
  
  for(i in 2:nrow(stat_times2)){
    if(stat_times2$curve_location_correct[i] == "inter") next
    
    if(stat_times2$curve_location_correct[i] == "min_season"){
      current_min <- stat_times2$perc_flood[i]
      last_min_position <- i
    }
    
    if(stat_times2$curve_location_correct[i] == "max_season"){
      if(stat_times2$curve_location_correct[i-1] == "min_season") {
        next
      } else {
        #identify closest minimum within the amp threshold from prior minimum
        #this helps remove inactive minimum periods
        min_threshold <- c(current_min- this_amp_threshold, current_min+this_amp_threshold)
        #go through each prior point and see if it is a minimum close enough to the prior minimum
        for(j in (i-1):last_min_position){
          if(stat_times2$curve[j] == "hill") next
          if(between(stat_times2$perc_flood[j], min_threshold[1], min_threshold[2])){
            stat_times2$curve_location_correct[j] <- "min_season"
            break
          } else next
        }
      }
      
    }
  }
  
  #go through and double-check for small wiggles that have been created in the max
  fake_max_date <- filter(stat_times2, curve_location_correct != "inter") |>
    arrange(date) |>
    mutate(new_abs_amp = abs(diff(c(perc_flood, 0)))) |>
    filter(new_abs_amp<this_amp_threshold, curve_location_correct == "max_season") |>
    pull(date)
  
  stat_times2$curve_location_correct[stat_times2$date %in% fake_max_date] <- "inter"
  
  stat_times2$season_num <- 0
  stat_times2$inactive <- 0
  #identify start of first season
  #identify first min_season to start counting
  first_min <- min(which(stat_times2$curve == "sink"))
  stat_times2$season_num[first_min] = 1
  #skip first and last points
  for(i in (first_min+1):(nrow(stat_times2)-1)){
    if(stat_times2$curve_location_correct[i] == "inter"){
      stat_times2$season_num[i] <- stat_times2$season_num[i-1]
    }
    if(stat_times2$curve_location_correct[i] == "min_season"){
      stat_times2$season_num[i] <- stat_times2$season_num[i-1] + 1
    } else
      {
      stat_times2$season_num[i] <- stat_times2$season_num[i-1]
      }
  }
  
  #identify inactive seasons as those with no maximum and a small difference between minimums
  stat_times2 <- stat_times2 |>
    group_by(season_num) |>
    mutate(inactive = case_when(
      "max_season" %in% curve_location_correct ~ 0,
      TRUE ~ 1
    )) |> 
    ungroup() 
  
  #adjust those that are the descent of a peak but getting caught as inactive
  false_inactive <- stat_times2 |>
    filter(curve_location_correct == "min_season") |>
    mutate(min_diff = abs(diff(c(perc_flood,0)))) |> 
    filter(min_diff > this_amp_threshold & inactive == 1) |> 
    pull(season_num)
  
  #set as active and assign season number to match peak
  stat_times2$inactive[stat_times2$season_num %in% false_inactive] <- 0
  stat_times2$season_num[stat_times2$season_num %in% false_inactive] <- stat_times2$season_num[stat_times2$season_num %in% false_inactive] - 1
  
  #get statistics for each season
  #join with full date dataframe and then fill downwards for each season, then group by and calculate by season_num
  tseries_stat <- select(stat_times2, full_id, date, amp, abs_amp, curve_location_correct, season_num, inactive) |>
    right_join(tseries_deriv, by = c("full_id", "date")) |>
    arrange(date) |>
    #fill downwards
    tidyr::fill(matches("season_num"), .direction = "down") |>
    tidyr::fill(matches("inactive"), .direction = "down") |>
    #drop before first season
    filter(!(is.na(season_num))) |>
    #get statistics by season
    group_by(full_id, season_num) |>
    summarise(start_date = min(date),
              end_date = max(date),
              min_flood = min(perc_flood),
              max_flood = max(perc_flood),
              peak_date = date[perc_flood==max(perc_flood)],
              state = mean(inactive)) |>
    ungroup() |>
    mutate(state = ifelse(state>0, "inactive", "active")) |>
    mutate(season_dur = end_date - start_date) |>
    #drop last season becuase it usually doesn't have an end
    filter(season_num<max(season_num)) |>
    #also drop season 0 becuase that also isn't actually a season
    filter(season_num>0)
  
  if(print_plot){
    rainbow_pal <- sample(rainbow(20), max(stat_times2$season_num))
    
    p1 <- select(stat_times2, full_id, date, amp, abs_amp, curve_location_correct, season_num, inactive) |>
      right_join(tseries_deriv, by = c("full_id", "date")) |>
      arrange(date) |>
      #fill downwards
      tidyr::fill(matches("season_num"), .direction = "down") |>
      tidyr::fill(matches("inactive"), .direction = "down") |>
      #drop before first season
      filter(!(is.na(season_num))) |>
      filter(season_num>0) |>
      # filter(inactive == 0) |>
      ggplot(aes(x = date, y =perc_flood)) +
      geom_line(aes(linetype = as.factor(inactive), color = as.factor(season_num))) +
      geom_point(data = filter(stat_times2, curve_location_correct != "inter")) +
      scale_color_manual(values = rainbow_pal) +
      guides(color = 'none', linetype = 'none')
      
    print(p1)
  }
  
  return(tseries_stat)
  
}

#' Get the average of stats per rice field
#' @param tseries_stat data.frame of rice dynamics by season, output of estimate_season_stats
#' @returns a one-row data.frame of 
#' number of seasons per year (number of peaks)
#' start of rice season (first start of season in a year)
#' end of rice season (last end of a season in a year)
#' average duration of season
#' amount of time inactive
calc_rice_avg <- function(tseries_stat){
  
  #' number of seasons per year (number of peaks)
  #' average duration of season
  #' start of rice season (first start of season in a year)
  #' end of rice season (last end of a season in a year)
  #' peak of biggest season (date of peak of largest flooding)
  #' amplitude (difference between max and min flood)
  #' amount of time inactive
  
  rice_avg_df <- tseries_stat |>
    #only include 2017-2021 to make sure we get full seasons
    filter(start_date >= as.Date("2017-01-01"),
           end_date <= as.Date("2021-12-31")) |>
    mutate(year_start = year(start_date)) |>
    mutate(year_peak = year(peak_date)) 
  
  num_season <- rice_avg_df |>
    filter(state == "active") |>
    group_by(year_peak) |>
    summarise(num_season = n()) |>
    ungroup() |>
    summarise(avg_peak = mean(num_season, na.rm = T)) |>
    pull(avg_peak)
  
  avg_duration <- rice_avg_df |>
    filter(state == "active") |>
    summarise(avg_dur = as.numeric(mean(season_dur, na.rm = T))) |>
    pull(avg_dur)
  
  avg_start <- rice_avg_df |>
    filter(state == "active") |>
    #only get first in each year
    group_by(year_start) |>
    arrange(start_date) |>
    slice(1)|>
    ungroup() |>
    summarise(avg_start = mean(yday(start_date), na.rm = T)) |>
    pull(avg_start)
  
  avg_end <-  rice_avg_df |>
    filter(state == "active") |>
    mutate(year_end = year(end_date)) |>
    #only get last in each year
    group_by(year_end) |>
    arrange(desc(end_date)) |>
    slice(1)|>
    ungroup() |>
    summarise(avg_end = mean(yday(end_date), na.rm = T)) |>
    pull(avg_end)
  
  avg_peak <- rice_avg_df |>
    filter(state == "active") |>
    mutate(year_peak = year(peak_date)) |>
    group_by(year_peak) |>
    #only use biggest peak of the season
    arrange(max_flood) |>
    slice(1) |>
    ungroup() |>
    summarise(avg_peak = mean(yday(peak_date), na.rm = T)) |>
    pull(avg_peak)
  
  avg_amp <- rice_avg_df |>
    filter(state == "active") |>
    mutate(year_peak = year(peak_date)) |>
    rowwise() |>
    mutate(amp = max_flood - min_flood) |>
    group_by(year_peak) |>
    summarise(avg_amp = mean(amp, na.rm = T)) |>
    ungroup() |>
    summarise(avg_amp = mean(avg_amp, na.rm = T)) |>
    pull(avg_amp)
  
  prop_inactive <- rice_avg_df |>
    summarise(prop_inactive = weighted.mean(state=="inactive", w = as.numeric(season_dur), na.rm = T)) |>
    pull(prop_inactive)
  
  # to help with errors due to differing lengths
  if(length(num_season)<1) num_season <- NA
  if(length(avg_start)<1) avg_start <- NA
  if(length(avg_end)<1) avg_end <- NA
  if(length(avg_duration)<1) avg_duration<- NA
  if(length(avg_peak)<1) avg_peak <- NA
  if(length(prop_inactive)<1) prop_inactive<- NA
  
  
  out <- data.frame(full_id = unique(tseries_stat$full_id),
                    num_season = num_season,
                    avg_start = avg_start,
                    avg_end = avg_end,
                    avg_duration = avg_duration,
                    avg_peak = avg_peak,
                    avg_amp = avg_amp,
                    prop_inactive = prop_inactive)
  
  return(out)
  
}

#' Function for the full run of analysis
#' Should be mapped over data.frames of smoothed time-series
#' @param ind_tseries an individual time series for a rice field
#' @rerturn output of calc_rice_avg (one-row of rice field stats)
wrap_rice <- function(ind_tseries){
  suppressMessages({
  deriv1 <- deriv_tseries(ind_tseries, print_plot = FALSE)
  season_stat <- estimate_season_stats(deriv1, print_plot = FALSE)
  stat_avg <- calc_rice_avg(season_stat)
  })
  
  return(stat_avg)
  
}


# Load the Data ####################################

rice_id <- qread("rice_id.qs")

test_smooth <- qread("flood-test_smooth.qs") #smaller df of 1000 rice fields for testing

ev_smooth <- qread("ev_smooth.qs")
flood_smooth <- qread("flood_smooth.qs")

# Run tests with functions ###############

# tseries_test <- filter(ev_smooth, full_id == "w689278817")
tseries_test <- filter(ev_smooth, full_id == sample(rice_id$full_id,1))

# Identify peaks and valleys
tseries_deriv <- deriv_tseries(tseries_test, print_plot = T)

# the output of this is the raw data that we can then aggregate
#' it is by rice field and describes each season
rice_stat_season <- estimate_season_stats(tseries_deriv, print_plot = T)
rice_stat_season

#one value for each stat per rice field
rice_stat_avg <- calc_rice_avg(rice_stat_season)
rice_stat_avg

#within the wrapped function
wrap_rice(filter(ev_smooth, full_id == "r14085422"))

# Map function over full dataset #############################

#create list of dataframes to map over
test_list <- test_smooth |>
  filter(full_id %in% sample(full_id,100)) |>
  group_by(full_id) |>
  group_split()

system.time({
  
  test_out <- bind_rows(lapply(test_list, wrap_rice))
}) #takes 18 seconds for 100 rice fields on 1 core (23 minutes for all on one core)


system.time({
  
  test_out <- bind_rows(mclapply(test_list, wrap_rice, mc.cores = 8))
}) #takes 3 seconds for 100 rice fields on 8 cores (6 minutes on 8 cores for all)

## Vegetated Water ####################

ev_list <- ev_smooth |>
  group_by(full_id) |>
  group_split()

ev_out_list <- mclapply(ev_list, wrap_rice, mc.cores = 10)

ev_out <- bind_rows(ev_out_list)
#bind with metadata
ev_out <- left_join(ev_out, rice_id, by = "full_id")

#save as a csv
write.csv(ev_out, "results/ev_rice_summary.csv")

## Free Water #########################

flood_list <- flood_smooth |>
  group_by(full_id) |>
  group_split()

flood_out <- bind_rows(mclapply(flood_list, wrap_rice, mc.cores = 10))

#bind with metadata
flood_out <- left_join(flood_out, rice_id, by = "full_id")

#save as a csv
write.csv(flood_out, "results/flood_rice_summary.csv")

# Fokontany Level Statistics ####################################

#' Here is just an example of how we could calculate these in R.

library(sf)
flood_out <- read.csv("results/flood_rice_summary.csv")
fkt_poly <- st_read("ifd_fokontany.gpkg") |>
  mutate(comm_fkt = paste(new_commune, fokontany, sep = "_"))

flood_out |>
  group_by(comm_fkt) |>
  summarise(mean_duration = mean(avg_duration, na.rm = T)) |>
  ungroup() |>
  left_join(fkt_poly) |>
  st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = mean_duration))

flood_out |>
  group_by(comm_fkt) |>
  summarise(mean_start = mean(avg_start, na.rm = T)) |>
  ungroup() |>
  left_join(fkt_poly) |>
  st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = mean_start))

#' proportion of rice fields with multiple seasons during the 
#' time period at least once
flood_out |>
  group_by(comm_fkt) |>
  summarise(prop_2season = mean(num_season>1, na.rm = T)) |>
  ungroup() |>
  left_join(fkt_poly) |>
  st_as_sf() |>
  ggplot() +
  geom_sf(aes(fill = prop_2season))
  