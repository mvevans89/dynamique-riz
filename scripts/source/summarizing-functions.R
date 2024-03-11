
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

#' function to correct for small anomalies
#' @param stat_times dataframe of stat_times from above
#' @param amp_threshold value of amplitude threshold
#' @return new datafrom of stat times with some rows removed to clean
correct_amp <- function(this_stat_times, amp_threshold){
  for(i in 1:nrow(this_stat_times)){
    #skip if already corrected
    if(!(is.na(this_stat_times$curve_location_correct[i]))) next
    #if below threshold (just a wiggle), drop
    if((this_stat_times$abs_amp[i] < amp_threshold)){
      this_stat_times <- this_stat_times[-i,]
      break
    }  else
      this_stat_times$curve_location_correct[i] <- this_stat_times$curve_location[i]
  } 
  
  #recalculate thresholds now that this point is dropped
  this_stat_new <- this_stat_times |>
    mutate(amp = diff(c(0, perc_flood))) |>
    mutate(abs_amp = abs(amp))
  
  return(this_stat_new)
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
  
  #threshold is 10% of maximum amplitude or 5, whichever is higher         
  this_amp_threshold <- 0.1*max(stat_times$abs_amp)
  
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
  #also must be less than 3 months from the next point
  first_date <- stat_times2$date[1]
  false_inactive <- stat_times2 |>
    filter(curve_location_correct == "min_season") |>
    mutate(min_diff = abs(diff(c(perc_flood,0)))) |> 
    mutate(date_diff = abs(diff(c(first_date, date)))) |>
    filter(min_diff > this_amp_threshold & inactive == 1 & date_diff < 90) |>  
    pull(season_num)
  
  #set as active and assign season number to match peak
  stat_times2$inactive[stat_times2$season_num %in% false_inactive] <- 0
  stat_times2$season_num[stat_times2$season_num %in% false_inactive] <- stat_times2$season_num[stat_times2$season_num %in% false_inactive] - 1
  
  #if there are two minimums without a maximum between them and they are more 
  #than three months apart, that becomes inactive
  last_date <- tail(stat_times2$date[stat_times2$curve_location_correct!="inter"],1)
  min_distance_season <- stat_times2 |>
    filter(curve_location_correct != "inter") |>
    mutate(date_diff = abs(diff(c(date, last_date)))) |>
    mutate(next_curve = lead(curve_location_correct)) |>
    mutate(next_curve = ifelse(is.na(next_curve), "min_season", next_curve)) |>
    mutate(new_inactive = case_when(
      (curve_location_correct == next_curve) & (date_diff > 90) & curve_location_correct == "min_season" ~ 1,
      TRUE ~ 0
    )) |> 
    filter(new_inactive == 1) |>
    pull(season_num)
  
  stat_times2$inactive[stat_times2$season_num %in% min_distance_season] <- 1
  
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
  
  if(print_plot){
    return(list(tseries_stat, p1))
  }
  
  return(tseries_stat)
  
}

#' Function to return agricultural year
#' Agricultural year is defined as that starting in July (i.e. July 2016 - June 2017 is considered rice2016)
get_ag_year <- function(input_date){
  true_year <- lubridate::year(input_date)
  second_year <- lubridate::month(input_date)<=6
  true_year[second_year] <- true_year[second_year] - 1
  return(paste0("rice", true_year))
}

#' Get the average of stats per rice field
#' @param tseries_stat data.frame of rice dynamics by season, output of estimate_season_stats
#' @returns a one-row data.frame of 
#' number of seasons total
#' number of years with more than 1 season
#' number of inactive seasons over the full time period
#' mode of start of rice season 1 (first start of season in a year)
#' mode of end of rice season 1 (first end of a season in a year)
#' mode of peak date of rice season 1
#' mean duration of rice season 1
#' median duration of rice season 1
#' mean amplitude of rice season 1
#' mode of start of rice season 2 (second start of season in a year) [NA if only one season]
#' mode of end of rice season 2 (second end of a season in a year) [NA if only one season]
#' mode of peak date of rice season 2 [NA if only one season]
#' mean duration of rice season 2 [NA if only one season]
#' median duration of rice season 2 [NA if only one season]
#' mean amplitude of rice season 2 [NA if only one season]

calc_rice_avg <- function(tseries_stat){
  
  rice_avg_df <- tseries_stat |>
    #only include July 2016 -June 2022 to make sure we get full seasons
    filter(start_date >= as.Date("2016-07-01"),
           end_date <= as.Date("2022-06-30")) |>
    mutate(year_start = get_ag_year(start_date)) |>
    mutate(year_peak = get_ag_year(peak_date)) |>
    #add months for calculation
    mutate(month_start = month(start_date),
           month_end = month(end_date),
           month_peak = month(peak_date))
  
  num_inactive <- rice_avg_df |>
    summarise(num_inactive = sum(state == "inactive"))
  
  rice_avg_df <- rice_avg_df |>
    filter(state != "inactive")
  
  #some rice fields are inactive the whole time, drop these
  if(nrow(rice_avg_df)<1){
    out <- data.frame(full_id = unique(tseries_stat$full_id)
                      # num_season = as.numeric(NA),
                      # year_2season = as.integer(NA),
                      # num_inactive = as.integer(NA),
                      # start_month_mode_season1 = as.numeric(NA),
                      # end_month_mode_season1 = as.numeric(NA),
                      # peak_month_mode_season1 = as.numeric(NA),
                      # dur_day_mean_season1 = as.numeric(NA),
                      # dur_day_median_season1 = as.numeric(NA),
                      # amp_mean_season1 = as.numeric(NA)
    )
    return(out)
  }
  
  num_season <- rice_avg_df |>
    group_by(full_id) |>
    summarise(num_season = max(season_num))
  
  year_2season <- rice_avg_df |>
    group_by(year_peak) |>
    summarise(num_season = n()) |>
    ungroup() |>
    summarise(year_2season = sum(num_season>1)) 
  
  #create mode function
  calc_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  stat_by_season <- rice_avg_df |>
    mutate(amp = max_flood-min_flood) |>
    #identify if season is first or second
    group_by(year_peak) |>
    mutate(season_order = paste0("season", 1:n())) |> 
    #drop any not corresponding to 1 or 2
    filter(season_order %in% c("season1", "season2")) |>
    #calculate stats by season
    group_by(season_order) |>
    summarise(start_month_mode = calc_mode(month_start),
              end_month_mode = calc_mode(month_end),
              peak_month_mode = calc_mode(month_peak),
              dur_day_mean = as.numeric(mean(season_dur)),
              dur_day_median = as.numeric(median(season_dur)),
              amp_mean = mean(amp)) |>
    ungroup() |>
    #rename season_prim and season_sec
    mutate(season_order = ifelse(season_order == "season1", "seasonPrim", "seasonSec")) |>
    pivot_wider(names_from = "season_order", values_from = start_month_mode:amp_mean) 
  
  out <- cbind(num_season, year_2season, num_inactive, stat_by_season)
  
  return(out)
  
}

#' Function for the full run of analysis
#' Should be mapped over data.frames of smoothed time-series
#' @param ind_tseries an individual time series for a rice field
#' @rerturn output of calc_rice_avg (one-row of rice field stats)
wrap_rice <- function(ind_tseries, return_intermediate = FALSE){
  suppressMessages({
    deriv1 <- tryCatch(
      {deriv_tseries(ind_tseries, print_plot = FALSE)
      }, error = function(msg){
        return(data.frame(full_id = ind_tseries$full_id[[1]]))
      })
  
    
    season_stat <- tryCatch(
      {estimate_season_stats(deriv1, print_plot = FALSE)
      }, error = function(msg){
        return(data.frame(full_id = ind_tseries$full_id[[1]]))
      })
    
    stat_avg <- tryCatch(
      {calc_rice_avg(season_stat)
      }, error = function(msg){
        return(data.frame(full_id = ind_tseries$full_id[[1]]))
      })
  }) #suppress messages
  
  if(return_intermediate){
    return(list(season_stat, stat_avg))
  } else
    
    return(stat_avg)
  
}