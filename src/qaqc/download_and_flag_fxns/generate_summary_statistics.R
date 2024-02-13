# Generate summary statistics for a given site parameter data frame.
#' @param site_param_df A data frame with a `mean` column retrieved from HydroVu API.
#' @return A data frame with summary statistics for a given site parameter data frame.
#' @examples
# generate_summary_statistics(site_param_df = all_data_flagged$`archery-Actual Conductivity`)
# generate_summary_statistics(site_param_df = all_data_flagged$`boxelder-Temperature`)

generate_summary_statistics <- function(site_param_df) {

  # # create seasons table
  # winter_baseflow <- c(12,1,2,3,4)
  # snowmelt <- c(5,6,NA,NA,NA)
  # monsoon <- c(7,8,9,NA,NA)
  # fall_baseflow <- c(10,11,NA,NA,NA)
  #
  # seasons <- cbind(winter_baseflow, snowmelt, monsoon, fall_baseflow) %>%
  #   as_tibble() %>%
  #   pivot_longer(cols = names(.), values_to = "month", names_to = "season") %>% drop_na()


  summary_stats_df <- site_param_df %>%
    # ... so that we can get the proper leading/lagging values across our entire timeseries:
    mutate(
      # Add the next value and previous value for mean.
      # Only do this for newest data (i.e., our appended historical
      # data already has these filled out and we don't want to over-
      # write them)
      front1 = ifelse(is.na(front1), lead(mean, n = 1), front1),
      back1 = ifelse(is.na(back1), lag(mean, n = 1), back1),
      # Add the median for a point and 6 points behind it:
      rollmed = ifelse(is.na(rollmed), roll_median(mean, n = 7, align = 'right', na.rm = F, fill = NA_real_), rollmed), # to do (j): check_na() function for when we append data
      # Add the mean for a point centered in a rolling mean of 7 points.
      rollavg = ifelse(is.na(rollavg), roll_mean(mean, n = 7, align = 'right', na.rm = F, fill = NA_real_), rollavg),
      # Add the standard deviation for a point centered in a rolling mean of 7 points.
      rollsd = ifelse(is.na(rollsd), roll_sd(mean, n = 7, align = 'right', na.rm = F, fill = NA_real_), rollsd),
      # Determine the slope of a point in relation to the point ahead and behind.
      # katie working on these (j)
      slope_ahead = ifelse(is.na(slope_ahead), (front1 - mean)/15, slope_ahead),
      slope_behind = ifelse(is.na(slope_behind), (mean - back1)/15, slope_behind),
      rollslope = ifelse(is.na(rollslope), roll_mean(slope_behind, n = 7, align = 'right', na.rm = F, fill = NA_real_), rollslope),
      # Add the standard deviation for points centered in a rolling slope of 7 points.
      # rollsdslope = roll_sd(slope_behind, n = 7, align = 'right', na.rm = F, fill = NA_real_),
      #KRW Comment: add monthly sd. add monthly avg. add 10th and 90th quantiles.
      # add some summary info for future us
      month = month(DT_round),
      year = year(DT_round),
      y_m = paste(year, '-', month),
      season = case_when(month %in% c(12,1,2,3,4) ~ "winter_baseflow",
                         month %in% c(5,6) ~ "snowmelt",
                         month %in% c(7,8,9) ~ "monsoon",
                         month %in% c(10,11) ~ "fall_baseflow",
                         TRUE ~ NA)
      )

  return(summary_stats_df)

}
