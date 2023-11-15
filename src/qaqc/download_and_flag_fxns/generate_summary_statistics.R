# Generate summary statistics for a given site parameter data frame.
#' @param site_param_df A data frame with a `mean` column retrieved from HydroVu API.
#' @return A data frame with summary statistics for a given site parameter data frame.
#' @examples
# generate_summary_statistics(site_param_df = all_data_flagged$`archery-Actual Conductivity`)
# generate_summary_statistics(site_param_df = all_data_flagged$`boxelder-Temperature`)

generate_summary_statistics <- function(site_param_df) {

  # create seasons table
  winter_baseflow <- c(12,1,2,3,4)
  snowmelt <- c(5,6,NA,NA,NA)
  monsoon <- c(7,8,9,NA,NA)
  fall_baseflow <- c(10,11,NA,NA,NA)

  seasons <- cbind(winter_baseflow, snowmelt, monsoon, fall_baseflow) %>%
    as_tibble() %>%
    pivot_longer(cols = names(.), values_to = "month", names_to = "season") %>% drop_na()


  summary_stats_df <- site_param_df %>%
    # ... so that we can get the proper leading/lagging values across our entire timeseries:
    mutate(
      # Add the next value and previous value for mean.
      front1 = lead(mean, n = 1),
      back1 = lag(mean, n = 1),
      # Add the median for a point centered in a rolling median of 7 points.
      rollmed = roll_median(mean, n = 7, align = 'center', na.rm = F, fill = NA_real_),
      # Add the mean for a point centered in a rolling mean of 7 points.
      rollavg = roll_mean(mean, n = 7, align = 'center', na.rm = F, fill = NA_real_),
      # Add the standard deviation for a point centered in a rolling mean of 7 points.
      rollsd = roll_sd(mean, n = 7, align = 'center', na.rm = F, fill = NA_real_),
      # Determine the slope of a point in relation to the point ahead and behind.
      slope_ahead = abs(front1 - mean)/15,
      slope_behind = (mean - back1)/15,
      rollslope = roll_mean(slope_behind, n = 7, align = 'center', na.rm = F, fill = NA_real_),
      # Add the standard deviation for points centered in a rolling slope of 7 points.
      # rollsdslope = roll_sd(slope_behind, n = 7, align = 'center', na.rm = F, fill = NA_real_),
      #KRW Comment: add monthly sd. add monthly avg. add 10th and 90th quantiles.
      # add some summary info for future us
      month = month(DT_round),
      year = year(DT_round),
      y_m = paste(year, '-', month)) %>%
    left_join(seasons, by = "month")

  return(summary_stats_df)

}
