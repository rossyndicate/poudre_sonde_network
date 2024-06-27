#' @title Generate Summary Statistics
#'
#' @description
#' A function that generates summary statistics for a given site parameter data frame.
#' The generated statistics include:
#'   - The next value and previous value for the mean.
#'   - The rolling 7-point median of the mean.
#'   - The rolling 7-point mean of the mean.
#'   - The rolling 7-point standard deviation of the mean.
#'   - The slope of a point in relation to the point ahead and behind.
#'   - The rolling 7-point slope of the mean.
#'   - The month and year of each data point.
#'   - The year-month combination.
#'
#' @param site_param_df A data frame with a `mean` column retrieved from HydroVu API.
#'
#' @return A data frame with summary statistics for a given site parameter data frame.
#'
#' @examples
#' generate_summary_statistics(site_param_df = all_data_flagged$`archery-Actual Conductivity`)
#' generate_summary_statistics(site_param_df = all_data_flagged$`boxelder-Temperature`)

generate_summary_statistics_full <- function(site_param_df) {

  summary_stats_df <- site_param_df %>%
    # ... so that we can get the proper leading/lagging values across our entire timeseries:
    dplyr::mutate(
      # Add the next value and previous value for mean.
      front1 = dplyr::lead(mean, n = 1),
      back1 = dplyr::lag(mean, n = 1),
      # Add the rolling 7-point median (using itself + data of the past).
      rollmed = RcppRoll::roll_median(mean, n = 7, align = 'right', na.rm = F, fill = NA_real_), #ifelse(is.na(rollmed), roll_median(mean, n = 7, align = 'right', na.rm = F, fill = NA_real_), rollmed), # to go (j): check_na() function for when we append data
      # Add the rolling 7-point mean (using itself + data of the past).
      rollavg = RcppRoll::roll_mean(mean, n = 7, align = 'right', na.rm = F, fill = NA_real_), #ifelse(is.na(rollavg), roll_mean(mean, n = 7, align = 'right', na.rm = F, fill = NA_real_), rollavg),
      # Add the rolling 7-point standard deviation (using itself + data of the past).
      rollsd = RcppRoll::roll_sd(mean, n = 7, align = 'right', na.rm = F, fill = NA_real_), #ifelse(is.na(rollsd), roll_sd(mean, n = 7, align = 'right', na.rm = F, fill = NA_real_), rollsd),
      # Determine the slope of a point in relation to the point ahead and behind
      slope_ahead = (front1 - mean)/15,
      slope_behind = (mean - back1)/15,
      # Get the rolling 7-point slope (using itself + data of the past).
      rollslope = RcppRoll::roll_mean(slope_behind, n = 7, align = 'right', na.rm = F, fill = NA_real_), #ifelse(is.na(rollslope), roll_mean(slope_behind, n = 7, align = 'right', na.rm = F, fill = NA_real_), rollslope),
      # add some summary info for future steps
      month = lubridate::month(DT_round),
      year = lubridate::year(DT_round),
      y_m = paste0(year, '-', month),
      # Define our seasons:
      season = dplyr::case_when(month %in% c(12, 1, 2, 3, 4) ~ "winter_baseflow",
                                month %in% c(5, 6) ~ "snowmelt",
                                month %in% c(7, 8, 9) ~ "monsoon",
                                month %in% c(10, 11) ~ "fall_baseflow",
                                TRUE ~ NA)
    )

  return(summary_stats_df)

}
