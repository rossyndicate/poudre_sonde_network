#' Calculate Fallback Drift Windows
#'
#' @description
#' Groups continuous periods of unverified drift into blocks based on time gaps
#' and assigns default correction strategies.
#'
#' @param df Dataframe. The current primary sensor data. Must include columns
#'   \code{drift} (logical), \code{mean_analysis} (numeric), and \code{DT_round} (POSIXct).
#'
#' @details
#' \itemize{
#'   \item Filters sensor data down to rows explicitly flagged as containing drift where \code{mean_analysis} is not missing.
#'   \item Calculates the time difference (\code{gap}) between consecutive observations.
#'   \item Groups continuous periods of drift into blocks (\code{window_id}), splitting them whenever a time gap exceeds 1 day.
#'   \item Aggregates each block to extract bounding timestamps (\code{start_dt}, \code{end_dt}) and the final sensor reading value (\code{end_data_val}).
#'   \item Appends default strategy tags (\code{"None"} for drift type and \code{"additive"} for formulation type).
#' }
#'
#' @return A summary tibble containing one row per calculated drift window block with columns:
#'   \code{window_id}, \code{start_dt}, \code{end_dt}, \code{end_data_val}, \code{arg_drift_type}, and \code{arg_correction_type}.
calculate_fallback_windows <- function(df) {
  df %>%
    filter(drift & !is.na(mean_analysis)) %>%
    arrange(DT_round) %>%
    #TODO: Consider other options in the event that drift occurs within a day
    mutate(gap = as.numeric(DT_round - lag(DT_round), units = "days")) %>%
    mutate(window_id = cumsum(if_else(is.na(gap) | gap > 1, 1, 0))) %>%
    group_by(window_id) %>%
    summarise(
      start_dt = min(DT_round),
      end_dt = max(DT_round),
      end_data_val = last(mean_analysis),
      .groups = 'drop'
    ) %>%
    mutate(
      arg_drift_type = "None",
      arg_correction_type = "additive"
    )
}
