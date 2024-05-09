#' @title Add malfunction flags to a data frame based on field notes and HydroVu.
#'
#' @description
#' This function adds the 'sensor malfunction' flag to a data frame by the dates,
#' sites, and parameters that members of the lab know contain erroneous data due
#' to sensor malfunctions. Note that this flag is used in two instances: first
#' when removing erroneous data from our statistics calculations and again during
#' the actual flagging step in the QAQC pipeline.
#'
#' @param df A data frame with a `flag` column.
#'
#' @param malfunction_records The malfunction records pulled from mWater
#'
#' @return A data frame with a `flag` column that has been updated with the
#' 'sensor malfunction' flag.
#'
#' @examples
#' add_range_flags(df = all_data_flagged$`archery-Actual Conductivity`)
#' add_range_flags(df = all_data_flagged$`boxelder-Temperature`)
#'
#' @seealso [grab_mwater_malfunction_records()]

add_malfunction_flag <- function(df, malfunction_records){

  # Filter malfunction records for relevant information
  df_site <- unique(df$site)
  df_parameter <- unique(df$parameter)

  malfunction_records_filtered <- malfunction_records %>%
    filter(site == df_site) %>%
    filter(is.na(parameter) | parameter == df_parameter) %>%
    mutate(end_DT = ifelse(is.na(end_DT), ymd_hms("9999-12-31 23:59:59", tz = "MST"), end_DT)) %>%
    mutate(end_DT = as.POSIXct(end_DT, tz = "MST"))

  if (nrow(malfunction_records_filtered > 0)) {

    # make a list of date intervals where sensor was malfunctioning
    malfunction_interval_list <- map2(
      .x = malfunction_records_filtered$start_DT,
      .y = malfunction_records_filtered$end_DT,
      .f = ~interval(.x, .y, tz = "MST"))

    # If DT_round is within any of the intervals where a sensor was malfunctioning, flag it
    df <- df %>%
      add_flag(DT_round %within% malfunction_interval_list, "sensor malfunction")
  }

  return(df)

}
