#' @title Add a flag if the value of a parameter exceeds its realistic range.
#'
#' @description
#' A function designed to append the 'outside of sensor realistic range' flag to
#' a row if the `mean` column in that row exceeds the realistic ranges that are
#' set in the `src/qaqc/sensor_real_thresholds.yml` file. These are instances
#' where a value exceeds the expected ranges for the Poudre. Note that this is
#' currently only aplied to pH, SC, and Temperature.
#'
#' @param df A data frame with a `flag` column.
#'
#' @return A data frame with a `flag` column that has been updated with the
#' 'outside of sensor realistic range' flag.
#'
#' add_range_flags(df = all_data_flagged$`archery-Actual Conductivity`)
#' add_range_flags(df = all_data_flagged$`boxelder-Temperature`)

add_realistic_flag <- function(df){

  sensor_realistic_ranges <- read_yaml("src/qaqc/sensor_real_thresholds.yml")

  # make this a non yaml solution and add it to the threshold table
  # get the parameter from the parameter column in the df of interest
  parameter_name <- unique(na.omit(df$parameter))
  # Pull the sensor specification range from the yaml file
  sensor_min <- eval(parse(text = sensor_realistic_ranges[[parameter_name]]$min))
  sensor_max <- eval(parse(text = sensor_realistic_ranges[[parameter_name]]$max))

  df <- df %>%
    # adding sensor range flags
    add_flag(parameter == parameter_name & (mean < sensor_min | mean > sensor_max),
             "outside of sensor realistic range")

    return(df)

}
