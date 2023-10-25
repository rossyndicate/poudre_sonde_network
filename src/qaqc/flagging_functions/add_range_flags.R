# Add flags to the `flag` column of a dataframe based on sensor specification ranges and seasonal ranges.
  # "outside of sensor specification range" flag is added if the mean is outside of the sensor specification range.
  # "outside of seasonal range" flag is added if the mean is outside of the seasonal range.
# @param df A dataframe with a `flag` column.
# @return A dataframe with a `flag` column that has been updated with the relevant range flag description.
# @examples
# add_range_flags(df = all_data_flagged$`archery-Actual Conductivity`)
# add_range_flags(df = all_data_flagged$`boxelder-Temperature`)

add_range_flags <- function(df) {

  # get the parameter from the parameter column in the df of interest
  parameter_name <- unique(na.omit(df$parameter))
  # Pull the sensor specification range from the yaml file
  sensor_min <- eval(parse(text = sensor_spec_ranges[[parameter_name]]$min))
  sensor_max <- eval(parse(text = sensor_spec_ranges[[parameter_name]]$max))

  df <- df %>%
    # adding sensor range flags
    add_flag(parameter == parameter_name & (mean < sensor_min | mean > sensor_max),
             paste0("outside of ", parameter_name," sensor specification range")) %>%
    # adding lab bound flags
    add_flag(parameter == parameter_name & (mean < m_mean05 | mean > m_mean99),
             paste0("outside of ", parameter_name," seasonal range"))

  return(df)

}
