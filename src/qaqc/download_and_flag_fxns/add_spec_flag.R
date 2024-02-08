# Adding flags related to sensor specification ranges; these are instances where
# a value exceeds the manufacturer specified limits.

add_spec_flag <- function(df){

  sensor_spec_ranges <- read_yaml("src/qaqc/sensor_spec_thresholds.yml")

  # make this a non yaml solution and add it to the threshold table
  # get the parameter from the parameter column in the df of interest
  parameter_name <- unique(na.omit(df$parameter))
  # Pull the sensor specification range from the yaml file
  sensor_min <- eval(parse(text = sensor_spec_ranges[[parameter_name]]$min))
  sensor_max <- eval(parse(text = sensor_spec_ranges[[parameter_name]]$max))

  df <- df %>%
    # adding sensor range flags
    add_flag(parameter == parameter_name & (mean < sensor_min | mean > sensor_max) & !grepl("outside of sensor specification range", flag),
             "outside of sensor specification range") %>%

    return(df)

}
