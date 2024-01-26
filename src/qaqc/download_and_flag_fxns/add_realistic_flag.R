# Adding flags related to realistic sensor ranges; these are instances where
# a value exceeds the expected ranges for the Poudre.
#Currently only applicable to pH, SC and Temperature

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
    add_flag(parameter == parameter_name & (mean < sensor_min | mean > sensor_max) & !grepl("outside of sensor realistic range", flag),
             "outside of sensor realistic range") %>%

    return(df)

}
