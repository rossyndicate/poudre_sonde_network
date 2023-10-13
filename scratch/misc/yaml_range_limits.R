# OG YAML CODE OF JUAN'S
# Add the function to add the range flags using the parameter thresholds yaml file

parameter_ranges <- yaml::read_yaml("src/qaqc/range_limits/parameter_thresholds.yml")
sensor_spec_ranges <- yaml::read_yaml("src/qaqc/range_limits/sensor_spec_thresholds.yml")

# sensor spec ranges flagging function
  # Some parameters are derived from a combination of other parameters. If any of those are wrong then they should be flagged.
add_range_flags <- function(df) {

  # get the parameter from the parameter column in the df of interest
  parameter_name <- unique(na.omit(df$parameter))
  # Pull the sensor specification range from the yaml file
  sensor_min <- eval(parse(text = sensor_spec_ranges[[parameter_name]]$min))
  sensor_max <- eval(parse(text = sensor_spec_ranges[[parameter_name]]$max))
  # Pull the lab bound range from the yaml file
  lab_min <- eval(parse(text = parameter_ranges[[parameter_name]]$lab_bounds$min))
  lab_max <- eval(parse(text = parameter_ranges[[parameter_name]]$lab_bounds$max))

  df <- df %>%
    # adding sensor range flags
    add_flag(parameter == parameter_name & (mean < sensor_min | mean > sensor_max),
             paste0("out of (", parameter_name,") sensor specification range")) %>%
    # adding lab bound flags
    add_flag(parameter == parameter_name & (mean < lab_min | mean > lab_max),
             paste0("out of (", parameter_name,") lab bounds"))

  return(df)

}
