# Adding flags related to parameter ranges
# Sensor specification ranges are those which exceed manufacturer specified limits.
# Seasonal ranges are mean values which are below the .05 quantile or exceed the
# .99 quantile.

# parameter_ranges <- yaml::read_yaml("src/qaqc/range_limits/parameter_thresholds.yml")
sensor_spec_ranges <- yaml::read_yaml("src/qaqc/sensor_spec_thresholds.yml")
# this not need to be a yaml solution

# sensor spec ranges flagging function
# Some parameters are derived from a combination of other parameters. If any of those are wrong then they should be flagged.
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
