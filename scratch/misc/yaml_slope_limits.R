# JUAN OG YAML CODE. MUST PRESERVE ----
# Add the function to add slope flags using parameter thresholds yaml file

add_slope_flags <- function(df) {

  # get the parameter from the parameter column in the df of interest
  parameter_name <- unique(na.omit(df$parameter))
  # Pull the threshold from the yaml file
  threshold <- eval(parse(text = parameter_ranges[[parameter_name]]$slope_threshold))

  df <- df %>%
    # adding slope flag
    add_flag((slope_ahead >= threshold | slope_behind >= threshold), "slope flag suspect") %>%
    alter_flag((slope_ahead >= threshold & slope_behind >= threshold), "slope flag suspect", "slope flag actual")

  return(df)

}
