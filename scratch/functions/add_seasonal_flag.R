# Adding flags related to parameter seasonal ranges
# Here we flag observations that fall outside of the seasonal 1st-99th percentile,
# specific to each site-parameter

add_seasonal_flag <- function(df, threshold_table) {

  # get the site name from the site column in the df of interest
  site_name <- unique(na.omit(df$site))
  # get the parameter from the parameter column in the df of interest
  parameter_name <- unique(na.omit(df$parameter))

  lookup <- threshold_table %>%
    filter(site == site_name & parameter == parameter_name) %>%
    select(!c(site, parameter))

  df <- df %>%
    # Using seasonal cut-offs...
    left_join(lookup, by = "season") %>%
    # ... flag obs that are outside the seasonal 1st - 99th percentile range:
    add_flag(((mean < t_mean01 | mean > t_mean99) & !grepl("outside of seasonal range", flag)), "outside of seasonal range") %>%
    # flag obs whose slope is outside the 1st - 99th percentile range
    add_flag(((slope_ahead >= t_slope_behind_99 | slope_behind >= t_slope_behind_99 |
                 slope_ahead <= t_slope_behind_01 | slope_behind <= t_slope_behind_01) & !grepl("slope violation", flag)), "slope violation")

  return(df)

}
