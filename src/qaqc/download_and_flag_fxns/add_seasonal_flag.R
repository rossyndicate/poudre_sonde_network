# Adding flags related to parameter seasonal ranges
# Here we flag observations that fall outside of the seasonal 1st-99th percentile,
# specific to each site-parameter

add_seasonal_flag <- function(df) {

  # get the site name from the site column in the df of interest
  site_name <- unique(na.omit(df$site))
  # get the parameter from the parameter column in the df of interest
  parameter_name <- unique(na.omit(df$parameter))

  lookup <- threshold_lookup %>%
    filter(site == site_name & parameter == parameter_name)

  df <- df %>%
    # Using seasonal cut-offs...
    left_join(lookup, by = "season") %>%
    select(!c(site.y, parameter.y),
           site = site.x,
           parameter = parameter.x) %>%
    # ... flag obs that are outside the seasonal 1-99 percentile range:
    add_flag((mean < t_mean01 | mean > t_mean99), "outside of seasonal range") %>%
    # flag obs whose slope is greater than the 99th percentile range
    add_flag((slope_ahead >= t_slope_behind_99 | slope_behind >= t_slope_behind_99), "slope violation") %>%
    # ... flag obs that are outside of the rolling mean times 3 sd's of the 1-99 percentile seasonal values
    add_flag((mean <= rollavg - (3 * t_sd_0199) | mean >= rollavg + (3 * t_sd_0199)), "outside sd range") %>%
    # ... flag obs that are outside of the rolling mean times 3 sd's of the 1-99 percentile seasonal values
    add_flag((mean <= rollavg - (3 * t_sd_0199) | mean >= rollavg + (3 * t_sd_0199)), "outside sd range") #%>%
   # ... flag obs that are outside of the rolling slope... how to play around with this better....
    # add_flag((parameter == "Turbidity") &
    #            (slope_behind <= rollslope - (3 * rollsdslope) | slope_behind >= rollslope + (3 * rollsdslope)), "outside sd range") %>%

  return(df)

}
