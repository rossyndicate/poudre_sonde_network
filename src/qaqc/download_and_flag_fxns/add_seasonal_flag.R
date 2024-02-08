# Adding flags related to parameter seasonal ranges
# Here we flag observations that fall outside of the seasonal 1st-99th percentile,
# specific to each site-parameter

add_seasonal_flag <- function(df) {

  # get the site name from the site column in the df of interest
  site_name <- unique(na.omit(df$site))
  # get the parameter from the parameter column in the df of interest
  parameter_name <- unique(na.omit(df$parameter))

  lookup <- threshold_lookup %>%
    filter(site == site_name & parameter == parameter_name) %>%
    select(!c(site, parameter))

  df <- df %>%
    # Using seasonal cut-offs...
    left_join(lookup, by = "season") %>%
    # coalesce (t_mean01, t_mean99, t_slope_behind_01, t_slope_behind_99, t_sd_0199)
    # mutate(t_mean01 = coalesce(t_mean01.x, t_mean01.y),
    #       t_mean99 = coalesce(t_mean99.x, t_mean99.y),
    #       t_slope_behind_01 = coalesce(t_slope_behind_01.x, t_slope_behind_01.y),
    #       t_slope_behind_99 = coalesce(t_slope_behind_99.x, t_slope_behind_99.y),
    #       t_sd_0199 = coalesce(t_sd_0199.x, t_sd_0199.y)) %>%
    # select(!c(t_mean01.x, t_mean01.y,
    #           t_mean99.x, t_mean99.y,
    #           t_slope_behind_01.x, t_slope_behind_01.y,
    #           t_slope_behind_99.x, t_slope_behind_99.y,
    #           t_sd_0199.x, t_sd_0199.y)) %>%
    #relocate(c(historical, over_50_percent_fail_window), .after = "t_sd_0199") %>% # to do (j): i don't love this solution
    # ... flag obs that are outside the seasonal 1st - 99th percentile range:
    add_flag(((mean < t_mean01 | mean > t_mean99) & !grepl("outside of seasonal range", flag)), "outside of seasonal range") %>%
    # flag obs whose slope is outside the 1st - 99th percentile range
    add_flag(((slope_ahead >= t_slope_behind_99 | slope_behind >= t_slope_behind_99 |
              slope_ahead <= t_slope_behind_01 | slope_behind <= t_slope_behind_01) & !grepl("slope violation", flag)), "slope violation")
    # ... flag obs that are outside of the rolling mean times 3 sd's of the 1-99 percentile seasonal values
    # add_flag(((mean <= rollavg - (3 * as.numeric(t_sd_0199)) | mean >= rollavg + (3 * as.numeric(t_sd_0199))) & !grepl("outside sd range", flag)), "outside sd range") %>%
    # ... flag obs that are outside of the rolling slope... how to play around with this better....
    # add_flag((parameter == "Turbidity") &
    #            (slope_behind <= rollslope - (3 * rollsdslope) | slope_behind >= rollslope + (3 * rollsdslope)), "outside sd range") %>%

  return(df)

}
