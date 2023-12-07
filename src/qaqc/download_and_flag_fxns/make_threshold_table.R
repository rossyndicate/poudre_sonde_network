make_threshold_table <- function(df){

  good_data_stats <- df %>%
    # REMOVE DATA WE KNOW TO BE ERRONEOUS:
    add_malfunction_flag() %>%
    # we should also add the outside of sensor specification flag here?
    add_spec_flag() %>%
    filter(is.na(flag)) %>%
    group_by(season) %>%
    mutate(f01 = quantile(mean, 0.01, na.rm = TRUE),
           f99 = quantile(mean, 0.99, na.rm = TRUE),
           f_slope_behind_01 = quantile(slope_behind, 0.01, na.rm = TRUE),
           f_slope_behind_99 = quantile(slope_behind, 0.99, na.rm = TRUE)) %>%
    # THEN, GET STANDARD DEVIATION OF ONLYYYY VALUES WITHIN THE 1-99th PERCENTILE OF THAT GOOD DATA:
    filter(mean > f01 & mean < f99) %>%
    # SD is the ONLY statistic that uses this winnowed-down data set in its development.
    # All else use the full, "good" data set.
    summarize(site = paste0(unique(site)),
              parameter = paste0(unique(parameter)),
              t_mean01 = as.numeric(paste0(unique(f01))),
              t_mean99 = as.numeric(paste0(unique(f99))),
              t_slope_behind_01 = as.numeric(paste0(unique(f_slope_behind_01))),
              t_slope_behind_99 = as.numeric(paste0(unique(f_slope_behind_99))),
              t_sd_0199 = sd(mean, na.rm = T))

  return(good_data_stats)

  }
