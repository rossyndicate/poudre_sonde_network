#' @title Generate Threshold Table for QAQC
#' 
#' @description
#' A function designed to generate a threshold table for QAQC. This table
#' contains the thresholds for the mean, slope_behind, and standard deviation
#' of the mean for each site and season.
#' 
#' @param df A data frame with a `flag` column.
#' 
#' @return A data frame with the thresholds for the mean, slope_behind, and
#' standard deviation of the mean for each site and season.
#' 
#' @examples
#' make_threshold_table(df = all_data_flagged$`archery-Actual Conductivity`)

make_threshold_table <- function(df){

  slope_down <- df %>%
    # REMOVE DATA WE KNOW TO BE ERRONEOUS:
    add_malfunction_flag() %>%
    add_spec_flag() %>%
    filter(is.na(flag)) %>%
    # Get threshold for negative slope data
    filter(slope_behind < 0) %>%
    group_by(season) %>%
    summarize(f_slope_behind_01 = quantile(slope_behind, 0.01, na.rm = TRUE))

  slope_up <- df %>%
    # REMOVE DATA WE KNOW TO BE ERRONEOUS:
    add_malfunction_flag() %>%
    add_spec_flag() %>%
    filter(is.na(flag)) %>%
    # Get threshold for positive slope data
    filter(slope_behind > 0) %>%
    group_by(season) %>%
    summarize(f_slope_behind_99 = quantile(slope_behind, 0.99, na.rm = TRUE))

  good_data_stats <- df %>%
    # REMOVE DATA WE KNOW TO BE ERRONEOUS:
    add_malfunction_flag() %>%
    add_spec_flag() %>%
    filter(is.na(flag)) %>%
    group_by(season) %>%
    # join our slope data thresholds:
    left_join(slope_up, by = "season") %>%
    left_join(slope_down, by = "season") %>%
    # develop other thresholds across all data
    mutate(f01 = quantile(mean, 0.01, na.rm = TRUE),
           f99 = quantile(mean, 0.99, na.rm = TRUE)) %>%
           # f_slope_behind_01 = slope_down, #quantile(slope_behind, 0.01, na.rm = TRUE),
           # f_slope_behind_99 = slope_up) %>% #quantile(slope_behind, 0.99, na.rm = TRUE)) %>%
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
              # This stat is useless. Should remove eventually.
              t_sd_0199 = sd(mean, na.rm = T))

  return(good_data_stats)

}
