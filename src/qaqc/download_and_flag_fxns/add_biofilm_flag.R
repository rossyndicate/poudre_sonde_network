add_biofilm_flag <- function(df){

  # Only test for biofilm growth on turbidity sensors
  if("Turbidity" %in% df$parameter){

    # subset data to turbidity and conductivity only
    sub <- df %>%
      dplyr::filter(parameter %in% c("Turbidity", "Specific Conductivity")) %>%
      dplyr::select(DT_round, DT_join, parameter, mean) %>%
      tidyr::pivot_wider(names_from = parameter, values_from = mean)
    names(sub) <- make.names(names(sub))

    # Check if a given window's worth of sensor data has a linear
    # relationship with time (indicates a "steady" drift)
    progressive_drift <- function(x) {
      # Only assess time series with less than 90% missing data in
      # the rolling window:
      if(length(x[!is.na(x)]) > (length(x) - (length(x)*0.1))){

        # Fit linear model
        model <- lm(x ~ c(1:length(x)), na.action = na.omit)

        # Extract R-squared value
        r_squared <- summary(model)$r.squared

        # Return R-squared value
        return(r_squared)

      } else {

        # If not enough data, set slope to 0
        no_slope <- 0

        return(no_slope)

      }
    }

    # Function to check if a selected time window's mean R-squared value is
    # at least 60% (i.e., fairly high linear relationship with time indicates a "steady" drift)
    check_too_steady <- function(x) {
      mean(x) >= 0.60
    }

    # Function that uses all the functions above to see if a given time window's R-squared with time is strong. If the one-day OR the three-day slope
    # for a selected parameter is greater than 60%, we determine it has "failed" (i.e., drift seems to exist)

    biofilm_tester <- function(data = sub, col){

      data %>%
        data.table::data.table() %>%
        #bind_rows(flagged_data_dfs[["prospect-Turbidity"]]) %>%
        # dplyr::filter(lubridate::as_date(DT_round) >= "2022-09-10" & lubridate::as_date(DT_round) <= "2022-10-15") %>%
        dplyr::mutate(r2_s_right = data.table::frollapply(!!sym(col), n = 96, FUN = progressive_drift, align = "right", fill = NA),
                      r2_s_center = data.table::frollapply(!!sym(col), n = 96, FUN = progressive_drift, align = "left", fill = NA),
                      r2_l_right = data.table::frollapply(!!sym(col), n = 288, FUN = progressive_drift, align = "right", fill = NA),
                      r2_l_center = data.table::frollapply(!!sym(col), n = 288, FUN = progressive_drift, align = "left", fill = NA),
                      tightest_r = pmax(r2_s_center, r2_s_right, r2_l_center, r2_l_right, na.rm = TRUE),
                      failed = data.table::frollapply(tightest_r, n = 96, FUN = check_too_steady, align = "right", fill = NA)) %>%
        dplyr::select(!!(col) := "failed",
                      DT_join)
    }

    biofouling <- c("Turbidity", "Specific.Conductivity") %>%
      purrr::map(~biofilm_tester(data = sub, col = .)) %>%
      dplyr::bind_cols() %>%
      dplyr::rename(DT_join = DT_join...2) %>%
      dplyr::mutate(parameter = "Turbidity") %>%
      dplyr::right_join(., df, by = c("DT_join", "parameter")) %>%
      # If a steady slope is existing through time for turbidity, but NOT for conductivity, it is likely
      # sensor bio-fouling
      add_flag(., Turbidity == 1 & Specific.Conductivity != 1 & !grepl("biofouling or burial", flag), "biofouling or burial") %>%
      dplyr::select(-c(Turbidity, DT_join...4, Specific.Conductivity))

    return(biofouling)
  } else {return(df)}

}
#
# a=ggplot(drift_tested)+
#   geom_point(aes(x = DT_round, y = r_squaredSC_short ), color = "blue") +
#   geom_point(aes(x = DT_round, y = r_squaredT_short ), color = "red") +
#   geom_point(aes(x = DT_round, y = r_squaredDO_short), color = "black")
#
# b=ggplot(drift_tested)+
#   geom_point(aes(x = DT_round, y = r_squaredSC_long ), color = "blue") +
#   geom_point(aes(x = DT_round, y = r_squaredT_long ), color = "red") +
#   geom_point(aes(x = DT_round, y = r_squaredDO_long), color = "black")
#
# c=ggplot(drift_tested %>% filter(as_date(DT_round) >= "2022-10-10")) +
#   #geom_point(aes(x = DT_round, y = Specific.Conductivity)) +
#   geom_point(aes(x = DT_round, y = Turbidity, color = as.character(failed))) #+
# #geom_point(aes(x = DT_round, y = DO))
#
# ggpubr::ggarrange(a, b, c, ncol = 1)
#

