add_threshold_lines <- function(plot, plot_data, site_arg, parameter_arg) {

  # pull in threshold data (i don't like that I do this everytime the function is run)
  real_thresholds <- read_csv("data/qaqc/realistic_thresholds.csv", show_col_types = FALSE) %>%
    filter(parameter == parameter_arg)
  sensor_thresholds <- read_csv("data/qaqc/sensor_spec_thresholds.csv", show_col_types = FALSE) %>%
    filter(parameter == parameter_arg)
  seasonal_thresholds <- bind_rows(read_csv('data/qaqc/seasonal_thresholds.csv', show_col_type = FALSE), read_csv('data/qaqc/seasonal_thresholds_virridy.csv', show_col_types = FALSE)) %>%
    distinct(site, parameter, season, .keep_all = TRUE) %>%
    #read_csv("data/qaqc/seasonal_thresholds_virridy.csv", show_col_types = FALSE) %>%
    filter(parameter == parameter_arg,
           site == site_arg)

  # Determine the unique seasons in plot_data
  unique_seasons <- unique(plot_data$season)

  if (length(unique_seasons) > 1) {

    # Filter seasonal_thresholds for the seasons present in plot_data
    seasonal_thresholds <- seasonal_thresholds %>%
      filter(season %in% unique_seasons)

    season_1 <- case_when(
      all(c("winter_baseflow", "snowmelt") %in% unique_seasons) ~ "winter_baseflow",
      all(c("snowmelt", "monsoon") %in% unique_seasons) ~ "snowmelt",
      all(c("monsoon", "fall_baseflow") %in% unique_seasons) ~ "monsoon",
      all(c("fall_baseflow", "winter_baseflow") %in% unique_seasons) ~ "fall_baseflow",
      TRUE ~ NA_character_
    )
    season_2 <- case_when(
      all(c("winter_baseflow", "snowmelt") %in% unique_seasons) ~ "snowmelt",
      all(c("snowmelt", "monsoon") %in% unique_seasons) ~ "monsoon",
      all(c("monsoon", "fall_baseflow") %in% unique_seasons) ~ "fall_baseflow",
      all(c("fall_baseflow", "winter_baseflow") %in% unique_seasons) ~ "winter_baseflow",
      TRUE ~ NA_character_
    )

    seasonal_threshold_s1_quantiles <- unname(quantile(c(seasonal_thresholds %>% filter(season == season_1) %>% pull(t_mean01),
                                                         seasonal_thresholds %>% filter(season == season_1) %>% pull(t_mean99)),
                                                       c(0.1, 0.9)))

    seasonal_threshold_s2_quantiles <- unname(quantile(c(seasonal_thresholds %>% filter(season == season_2) %>% pull(t_mean01),
                                                         seasonal_thresholds %>% filter(season == season_2) %>% pull(t_mean99)),
                                                       c(0.1, 0.9)))

    slice_data <- plot_data %>%
      group_by(season) %>%
      slice(1) %>%
      ungroup()

    site_data <- filter(plot_data, site == site_arg)

    if (!all(is.na(site_data$mean))) {
        # Lower bound
        if ( # !is.infinite(min(site_data$mean, na.rm = TRUE)) &
            (min(site_data$mean, na.rm = TRUE) <= seasonal_threshold_s1_quantiles[1] | min(site_data$mean, na.rm = TRUE) <= seasonal_threshold_s2_quantiles[1])) {

          # Xs
          start_x_s1 <- min(slice_data$DT_round)

          transition_date <- slice_data %>%
            filter(season == season_2) %>%
            pull(DT_round)

          end_x_s2 <- ceiling_date(max(plot_data$DT_round), "day")

          #Ys
          y_lower_s1 <- seasonal_thresholds %>%
            filter(season == season_1) %>%
            pull(t_mean01)

          y_lower_s2 <- seasonal_thresholds %>%
            filter(season == season_2) %>%
            pull(t_mean01)

            plot <- plot +
              # season 1
              ## lower bound
              geom_segment(aes(x = start_x_s1, y = y_lower_s1,
                               xend = transition_date, yend = y_lower_s1,
                               color = "Seasonal Min", linetype = "Seasonal Min")) +
              # season 2
              ## lower bound
              geom_segment(aes(x = transition_date, y = y_lower_s2,
                               xend = end_x_s2, yend = y_lower_s2,
                               color = "Seasonal Min", linetype = "Seasonal Min"))

        }

        # Upper bound
        if (# !is.infinite(max(site_data$mean, na.rm = TRUE)) &
            (max(site_data$mean, na.rm = TRUE) >= seasonal_threshold_s1_quantiles[2] | max(site_data$mean, na.rm = TRUE) >= seasonal_threshold_s2_quantiles[2])) {

            # Xs
          start_x_s1 <- min(slice_data$DT_round)


            transition_date <- slice_data %>%
              filter(season == season_2) %>%
              pull(DT_round)

            end_x_s2 <- ceiling_date(max(plot_data$DT_round), "day")

            #Ys
            y_upper_s1 <- seasonal_thresholds %>%
              filter(season == season_1) %>%
              pull(t_mean99)

            y_upper_s2 <- seasonal_thresholds %>%
              filter(season == season_2) %>%
              pull(t_mean99)

            plot <- plot +
              # season 1
              ## lower bound
              geom_segment(aes(x = start_x_s1, y = y_upper_s1,
                               xend = transition_date, yend = y_upper_s1,
                               color = "Seasonal Max", linetype = "Seasonal Max")) +
              # season 2
              ## lower bound
              geom_segment(aes(x = transition_date, y = y_upper_s2,
                               xend = end_x_s2, yend = y_upper_s2,
                               color = "Seasonal Max", linetype = "Seasonal Max"))

        }

      # real thresholds
      real_thresholds_quantiles <- unname(quantile(c(real_thresholds$min, real_thresholds$max), c(0.1, 0.9)))

      if (#!is.infinite(min(site_data$mean, na.rm = TRUE))  &
        (min(site_data$mean, na.rm = TRUE) <= real_thresholds_quantiles[1])) {
        plot <- plot +
          geom_hline(aes(yintercept = real_thresholds$min,
                         color = "Real Min",
                         linetype = "Real"))
      }
      if (#!is.infinite(max(site_data$mean, na.rm = TRUE)) &
        (max(site_data$mean, na.rm = TRUE) >= real_thresholds_quantiles[2])) {
        plot <- plot +
          geom_hline(aes(yintercept = real_thresholds$max,
                         color = "Real Max",
                         linetype = "Real"))
      }

      # sensor thresholds
      sensor_thresholds_quantiles <- unname(quantile(c(sensor_thresholds$mix, sensor_thresholds$max), c(0.1, 0.9)))

      if (# !is.infinite(min(site_data$mean, na.rm = TRUE)) &
        (min(site_data$mean, na.rm = TRUE) <= sensor_thresholds_quantiles[1])) {
        plot <- plot +
          geom_hline(aes(yintercept = sensor_thresholds$mix, # *** this needs to be min
                         color = "Sensor Min",
                         linetype = "Sensor"))
      }
      if (#!is.infinite(max(site_data$mean, na.rm = TRUE)) &
        (max(site_data$mean, na.rm = TRUE) >= sensor_thresholds_quantiles[2])) {
        plot <- plot +
          geom_hline(aes(yintercept = sensor_thresholds$max,
                         color = "Sensor Max",
                         linetype = "Sensor"))
      }
      return(plot)
    }

  } else if (length(unique_seasons == 1)){

    site_data <- filter(plot_data, site == site_arg)

    # Filter seasonal_thresholds for the seasons present in site_data
    seasonal_thresholds <- seasonal_thresholds %>%
      filter(season %in% unique(site_data$season))

    if (!all(is.na(site_data$mean))) {
      # seasonal thresholds
      seasonal_thresholds_quantiles <- unname(quantile(c(seasonal_thresholds$t_mean01, seasonal_thresholds$t_mean99), c(0.1, 0.9)))

      if ((min(site_data$mean, na.rm = TRUE) <= seasonal_thresholds_quantiles[1]) == TRUE){
        plot <- plot +
          geom_hline(aes(yintercept = seasonal_thresholds$t_mean01,
                         color = "Seasonal Min",
                         linetype = "Seasonal"))
      }
      if ((max(site_data$mean, na.rm = TRUE) >= seasonal_thresholds_quantiles[2]) == TRUE) {
        plot <- plot +
          geom_hline(aes(yintercept = seasonal_thresholds$t_mean99,
                         color = "Seasonal Max",
                         linetype = "Seasonal"))
      }

      # real thresholds
      real_thresholds_quantiles <- unname(quantile(c(real_thresholds$min, real_thresholds$max), c(0.1, 0.9)))

      if (min(site_data$mean, na.rm = TRUE) <= real_thresholds_quantiles[1]) {
        plot <- plot +
          geom_hline(aes(yintercept = real_thresholds$min,
                         color = "Real Min",
                         linetype = "Real"))
      }
      if (max(site_data$mean, na.rm = TRUE) >= real_thresholds_quantiles[2]) {
        plot <- plot +
          geom_hline(aes(yintercept = real_thresholds$min,
                         color = "Real Max",
                         linetype = "Real"))
      }

      # sensor thresholds
      sensor_thresholds_quantiles <- unname(quantile(c(sensor_thresholds$mix, sensor_thresholds$max), c(0.1, 0.9)))

      if (min(site_data$mean, na.rm = TRUE) <= sensor_thresholds_quantiles[1]) {
        plot <- plot +
          geom_hline(aes(yintercept = sensor_thresholds$mix,
                     color = "Sensor Mine",
                     linetype = "Sensor"))
      }
      if (max(site_data$mean, na.rm = TRUE) >= sensor_thresholds_quantiles[2]) {
        plot <- plot +
          geom_hline(aes(yintercept = sensor_thresholds$max,
                         color = "Sensor Max",
                         linetype = "Sensor"))
      }
      return(plot)
    }
    return(plot)
  }
  return(plot)
}
