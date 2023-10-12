generate_flag_report <- function(df, df_index) {

  # Extract the site and parameter from the df_name
  site <- sub("^([^\\-]+)-.*$", "\\1", df_index)
  parameter <- sub("^[^-]+-(.*)$", "\\1", df_index)

  list_of_flags <- c("sonde not employed",
                     "site visit",
                     "sv window",
                     "sensor specification range",
                     "seasonal range",
                     "slope flag suspect",
                     "outside sd range",
                     "repeated value",
                     "missing data",
                     "24hr anomaly",
                     "anomaly window")

  # summarize total data points
  total_observations <- df %>%
    summarise(n_total = n_distinct(DT_round)) %>%
    pull(n_total)

  # summarize total days
  total_observations_dates <- df %>%
    group_by(date = format(DT_round, "%m-%d-%Y")) %>%
    summarize(n_total = nrow(date)) %>%
    nrow()

  row_list <- list()
  for (i in list_of_flags) {
    # summarize flagged data points
    flagged_observations <- df %>%
      filter(str_detect(flag, i)) %>%
      summarise(n_flag = n_distinct(DT_round)) %>%
      pull(n_flag)
    # summarize percent data points that are flagged
    percent_flagged <- flagged_observations/total_observations

    # summarize flagged days
    flagged_observations_dates <- df %>%
      filter(str_detect(flag, i)) %>%
      group_by(date = format(DT_round, "%m-%d-%Y")) %>%
      summarize(n_total = nrow(date)) %>%
      nrow()
    # summarize percent days that are flagged
    percent_flagged_dates <- flagged_observations_dates/total_observations_dates

    # creating a vector with the information
    calculated_values <- tibble(
      # metadata
      site = site,
      parameter = parameter,
      flag = i,
      # data points
      data_points_flagged_percentage = format(percent_flagged * 100, nsmall=2),
      data_points_flagged = flagged_observations,
      total_data_points = total_observations,
      # dates
      dates_flagged_percentage = format(percent_flagged_dates * 100, nsmall=2),
      dates_flagged = flagged_observations_dates,
      total_dates = total_observations_dates
      )

    row_list[[i]] <- calculated_values
  }

  #calculated_df <- bind_cols(row_list)
  return(bind_rows(row_list))

}

flag_report <- imap(all_data_flagged, ~generate_flag_report(.x,.y)) %>%
  bind_rows()

View(flag_report)

flag_report_1 <- flag_report %>%
  group_by(site, parameter) %>%
  arrange(desc(data_points_flagged_percentage), .by_group = TRUE)

View(flag_report_1)
