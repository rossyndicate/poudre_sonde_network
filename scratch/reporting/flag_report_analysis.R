generate_flag_report <- function(df) {

  # Extract the site and parameter from the df_name
  site <- unique(na.omit(df$site))
  parameter <- unique(na.omit(df$parameter))

  list_of_flags <- c("sonde not employed", # add_field_flag()
                     "site visit", # add_field_flag()
                     "sv window", # add_field_flag()
                     "sensor malfunction", # add_malfunction_flag()
                     "outside of sensor specification range", # add_spec_flag()
                     "outside of seasonal range", # add_seasonal_flag()
                     "slope violation", # add_seasonal_flag()
                     "outside sd range", # add_seasonal_flag()
                     "repeated value", # add_repeat_flag()
                     "missing data", # add_na_flag()
                     "suspect data") # add_suspect_flag()

  # check these
  sans_na_flags <- "^(missing data|
                      sonde not employed;\\nmissing data|
                      missing data;\\nsuspect data|
                      sonde not employed;\\nmissing data;\\nsuspect data|
                      site visit;\\nmissing data;\\nsuspect data|
                      sv window;\\nmissing data;\\nsuspect data)$"

  # summarize total data points
  total_observations <- df %>%
    summarise(n_total = n_distinct(DT_round)) %>%
    pull(n_total)

  # summarize total data points with no flags
  tot_no_flag <- df %>%
    summarise(n_total_no_flag = n(is.na(flag))) %>%
    pull(n_total_no_flag)

  # summarize total data points sans missing data
  total_observations_1 <- df %>%
    # filter out when flag has only missing data or only sonde not employed and missing data
    filter(!str_detect(flag, sans_na_flags)) %>%
    summarise(n_total = n_distinct(DT_round)) %>%
    pull(n_total)

  # summarize total data points with no flags sans missing data
  tot_no_flag_sans_na <- df %>%
    filter(!str_detect(flag, sans_na_flags)) %>%
    summarise(n_total_no_flag_sans_na = n(is.na(flag))) %>%
    pull(n_tota_no_flag_san_na)

  # summarize total days
  total_observations_dates <- df %>%
    group_by(date = format(DT_round, "%m-%d-%Y")) %>%
    summarise(n_total = nrow(date)) %>%
    nrow()

  # summarize total days with no flags
  tot_day_no_flag <- df %>%
    group_by(date = format(DT_round, "%m-%d-%Y")) %>%
    filter(is.na(flag)) %>%
    nrow()

  # summarize total days sans missing data
  total_observations_dates_1 <- df %>%
    filter(!str_detect(flag, sans_na_flags)) %>%
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
    # summarize flagged data points
    flagged_observations_1 <- df %>%
      filter(!str_detect(flag, sans_na_flags)) %>%
      filter(str_detect(flag, i)) %>%
      summarise(n_flag = n_distinct(DT_round)) %>%
      pull(n_flag)
    # summarize percent data points that are flagged
    percent_flagged <- flagged_observations/total_observations
    # summarize percent data points that are flagged sans missing data
    percent_flagged_1 <- flagged_observations_1/total_observations_1

    # summarize flagged days
    flagged_observations_dates <- df %>%
      filter(str_detect(flag, i)) %>%
      group_by(date = format(DT_round, "%m-%d-%Y")) %>%
      summarize(n_total = nrow(date)) %>%
      nrow()
    # summarize flagged days
    flagged_observations_dates_1 <- df %>%
      filter(!str_detect(flag, sans_na_flags)) %>%
      filter(str_detect(flag, i)) %>%
      group_by(date = format(DT_round, "%m-%d-%Y")) %>%
      summarize(n_total = nrow(date)) %>%
      nrow()
    # summarize percent days that are flagged
    percent_flagged_dates <- flagged_observations_dates/total_observations_dates
    # summarize percent days that are flagged
    percent_flagged_dates_1 <- flagged_observations_dates_1/total_observations_dates_1

    # creating a row with the information
    calculated_values <- tibble(
      # metadata
      site = site,
      parameter = parameter,
      flag = i,
      # data points
      data_points_flagged_percentage = format(round(percent_flagged * 100, 2), nsmall = 2),
      data_points_flagged = flagged_observations,
      total_data_points = total_observations,
      data_points_flagged_percentage_sans_na = format(round(percent_flagged_1 * 100, 2), nsmall = 2),
      data_points_flagged_sans_na = flagged_observations_1,
      total_data_points_sans_na = total_observations_1,
      # dates
      dates_flagged_percentage = format(round(percent_flagged_dates * 100, 2), nsmall=2),
      dates_flagged = flagged_observations_dates,
      total_dates = total_observations_dates,
      dates_flagged_percentage_sans_na = format(round(percent_flagged_dates_1 * 100, 2), nsmall=2),
      dates_flagged_sans_na = flagged_observations_dates_1,
      total_dates_sans_na = total_observations_dates_1
    ) %>%
      mutate(
        data_points_flagged_percentage = as.double(data_points_flagged_percentage),
        data_points_flagged_percentage_sans_na = as.double(data_points_flagged_percentage_sans_na),
        dates_flagged_percentage = as.double(dates_flagged_percentage),
        dates_flagged_percentage_sans_na = as.double(dates_flagged_percentage_sans_na)
      )

    row_list[[i]] <- calculated_values
  }

  # bound_rows_df <- bind_rows(row_list) ----
  #
  # no_flag_row <- tibble(
  #   # metadata
  #   site = site,
  #   parameter = parameter,
  #   flag = "no flag",
  #   # data points
  #   data_points_flagged_percentage = (100.00 - max((bound_rows_df$data_points_flagged_percentage))),
  #   data_points_flagged = (max(bound_rows_df$total_data_points) - max(bound_rows_df$data_points_flagged)),
  #   total_data_points = max(bound_rows_df$total_data_points),
  #   data_points_flagged_percentage_sans_na = (100.00 - max(bound_rows_df$data_points_flagged_percentage_sans_na)),
  #   data_points_flagged_sans_na = (max(bound_rows_df$total_data_points_sans_na) - max(bound_rows_df$data_points_flagged_sans_na)),
  #   total_data_points_sans_na = max(bound_rows_df$total_data_points_sans_na),
  #   # dates
  #   dates_flagged_percentage = (100.00 - max(bound_rows_df$dates_flagged_percentage)),
  #   dates_flagged = (max(bound_rows_df$total_dates) - max(bound_rows_df$dates_flagged)),
  #   total_dates = max(bound_rows_df$total_dates),
  #   dates_flagged_percentage_sans_na = (100.00 - max(bound_rows_df$dates_flagged_percentage_sans_na)),
  #   dates_flagged_sans_na = (max(bound_rows_df$total_dates_sans_na) - max(bound_rows_df$dates_flagged_sans_na)),
  #   total_dates_sans_na = max(bound_rows_df$total_dates_sans_na)
  # )
  #
  # final_df <- bind_rows(no_flag_row, bound_rows_df)

  return(final_df) # ----

}

# making the df
new1 <- flagged_data_dfs[!grepl("Battery Level", names(flagged_data_dfs), ignore.case = TRUE)]
new2 <- new1[!grepl("External Voltage", names(new1), ignore.case = TRUE)]
new_flag_report <-  map(new2 ,generate_flag_report) %>%
  bind_rows()

# using the df to make bar charts

df <- new_flag_report %>%
  filter(flag == "no flag")

# Create a stacked bar chart
ggplot(df, aes(x = data_points_flagged_percentage)) +
  geom_histogram()
