generate_flag_report2 <- function(df) {

  # Extract the site and parameter from the df_name ----
  df_site <- unique(na.omit(df$site))
  df_parameter <- unique(na.omit(df$parameter))

  # Create the list of flags we are searching for ----
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

  # Combinations of flags that we will exclude for nona (columns with missing data that skews the percentages) columns ----
  sans_na_flags <- "^(missing data|
                      sonde not employed;\\nmissing data|
                      missing data;\\nsuspect data|
                      sonde not employed;\\nmissing data;\\nsuspect data|
                      site visit;\\nmissing data;\\nsuspect data|
                      sv window;\\nmissing data;\\nsuspect data)$"

  # Create structure for tibble ----
  flag_report_structure <- tibble(

    # metadata (1)
    site = NA,
    parameter = NA,
    flag = NA,

    # data points: including all data points (2)
    tot_data = NA, # The total amount of instances collected for the <site parameter> group
    no_flag_data = NA, # The total amount of instances that are not flagged for the <site parameter> group
    no_flag_data_pct = NA, # no_flag_data/tot_data; percentage of data points not flagged out of total data points
    flag_data = NA, # The total amount of instances that are flagged for the <site parameter flag> group
    flag_data_pct = NA, # flag_data/tot_data; percent of data points flagged out of total data points

    # data points: excluding data points with missing data (as in data points with the values in `san_na_flags` in their flag columns) (3)
    tot_data_nona = NA, # The total amount of instances collected for the <site parameter> (excluding missing data)
    no_flag_data_nona = NA, # The total amount of instances that are not flagged for the <site parameter> (excluding missing data)
    no_flag_data_pct_nona = NA, # no_flag_data_nona/tot_data_nona; percentage of data points not flagged out of total data points (excluding missing data)
    flag_data_nona = NA, # The total amount of instances for the <site parameter flag> (excluding missing data)
    flag_data_pct_nona = NA, # flag_data/tot_data_nona; percent of data points flagged out of total data points (excluding missing data)

    # dates: including all dates (4)
    tot_date = NA, # The total amount of instances collected for the <site parameter>
    no_flag_date = NA, # The total amount of instances that are not flagged for the <site parameter>
    no_flag_date_pct = NA, # no_flag_date/tot_date; percentage of dates not flagged out of total dates
    flag_date = NA, # The total amount of instances for the <site parameter flag>
    flag_date_pct = NA, # flag_date/tot_date; percent of dates flagged out of total dates

    # dates: excluding dates with missing data (as in data points with the values in `san_na_flags` in their flag columns) (5)
    tot_date_nona = NA, # The total amount of instances collected for the <site parameter> (excluding missing data)
    no_flag_date_nona = NA, # The total amount of instances that are not flagged for the <site parameter> (excluding missing data)
    no_flag_date_pct_nona = NA, # no_flag_date/tot_date; percentage of dates not flagged out of total dates (excluding missing data)
    flag_date_nona = NA, # The total amount of instances for the <site parameter flag> (excluding missing data)
    flag_date_pct_nona = NA # flag_date/tot_date_nona; percent of dates flagged out of total dates (excluding missing data)

  )

  # Fill in the data frame level information in the flag report ----

  flag_report_1 <- flag_report_structure %>%
    mutate(
      # metadata 1
      site = df_site,
      parameter = df_parameter,

      # data points (incl.) 2
      tot_data = nrow(df),
      no_flag_data = nrow(df %>% filter(is.na(flag))),
      no_flag_data_pct = format(round((no_flag_data/tot_data)*100, 2), nsmall=2),

      # data points (excl.) 3
      tot_data_nona = nrow(df %>% filter(!grepl(regex(sans_na_flags, multiline = TRUE), flag))),
      no_flag_data_nona = nrow(df %>%
                                 filter(!grepl(regex(sans_na_flags, multiline = TRUE), flag)) %>%
                                 filter(is.na(flag))),
      no_flag_data_pct_nona = format(round((no_flag_data_nona/tot_data_nona)*100, 2), nsmall=2),

      # dates (incl.) 4
      tot_date = nrow(df %>%
                        mutate(DT_round = format(DT_round, "%m-%d-%Y")) %>%
                        count(DT_round)),
      no_flag_date = nrow(df %>%
                            mutate(DT_round = format(DT_round, "%m-%d-%Y")) %>%
                            filter(is.na(flag)) %>%
                            count(DT_round)),
      no_flag_date_pct = format(round((no_flag_date/tot_date)*100, 2), nsmall=2),

      # dates (excl.) 5
      tot_date_nona = nrow(df %>%
                             filter(!grepl(regex(sans_na_flags, multiline = TRUE), flag)) %>%
                             mutate(DT_round = format(DT_round, "%m-%d-%Y")) %>%
                             count(DT_round)),
      no_flag_date_nona = nrow(df %>%
                                 filter(!grepl(regex(sans_na_flags, multiline = TRUE), flag)) %>%
                                 filter(is.na(flag)) %>%
                                 mutate(DT_round = format(DT_round, "%m-%d-%Y")) %>%
                                 count(DT_round)),
      no_flag_date_pct_nona = format(round((no_flag_date_nona/tot_date_nona)*100, 2), nsmall=2)
    )

  # Fill in the flag level information from each information for the flag report ----
  row_list <- list()
  for (i in list_of_flags) {
    # calculate the percentage values
    flag_report_2 <- flag_report_1 %>%
      mutate(
        # 1
        flag = i,
        # 2
        flag_data = nrow(df %>% filter(grepl(i, flag))),
        flag_data_pct = format(round((flag_data/tot_data)*100, 2), nsmall=2),
        # 3
        flag_data_nona = nrow(df %>%
                                filter(!grepl(regex(sans_na_flags, multiline = TRUE), flag)) %>%
                                filter(grepl(i, flag))),
        flag_data_pct_nona = format(round((flag_data_nona/tot_data_nona)*100, 2), nsmall=2),
        # 4
        flag_date = nrow(df %>%
                           filter(grepl(i, flag)) %>%
                           mutate(DT_round = format(DT_round, "%m-%d-%Y")) %>%
                           count(DT_round)),
        flag_date_pct = format(round((flag_date/tot_date)*100, 2), nsmall=2),
        # 5
        flag_date_nona = nrow(df %>%
                                filter(!grepl(regex(sans_na_flags, multiline = TRUE), flag)) %>%
                                filter(grepl(i, flag)) %>%
                                mutate(DT_round = format(DT_round, "%m-%d-%Y")) %>%
                                count(DT_round)),
        flag_date_pct_nona = format(round((flag_date_nona/tot_date_nona)*100, 2), nsmall=2)
      )
    # format the data
    flag_report_3 <- flag_report_2 %>%
      mutate(
        # data
        no_flag_data_pct = ifelse(no_flag_data_pct == "NaN" | no_flag_data_pct == "Inf", NA, no_flag_data_pct),
        flag_data_pct = ifelse(flag_data_pct == "NaN" | flag_data_pct == "Inf", NA, flag_data_pct),
        no_flag_data_pct_nona = ifelse(no_flag_data_pct_nona == "NaN" | no_flag_data_pct_nona == "Inf", NA, no_flag_data_pct_nona),
        flag_data_pct_nona = ifelse(flag_data_pct_nona == "NaN" | flag_data_pct_nona == "Inf", NA, flag_data_pct_nona),
        # dates
        no_flag_date_pct = ifelse(no_flag_date_pct == "NaN" | no_flag_date_pct == "Inf", NA, no_flag_date_pct),
        flag_date_pct = ifelse(flag_date_pct == "NaN" | flag_date_pct == "Inf", NA, flag_date_pct),
        no_flag_date_pct_nona = ifelse(no_flag_date_pct_nona == "NaN" | no_flag_date_pct_nona == "Inf", NA, no_flag_date_pct_nona),
        flag_date_pct_nona = ifelse(flag_date_pct_nona == "NaN" | flag_date_pct_nona == "Inf", NA, flag_date_pct_nona)
      )
    # put each row in a list
    row_list[[i]] <- flag_report_3
  }

  # Return the collated_rows df ----
  collated_rows <- bind_rows(row_list)

  return(collated_rows)
}

# generate the flag report
new_flag_report <-  map(head(flagged_data_dfs), generate_flag_report2) %>%
  bind_rows() %>%
  group_by(site, parameter) %>%
  arrange(.by_group = TRUE)

# write the flag report to csv
write_csv(new_flag_report, "data/flag_analysis/flag_report.csv")

# write the flag report to RDS
saveRDS(new_flag_report, "data/flag_analysis/flag_report.RDS")

