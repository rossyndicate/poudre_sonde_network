# generate_flag_report <- function(df) {
#
#   # Extract the site and parameter from the df_name
#   site <- unique(na.omit(df$site))
#   parameter <- unique(na.omit(df$parameter))
#
#   list_of_flags <- c("sonde not employed",
#                      "site visit",
#                      "sv window",
#                      "sensor specification range",
#                      "seasonal range",
#                      "slope violation",
#                      "outside sd range",
#                      "repeated value",
#                      "missing data",
#                      "24hr anomaly",
#                      "anomaly window")
# # check these
#   sans_na_flags <- "^(missing data|
#                       sonde not employed;\\nmissing data|
#                       missing data;\nanomaly window|
#                       sonde not employed;\nmissing data;\nanomaly window|
#                       missing data;\n24hr anomaly;\nanomaly window|
#                       site visit;\nmissing data;\n24hr anomaly;\nanomaly window|
#                       sv window;\nmissing data;\n24hr anomaly;\nanomaly window)$"
#
#   # summarize total data points
#   total_observations <- df %>%
#     summarise(n_total = n_distinct(DT_round)) %>%
#     pull(n_total)
#
#   # summarize total data points sans missing data
#   total_observations_1 <- df %>%
#     # filter out when flag has only missing data or only sonde not employed and missing data
#     filter(!str_detect(flag, sans_na_flags)) %>%
#     summarise(n_total = n_distinct(DT_round)) %>%
#     pull(n_total)
#
#   # summarize total days
#   total_observations_dates <- df %>%
#     group_by(date = format(DT_round, "%m-%d-%Y")) %>%
#     summarize(n_total = nrow(date)) %>%
#     nrow()
#
#   # summarize total days sans missing data
#   total_observations_dates_1 <- df %>%
#     filter(!str_detect(flag, sans_na_flags)) %>%
#     group_by(date = format(DT_round, "%m-%d-%Y")) %>%
#     summarize(n_total = nrow(date)) %>%
#     nrow()
#
#   row_list <- list()
#   for (i in list_of_flags) {
#
#     # summarize flagged data points
#     flagged_observations <- df %>%
#       filter(str_detect(flag, i)) %>%
#       summarise(n_flag = n_distinct(DT_round)) %>%
#       pull(n_flag)
#     # summarize flagged data points
#     flagged_observations_1 <- df %>%
#       filter(!str_detect(flag, sans_na_flags)) %>%
#       filter(str_detect(flag, i)) %>%
#       summarise(n_flag = n_distinct(DT_round)) %>%
#       pull(n_flag)
#     # summarize percent data points that are flagged
#     percent_flagged <- flagged_observations/total_observations
#     # summarize percent data points that are flagged sans missing data
#     percent_flagged_1 <- flagged_observations_1/total_observations_1
#
#     # summarize flagged days
#     flagged_observations_dates <- df %>%
#       filter(str_detect(flag, i)) %>%
#       group_by(date = format(DT_round, "%m-%d-%Y")) %>%
#       summarize(n_total = nrow(date)) %>%
#       nrow()
#     # summarize flagged days
#     flagged_observations_dates_1 <- df %>%
#       filter(!str_detect(flag, sans_na_flags)) %>%
#       filter(str_detect(flag, i)) %>%
#       group_by(date = format(DT_round, "%m-%d-%Y")) %>%
#       summarize(n_total = nrow(date)) %>%
#       nrow()
#     # summarize percent days that are flagged
#     percent_flagged_dates <- flagged_observations_dates/total_observations_dates
#     # summarize percent days that are flagged
#     percent_flagged_dates_1 <- flagged_observations_dates_1/total_observations_dates_1
#
#     # creating a row with the information
#     calculated_values <- tibble(
#       # metadata
#       site = site,
#       parameter = parameter,
#       flag = i,
#       # data points
#       data_points_flagged_percentage = format(round(percent_flagged * 100, 2), nsmall = 2),
#       data_points_flagged = flagged_observations,
#       total_data_points = total_observations,
#       data_points_flagged_percentage_sans_na = format(round(percent_flagged_1 * 100, 2), nsmall = 2),
#       data_points_flagged_sans_na = flagged_observations_1,
#       total_data_points_sans_na = total_observations_1,
#       # dates
#       dates_flagged_percentage = format(round(percent_flagged_dates * 100, 2), nsmall=2),
#       dates_flagged = flagged_observations_dates,
#       total_dates = total_observations_dates,
#       dates_flagged_percentage_sans_na = format(round(percent_flagged_dates_1 * 100, 2), nsmall=2),
#       dates_flagged_sans_na = flagged_observations_dates_1,
#       total_dates_sans_na = total_observations_dates_1
#       )
#
#     row_list[[i]] <- calculated_values
#   }
#
#   #calculated_df <- bind_cols(row_list)
#   return(bind_rows(row_list))
#
# }
#
# flag_report <- map(all_data_flagged, generate_flag_report) %>%
#   bind_rows()
#
# View(flag_report)
#
# # write_csv(flag_report, "data/flag_report.csv")
#
# flag_report_1 <- flag_report %>%
#   group_by(site, parameter) %>%
#   arrange(desc(data_points_flagged_percentage), .by_group = TRUE)
#
# View(flag_report_1)
#
#
#
# text <- unique(all_data_flagged$`archery-Actual Conductivity`$flag)
# !str_detect(x, "^(missing data|sonde not employed;\\nmissing data)$")
#
# print(unique(all_data_flagged$`archery-Actual Conductivity`$flag)[grepl("missing data", unique(all_data_flagged$`archery-Actual Conductivity`$flag))])
