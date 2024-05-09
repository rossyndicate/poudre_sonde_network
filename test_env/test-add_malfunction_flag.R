library(testthat)
library(tidyverse)

add_malfunction_flag <- function(df, malfunction_records){

  df_site <- unique(df$site)
  df_parameter <- unique(df$parameter)

  malfunction_records_filtered <- malfunction_records %>%
    filter(site == df_site) %>%
    filter(is.na(parameter) | parameter == df_parameter) %>%
    mutate(end_DT = ifelse(is.na(end_DT), ymd_hms("9999-12-31 23:59:59", tz = "MST"), end_DT)) %>%
    mutate(end_DT = as.POSIXct(end_DT, tz = "MST"))

  if (nrow(malfunction_records_filtered > 0)) {
    malfunction_interval_list <- map2(
      .x = malfunction_records_filtered$start_DT,
      .y = malfunction_records_filtered$end_DT,
      .f = ~interval(.x, .y, tz = "MST"))

    df <- df %>%
      add_flag(DT_round %within% malfunction_interval_list, "sensor malfunction")
  }

  return(df)

}

malfunction_records <- grab_mwater_malfunction_records(mWater_notes_cleaned)

sample_data <- tibble(
  DT_round = seq(
    from = ymd_hms("2022-06-01 00:00:00", tz = "MST"),
    to = ymd_hms("2024-04-01 00:00:00", tz = "MST"),
    by = "15 min"
  ),
  site = "archery",
  parameter = "DO",
  flag = NA
)

sample_data_result <- add_malfunction_flag(df = sample_data, malfunction_records = malfunction_records)

malfunction_records_filtered <- malfunction_records %>%
  filter(site == "archery") %>%
  filter(is.na(parameter) | parameter == "DO") %>%
  mutate(end_DT = ifelse(is.na(end_DT), ymd_hms("9999-12-31 23:59:59", tz = "MST"), end_DT)) %>%
  mutate(end_DT = as.POSIXct(end_DT, tz = "MST"))
