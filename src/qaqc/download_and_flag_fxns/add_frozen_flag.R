add_frozen_flag <- function(df){

  # create a df of temperature for each site
  temperature <- df %>%
    data.table::data.table() %>%
    dplyr::select(DT_round, DT_join, parameter, mean) %>%
    dplyr::filter(parameter == "Temperature") %>%
    dplyr::select(DT_join, Temperature = mean)

    # add "temperature" column to df:
    temperature_checked <- df %>%
      dplyr::left_join(., temperature, by = "DT_join") %>%
      # If water temperature is freezing, flag all parameters
      add_flag(., Temperature <= 0 & !grepl("frozen", flag), "frozen") %>%
      # remove the temp column so df is identical in structure to OG df
      dplyr::select(-Temperature)

    return(temperature_checked)

}
