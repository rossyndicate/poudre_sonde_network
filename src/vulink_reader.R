# Pulling in raw data sets FROM THE FIELD, VIA VuLINK:
# file <- 'data/sensor_data/2022/elkhornupper/VuSitu_2022-08-27_14-20-32_Unnamed_Log_ElkhornUpper_20220827.htm'
vulink_reader <- function(file) {

  raw_data <- rvest::read_html(file) %>%
    rvest::html_node('table') %>%
    rvest::html_table() %>%
    dplyr::slice(-1:-32) %>%
    janitor::row_to_names(row_number = 1)

  names(raw_data) <- make.names(names(raw_data), unique = T)

  raw_data <- raw_data %>%
    select(DT = contains('Date.Time'),
           Water_Temp_C = contains('Temperature'),
           pH = contains('pH'),
           ORP_mV = contains('ORP'),
           Specific_Conductivity_µS_cm = contains('Specific.Conductivity') & contains('µS.cm.'),
           DO_ppm = contains('RDO') & contains('concentration'),
           Turbidity_NTU = contains('Turbidity')) %>%
    rename(Water_Temp_C = Water_Temp_C2,
           Air_Temp_C = Water_Temp_C1,
           pH = pH1,
           pH_mV = pH2) %>%
    filter(!is.na(DT))

  try(raw_data <- raw_data %>% select(-Water_Temp_C3),
      silent = TRUE)

  return(raw_data)

}

