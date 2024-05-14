grab_mWater_malfunction_notes <- function(mWater_api_data){

  # Grab notes about sensor malfunction
  malfunction_records <- mWater_api_data %>%
    filter(grepl("Sensor malfunction", visit_type, ignore.case = TRUE)) %>%
    select(start_DT, site, crew, which_sensor_malfunction, malfunction_end_dt, notes = visit_comments)

  #write to csv
  write_csv(malfunction_records, "data/mWater_malfunction_records.csv")

  parameters <- c("Battery Level",
                  "Baro",
                  "Chl-a Fluorescence",
                  "Depth",
                  "DO",
                  "External Voltage",
                  "ORP",
                  "pH",
                  "Specific Conductivity",
                  "Temperature",
                  "Turbidity")

  malfunction_records <- malfunction_records %>%
    # keep records relevant to {target} analysis
    select(start_DT, end_DT = malfunction_end_dt, site, parameter = which_sensor_malfunction) %>%
    # match the text in the sensor column to the text in the target analysis
    separate_rows(parameter, sep = ", ") %>%
    mutate(
      parameter = case_when(
      parameter == "Chlorophyll a" ~ "Chl-a Fluorescence",
      parameter == "RDO" ~ "DO",
      parameter == "Conductivity" ~ "Specific Conductivity",
      .default = parameter
    ),
    site = case_when(
      site == "riverbluffs" ~ "river bluffs",
      .default = site
    )) %>%
    filter((is.na(parameter)) | (parameter %in% parameters))

  return(malfunction_records)

}
