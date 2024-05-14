#' @title Clean field notes
#' 
#' @description
#' A function that cleans the field notes excel file. This function adds datetime
#' columns to the field notes dataframe and filters out field notes where the sensor
#' was not handled.
#' 
#' @param raw_field_notes A dataframe with the raw field notes.
#' 
#' @return A dataframe with the field notes.
#' 
#' @examples
#' clean_field_notes(field_note_path = "data/sensor_field_notes.xlsx")

clean_field_notes <- function(raw_field_notes){

  field_notes <- raw_field_notes%>%
    mutate(start_DT = ymd_hm(paste(date, start_time_mst), tz = "MST"))%>%
    mutate(#start_DT = with_tz(start_DT, tzone = "MST"),
      DT_round = floor_date(start_DT, "15 minutes"),
      DT_join = as.character(DT_round),
      site = tolower(site),
      field_season = year(DT_round),
      last_site_visit = DT_round) %>%
    arrange(site, DT_round) %>%
    # `sonde_employed` determines if the sonde is deployed or not. 0 = sonde deployed, 1 = sonde is not deployed
    mutate(sonde_employed = case_when(!is.na(sensor_pulled) & !is.na(sensor_deployed) ~ 0,
                                      !is.na(sensor_pulled) & is.na(sensor_deployed) ~ 1,
                                      is.na(sensor_pulled) & !is.na(sensor_deployed) ~ 0,
                                      is.na(sensor_pulled) & is.na(sensor_deployed) ~ NA),
           end_dt  = as.POSIXct(NA, tz = "MST")) %>%
    # remove field dates where sensor was not handled:
    filter(grepl("Sensor Cleaning or Check|Sensor Calibration", visit_type, ignore.case = TRUE))

  return(field_notes)

}

