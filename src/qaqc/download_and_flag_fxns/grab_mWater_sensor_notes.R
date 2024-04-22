grab_mWater_sensor_notes <- function(){

  # API Pull of mWater submitted notes

  # Grab API url from yml
  # Contact Sam Struthers if you need access
  creds <- yaml::read_yaml("src/mWater_collate/mWater_API.yml")
  api_url <- as.character(creds["url"])

  # Read in from API and tidy for downstream use

  # This is basic tidying of data set to:
  # correct datetime from UTC to Denver time (always MST)
  # correct columns where Other input is allowed (Site, visit type, photos downloaded, sensor malfunction)
  # Add rounded date time

  all_notes_cleaned <- readr::read_csv(url(api_url), show_col_types = FALSE) %>%
    dplyr::mutate(
      # start and end dt comes in as UTC -> to MST
      start_DT = lubridate::with_tz(lubridate::parse_date_time(start_dt, orders = c("%Y%m%d %H:%M:%S", "%m%d%y %H:%M", "%m%d%Y %H:%M", "%b%d%y %H:%M")), tz = "MST"),
      end_dt = lubridate::with_tz(lubridate::parse_date_time(end_dt, orders = c("%Y%m%d %H:%M:%S", "%m%d%y %H:%M", "%m%d%Y %H:%M", "%b%d%y %H:%M" )), tz = "MST"),
      malfunction_end_dt = with_tz(lubridate::parse_date_time(malfunction_end_dt, orders = c("%Y%m%d %H:%M:%S", "%m%d%y %H:%M", "%m%d%Y %H:%M", "%b%d%y %H:%M" )), tz = "MST"),
      date = as.Date(start_DT, tz = "MST"),
      start_time_mst = format(start_DT, "%H:%M"),
      sensor_pulled = as.character(sn_removed),
      sensor_deployed = as.character(sn_deployed),
      # If other is chosen, make site == other response
      site = ifelse(site == "Other (please specify)", tolower(stringr::str_replace_all(site_other, " ", "")), site),
      # When I changed the mWater survey, I accidentally introduced ??? in the place of Sensor Calibration option, fixing that here
      visit_type = dplyr::case_when(stringr::str_detect(visit_type, "\\?\\?\\?") ~ stringr::str_replace(string = visit_type,
                                                                                                        pattern =  "\\?\\?\\?",
                                                                                                        replacement = "Sensor Calibration or Check"),
                                    TRUE ~ visit_type),
      # Merging visit_type and visit type other
      visit_type = dplyr::case_when(stringr::str_detect(visit_type, "Other") ~ stringr::str_replace(string = visit_type,
                                                                                                    pattern =  "Other \\(please specify\\)",
                                                                                                    replacement = visit_type_other),
                                    TRUE ~ visit_type),
      # Merge sensor malfunction and sensor malfunction other
      which_sensor_malfunction = dplyr::case_when(stringr::str_detect(which_sensor_malfunction, "Other") ~ stringr::str_replace(string = which_sensor_malfunction,
                                                                                                                                pattern =  "Other \\(please specify\\)",
                                                                                                                                replacement = as.character(other_which_sensor_malfunction)),
                                                  TRUE ~ which_sensor_malfunction),
      # If other is chosen, make photos downloaded equal to response
      photos_downloaded = ifelse(photos_downloaded == "Other (please specify)", photos_downloaded_other, photos_downloaded),
      # Rounded start date time
      DT_round = lubridate::floor_date(start_DT, "15 minutes")) %>%
    # arrange by most recent visit
    dplyr::arrange(DT_round) %>%
    # Remove other columns
    dplyr::select(-c(photos_downloaded_other, visit_type_other, site_other, other_which_sensor_malfunction))

  # Sensor Notes

  # These are the notes that will be added to the QAQC workflow notes Most of the code in this chunk is to get the df to
  # match the one in the QAQC workflow It can be saved as a CSV or pulled directly into QAQC workflow
  # grab only notes where technician is interacting with sensor on site (excludes sensor malfunction notes)

  mWater_field_notes <- all_notes_cleaned %>%
    dplyr::filter(grepl("Sensor",visit_type, ignore.case = TRUE) & !grepl("Sensor malfunction",visit_type, ignore.case = TRUE)) %>%
    # determining sonde employed status based on sensor_change
    dplyr::mutate(sonde_employed = dplyr::case_when(is.na(sensor_change)  ~ NA,
                                      sensor_change == "Swapped" ~ NA,
                                      sensor_change == "Pulled" ~ 1,
                                      sensor_change == "Deployed" ~ 0),
           #sensor_change %in% c("Swapped", "Deployed") ~ 1),

           # Sensor swapped notes
           sensor_swapped_notes = dplyr::case_when(is.na(sensor_change) ~ NA,
                                            sensor_change == "Pulled" & !is.na(sensor_pulled) ~ paste0("SN Removed: ", sensor_pulled),
                                            sensor_change == "Swapped" ~ paste0("SN Removed: ", sensor_pulled, " SN Deployed: ", sensor_deployed),
                                            sensor_change == "Deployed" ~ sensor_deployed),
           # Date/field season columns to match QAQC workflow
           DT_join = as.character(DT_round),
           field_season = lubridate::year(DT_round),
           last_site_visit = DT_round,
           date = as.character(date)) %>%
    dplyr::arrange(desc(DT_round))%>%
    # order columns in easily readable ordering
    dplyr::select(site, crew, DT_round, sonde_employed, sensors_cleaned, wiper_working, rdo_cap_condition, rdo_cap_replaced, ph_junction_replaced,
           cals_performed, cal_report_collected, sensor_malfunction, sensor_pulled, sensor_deployed, sensor_swapped_notes,
           visit_type, start_time_mst, DT_join, start_DT, end_dt,date,  visit_comments, photos_downloaded, field_season, last_site_visit)

  return(mWater_field_notes)

}

