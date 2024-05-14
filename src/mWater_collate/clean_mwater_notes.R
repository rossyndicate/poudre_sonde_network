clean_mwater_notes <- function(){

  `%nin%` = Negate(`%in%`)

  # API Pull of mWater submitted notes

  # Grab API url from yml
  # Contact Sam Struthers if you need access
  creds = yaml::read_yaml("src/mWater_collate/mWater_API.yml")
  api_url = as.character(creds["url"])

  # Read in from API and tidy for downstream use

  # This is basic tidying of data set to:
  # correct datetime from UTC to Denver time (MST)
  # correct columns where Other input is allowed (Site, visit type, photos downloaded, sensor malfunction)
  # Add rounded date time


  all_notes_cleaned <- read_csv(url(api_url),show_col_types = FALSE) %>%
    mutate(
      # start and end dt comes in as UTC -> to MST
      start_DT = with_tz(parse_date_time(start_dt, orders = c("%Y%m%d %H:%M:%S", "%m%d%y %H:%M", "%m%d%Y %H:%M", "%b%d%y %H:%M" )), tz = "MST"),
      end_dt = with_tz(parse_date_time(end_dt, orders = c("%Y%m%d %H:%M:%S", "%m%d%y %H:%M", "%m%d%Y %H:%M", "%b%d%y %H:%M" )), tz = "MST"),
      malfunction_end_dt = with_tz(parse_date_time(malfunction_end_dt, orders = c("%Y%m%d %H:%M:%S", "%m%d%y %H:%M", "%m%d%Y %H:%M", "%b%d%y %H:%M" )), tz = "MST"),
      date = as.Date(start_DT, tz = "MST"),
      start_time_mst = format(start_DT, "%H:%M"),
      sensor_pulled = as.character(sn_removed),
      sensor_deployed = as.character(sn_deployed),
      # If other is chosen, make site == other response
      site = ifelse(site == "Other (please specify)", tolower(str_replace_all(site_other, " ", "")), site),
      # When I changed the mWater survey, I accidentally introduced ??? in the place of Sensor Calibration option, fixing that here
      visit_type = case_when(str_detect(visit_type, "\\?\\?\\?") ~ str_replace(string = visit_type,
                                                                               pattern =  "\\?\\?\\?",
                                                                               replacement = "Sensor Calibration or Check"),
                             TRUE ~ visit_type),
      # Merging visit_type and visit type other
      visit_type = case_when(str_detect(visit_type, "Other") ~ str_replace(string = visit_type,
                                                                           pattern =  "Other \\(please specify\\)",
                                                                           replacement = visit_type_other),
                             TRUE ~ visit_type),
      # Merge sensor malfunction and sensor malfunction other
      which_sensor_malfunction = case_when(str_detect(which_sensor_malfunction, "Other") ~ str_replace(string = which_sensor_malfunction,
                                                                                                       pattern =  "Other \\(please specify\\)",
                                                                                                       replacement = as.character(other_which_sensor_malfunction)),
                                           TRUE ~ which_sensor_malfunction),
      # If other is chosen, make photos downloaded equal to response
      photos_downloaded = ifelse(photos_downloaded == "Other (please specify)", photos_downloaded_other, photos_downloaded),
      # Rounded start date time
      DT_round = floor_date(start_DT, "15 minutes")) %>%
    # arrange by most recent visit
    arrange(DT_round)%>%
    # Remove other columns
    select(-c(photos_downloaded_other,visit_type_other, site_other, other_which_sensor_malfunction ))

  # Remove extra objects not needed other scripts

  # rm(creds, api_url)

  return(all_notes_cleaned)

}
