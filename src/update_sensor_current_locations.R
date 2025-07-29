#' @title update_sensor_current_locations
#' @description This function looks through all the calibration reports, grabs the most recent and extracts the useful info.
#'  It organizes the data and returns a dataframe with the each SNs current location, sensor type, owner and sonde.
#' @param sonde_tracking_file_path A string filepath to the current file where sonde info is tracked. This is an .xslx file with the sheet "station_info" where we will update the sonde numbers,
#' and the sheet "ownership" where we will look for sensors that are not to be included in the current sensor deployments.
#' @param field_notes A dataframe containing field notes with columns: site, crew, start_DT,end_dt, cal_report_collected, cals_performed, log_downloaded, log1_type,log1_mmdd,  log2_type, log2_mmdd
#'@example
#' source("src/current_sensor_locations.R")
#' current_sensor_locations()
update_sensor_current_locations <- function(field_notes, sonde_tracking_file_path){

#check for missing files before proceeding
  source("src/files_missing.R")
  file_check <- files_missing(field_notes,sonde_tracking_file_path, logical = T)

  if(file_check == T){
    stop("`files_missing()` has found missing calibration reports or logs. Please upload all calibration reports and logs before re-running")
  }

  `%nin%` <- Negate(`%in%`)
# read in sensor ownership sheet
all_sensors <- readxl::read_xlsx(here(sonde_tracking_file_path), sheet = "ownership")%>%
  #remove sensors given back to CU and YSI chla sensors
  filter(is.na(archive) & Equipment != "YSI chla sensor")%>%
  mutate(SN = as.numeric(SN))

#get current field season
field_season <- year(Sys.Date())

#get all files in calibration report folder
calibration_reports <- list.files(here("data", "calibration_reports"), full.names = T)%>%
  # filter to current field season
  .[grepl(field_season, .)]

#grab the most recent calibration report for each site
most_recent_cals <- tibble(path = calibration_reports)%>%
  mutate(
    filename = basename(path),
    site = str_extract(filename, "^[^_]+"),
    datetime_str = str_extract(filename, "(?<=_)[0-9]{8}_[0-9]{4}"),
    datetime = ymd_hm(datetime_str)
  ) %>%
  group_by(site) %>%
  slice_max(order_by = datetime, n = 1, with_ties = FALSE) %>%
  ungroup()%>%
  filter(!is.na(datetime))

# function to actually parse cal report and grab the info we need
read_cal_report <- function(html_file){

raw <- rvest::read_html(html_file) %>%
  rvest::html_nodes('body')%>%
  rvest::html_nodes('table')%>%
  #turn into tibble
  rvest::html_table(fill = TRUE)%>%
  bind_rows()

parsed <- raw %>%
  filter(X1 %in% c("Serial Number", "Last Calibrated", "Sensor", "Instrument"))%>%
  mutate(
    sensor_port = cumsum(X1 == "Sensor")  # new group each time we see "Sensor"
  ) %>%
  pivot_wider(id_cols = sensor_port, names_from = X1, values_from = X2)%>%
  select(sensor_port, sonde = Instrument, sn = `Serial Number`, last_calibrated = `Last Calibrated`, sensor = Sensor)%>%
  mutate(sn = as.numeric(sn))%>%
  filter(sensor %nin% c("Barometric Pressure","Pressure"  ))

sonde_sn <- parsed %>%
  filter(sensor_port == 0)%>%
  pull(sn)

#get sonde site from file name, first string before _
short_file_path <- basename(html_file)

parts <- strsplit(short_file_path, "_")[[1]]
#grab site
site <- parts[1]
#grab deployment date from filename
cal_date <- parts[2]

#create final table from calibration reports
summary <- parsed %>%
  filter(sensor_port != 0)%>%
  select(-sonde, -sensor_port)%>%
  mutate(
    last_calibrated = as.Date(last_calibrated, format = "%m/%d/%Y"),
    owner = all_sensors$Owner[match(sn, all_sensors$SN)],
    site = site,
    sonde_sn = sonde_sn,
    cal_date = as_date(cal_date, format = "%Y%m%d")
  )

return(summary)

}
#read in the calibration reports and extract the summary for each report
current_status <- map(most_recent_cals$path, read_cal_report)%>%
  bind_rows()

#find all sensor sns in all_sensors that are not in current status
lab_sns <- all_sensors%>%
  filter(!grepl("AT|Wiper|Cable|vulink",Equipment, ignore.case = T))%>%
  filter(SN %nin% current_status$sn)%>%
  select(sn = SN, sensor = Equipment, owner = Owner)%>%
  mutate(
    site = "Lab",
    sonde_sn = NA,
    cal_date = NA,
    last_calibrated = NA
  )

final_status <- bind_rows(current_status, lab_sns)%>%
  arrange(site, sensor)


# Define file path
file_path <- here(sonde_tracking_file_path)

# Load the workbook
wb <- loadWorkbook(file_path)

# Remove old 'sensor_cur_loc' sheet if it exists
if ("sensor_cur_loc" %in% names(wb)) {
  removeWorksheet(wb, "sensor_cur_loc")
}

# Add updated 'sensor_cur_loc' data
addWorksheet(wb, "sensor_cur_loc")
writeData(wb, sheet = "sensor_cur_loc", x = final_status)

# Save workbook (overwrites file)
saveWorkbook(wb, file = file_path, overwrite = TRUE)

}
