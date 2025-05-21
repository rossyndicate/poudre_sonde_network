#' @title current_sensor_locations
#' @description This function looks through all the calibration reports, grabs the most recent and extracts the useful info.
#'  It organizes the data and returns a dataframe with the each SNs current location, sensor type, owner and sonde.
#'  Sensors no
#'@example
#' source("src/current_sensor_locations.R")
#' current_sensor_locations()
current_sensor_locations <- function(){
  library(openxlsx)
  library(readxl)

  source("src/files_missing.R")
  files_missing()

  cat("If there are out of date calibration reports, please update and re-run")

  `%nin%` <- Negate(`%in%`)
# read in sensor ownership sheet
all_sensors <- readxl::read_xlsx(here("data", "metadata", "2025_sensor_tracking.xlsx"), sheet = "ownership")%>%
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
short_file_path <- str_remove(string = html_file, pattern = here( "data", "calibration_reports/"))

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
file_path <- here("data", "metadata", "2025_sensor_tracking.xlsx")

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
