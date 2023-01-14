# Situational data downloading processes (for when data isn't  on HydroVu for whatever reason, not common)

# From AquaTROLL 500/600, in the field:
troll_reader <- function(file) {

  raw_data <- rvest::read_html(file) %>%
    rvest::html_node('table') %>%
    rvest::html_table() %>%
    slice(-1:-23) %>%
    janitor::row_to_names(row_number = 1)

  raw_data_bad <- rvest::read_html(file) %>%
    rvest::html_node('table') %>%
    rvest::html_table() %>%
    slice(-1:-25) %>%
    janitor::row_to_names(row_number = 1)

  if (isTRUE(names(raw_data)[1] == "Date Time")) {
    return(raw_data)} else {
      return(raw_data_bad)}
}

# From VuLink, in the field:
vulink_reader <- function(file) {

  raw_data <- rvest::read_html(file) %>%
    rvest::html_node('table') %>%
    rvest::html_table() %>%
    slice(-1:-31) %>%
    janitor::row_to_names(row_number = 1)
}

# TROLL from HydroVu, on the cloud:
hydrovu_reader <- function(file) {
  raw_data <- rvest::read_html(file) %>%
    rvest::html_node('table') %>%
    rvest::html_table() %>%
    slice(-1:-8) %>%
    janitor::row_to_names(row_number = 1)
}

# Function to pull data from the VuLink itself from HydroVu (includes sensor temperature, baro, and power levels):
tube_reader <- function(file) {
  raw_data <- rvest::read_html(file) %>%
    rvest::html_node('table') %>%
    rvest::html_table() %>%
    slice(-1:-8) %>%
    janitor::row_to_names(row_number = 1)
}



# Function that binds all datasets together, harmonizes column names, and removes extraneous columns:
going_rawless <- function(site_name, trolled) {

  # Downloading VuLink data:
  raw_tube <- map_dfr(grep(list.files(paste0("data/sensor_data/2022/", site_name, "/"), full.names = T),
                           pattern = "VuLink", invert = F, value = T, ignore.case = F), tube_reader) %>%
    select(DT = 1,
           PVC_Temp = 2,
           Battery = 3,
           Air_Baro = 4) %>%
    filter(!is.na(PVC_Temp)) %>%
    mutate_at(vars(2:ncol(.)), as.numeric) %>%
    mutate(DT = ymd_hms(DT)) %>%
    mutate(DT = DT - lubridate::hours(7)) %>%
    mutate(DT = as.character(round_date(ymd_hms(DT), "15 minutes"))) %>%
    mutate(DT = ymd_hms(DT))

  # Downloading HydroVu data:
  raw <- map_dfr(grep(list.files(paste0("data/sensor_data/2022/", site_name, "/"), full.names = T),
                      pattern = "TROLL", invert = F, value = T, ignore.case = F), hydrovu_reader)
  names(raw) <- make.names(names(raw), unique = T)

  rawless <- raw %>%
    select(DT_instrument = contains('Date.Time'),
           Water_Temp_C = as.numeric(contains('Temperature..C')),
           pH = contains('pH'),
           ORP_mV = contains('ORP'),
           Specific_Conductivity_µS_cm = contains('Specific.Conductivity..µS.cm.'),
           DO_ppm = contains('DO..mg'),
           Turbidity_NTU = contains('Turbidity'),
           Depth_ft = contains('Depth..ft')) %>%
    mutate(DT_instrument = ymd_hms(DT_instrument)) %>%
    mutate(DT_instrument = DT_instrument - lubridate::hours(7))

  try(rawless <- raw %>%
        select(DT_instrument = contains('Date.Time'),
               Water_Temp_C = as.numeric(contains('Temperature..C')),
               pH = contains('pH'),
               ORP_mV = contains('ORP'),
               Specific_Conductivity_µS_cm = contains('Specific.Conductivity..µS.cm.'),
               DO_ppm = contains('DO..mg'),
               Chla = contains('Chl.a'),
               Turbidity_NTU = contains('Turbidity'),
               Depth_ft = contains('Depth..ft')) %>%
        mutate(DT_instrument = ymd_hms(DT_instrument)) %>%
        mutate(DT_instrument = DT_instrument - lubridate::hours(7)))


  # If raw troll data exists, incorporate it into the data set. All of the TROLL datasets
  # are different, so it is important to tidy them individually. :-(
  try(rawless <- rawless %>%
        rbind(trolled))

  rawless <- rawless %>%
    mutate_at(vars(2:ncol(.)), as.numeric) %>%
    mutate(site = site_name) %>%
    arrange(ymd_hms(DT_instrument)) %>%
    mutate(DT = as.character(round_date(ymd_hms(DT_instrument), "15 minutes"))) %>%
    mutate(DT = ymd_hms(DT)) %>%
    distinct(.keep_all = TRUE) %>%
    # Join information about the telemetry device (like temperature, % battery):
    full_join(raw_tube, by = 'DT') %>%
    # Join pre-2022 dataset
    dplyr::bind_rows(., filter(pre_2022, site == site_name)) %>%
    # Link up field notes:
    full_join(filter(field_notes, site == site_name), by = c('DT','site')) %>%
    mutate(date = as_date((DT)),
           hour = hour(DT),
           year = year(DT),
           month = month(DT)) %>%
    arrange(ymd_hms(DT))

  return(rawless)
}
