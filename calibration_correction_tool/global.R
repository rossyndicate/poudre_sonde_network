# global.R
library(shiny)
library(ross.wq.tools)
library(DT)

#Installing and loading all packages
invisible(
  lapply(c(
    "tidyverse", # Data manipulation
    "janitor", # Clean dirty data
    "lubridate", # Date-Time Manipulation
    "rvest", # HTML Retrieval and Manipulation
    "readxl", # Reading excel files
    "here", # Easy, cross platform file referencing
    "ggplot2", # Plotting libraries
    "ggpubr",
    "plotly",
    "devtools", # For downloading GitHub packages
    "remotes",
    "yaml",
    "arrow", # reading parquet files
    "groupdata2"
  ),
  function(x) {
    if (x %in% installed.packages()) {
      suppressMessages({
        library(x, character.only = TRUE)
      })
    } else {
      suppressMessages({
        install.packages(x)
        library(x, character.only = TRUE)
      })
    }
  })
)

# Load data once when app starts

# Define file paths for tracking system
data_dir <- here::here("data", "raw", "sensor", "manual_data_verification", "2025_cycle","hydro_vu_pull","back_calibration")
tracking_file <- file.path(data_dir,"bellvue_calibrated_tracking_sensor_data_2025.rds")
finalized_file <- file.path(data_dir, "bellvue_calibrated_finalized_sensor_data_2025.rds")
original_file <- here::here("data", "raw", "sensor", "manual_data_verification",
                            "2025_cycle","hydro_vu_pull","back_calibration",
                            "bellvue_calibrated_sensor_data_2025.rds")

# Initialize tracking data (unverified calibrations)
if (file.exists(tracking_file)) {
  calibrated_data_tracking <- readr::read_rds(tracking_file)
} else {
  # First time running - copy original to tracking
  calibrated_data_tracking <- readr::read_rds(original_file)
  readr::write_rds(calibrated_data_tracking, tracking_file)
}

# Initialize finalized data (verified calibrations)
if (file.exists(finalized_file)) {
  final_calibrated_data <- readr::read_rds(finalized_file)
} else {
  # First time running - create empty nested list structure matching tracking data
  final_calibrated_data <- lapply(calibrated_data_tracking, function(year_data) {
    list()  # Empty list for each year
  })
  readr::write_rds(final_calibrated_data, finalized_file)
}

# Read in the sensor specific calibration data
sensor_calibration_data <- readr::read_rds(here::here("data", "collated", "sensor", "cal_reports",
                                                      "sensor_calibration_data.RDS"))

ross.wq.tools::load_calibration_data(
  cal_data_file_path = here::here("data", "collated", "sensor", "cal_reports",
                                  "munged_calibration_data.RDS")
)

# Read in field notes
field_notes_data <- ross.wq.tools::load_mWater()%>%
  mutate(DT_round = floor_date(start_dt, unit = "15 minutes"))%>%
  filter(str_detect(visit_type, "Sensor Calibration, Cleaning or Check"))%>%
  pivot_longer(cols = contains(c("post", "pre" )),
               names_to = "col",
               values_to = "value")%>%
  select(site, DT_round, col, value)%>%
  mutate(parameter = str_split(col, "_", simplify = TRUE)[,1],
         parameter_clean = case_when(
           parameter == "rdo" ~ "DO",
           parameter == "cond" ~ "Specific Conductivity",
           parameter == "turb" ~ "Turbidity",
           parameter == "fdom" ~ "FDOM Fluorescence",
           parameter == "chla" ~ "Chl-a Fluorescence",
           parameter == "orp" ~ "ORP",
           parameter == "ph" ~ "pH",
           TRUE ~ parameter
         ),
         type = str_split(col, paste0(parameter, "_"), simplify = T)[,2])

# Source helper functions
source(here::here("R", "generate_final_df.R"))
source(here::here("R", "cal_plot.R"))
source(here::here("R", "final_calibration_plot.R"))
source(here::here("R", "update_backend.R"))
