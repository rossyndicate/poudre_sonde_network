# The objective of this script is to prepare Virridy 2024 sensor data for manual
# calibration verification app.
#
# Runs back-calibration pipeline on archery_virridy, cottonwood_virridy, and
# riverbend_virridy, then injects results into the app tracking RDS.
#
# Prerequisites:
# - ross.wq.tools reinstalled from with the updates from the dev branch (Virridy filter fix applied)
# - Calibration HTML files present in data/raw/sensor/calibration_reports/
#
# NOTE/WARNING: This is already ran and if it is run again the virridy data will
# duplicate the calibration app's virridy tracking data. This data's calibration
# decisions have already been completed and re-running this script will definitely
# cause some tracking issues downstream. For this reason the section of this script
# that writes the data to the tracking file is commented out. Do not run section
# 9 of this script for testing as it is written.

library(tidyverse)
library(lubridate)
library(here)
library(arrow)
library(groupdata2)
library(ross.wq.tools)

# Paths ====

# This is the path for the manually flagged data
virridy_parquet_dir <- here::here(
  "data/raw/sensor/manual_data_verification/2024_cycle/in_progress/verified_directory"
)

# This is the path of the tracking file for the manual calibration verification app
tracking_rds <- here::here(
  "data/raw/sensor/manual_data_verification/complete_dataset/calibrated_sensor_field_data_tracking.rds"
)

# Parameters with calibration HTML files for Virridy sondes
cal_params <- c("Chl-a Fluorescence", "FDOM Fluorescence", "Specific Conductivity", "Turbidity")

# Step 1: Extract calibration data (Virridy now included after filter fix in ross.wq.tools) ====
calibration_data <- cal_extract_markup_data(
  field_cal_dir = here::here("data", "raw", "sensor", "calibration_reports")
)

# Step 2: Read Virridy parquet files ====
virridy_files <- list.files(virridy_parquet_dir, pattern = "\\.parquet$", full.names = TRUE)
virridy_files <- virridy_files[grepl("virridy", basename(virridy_files), ignore.case = TRUE)]

virridy_raw <- map_dfr(virridy_files, read_parquet)

# Retain field-note columns from parquets for later join
field_note_cols <- virridy_raw %>%
  select(DT_round, site, parameter, mal_flag, sonde_moved, sonde_employed, last_site_visit)

# Step 3: Filter to calibratable parameters and organize into nested list ====
# Structure: [[year]][[site-param]]
virridy_sensor_list <- list(
  `2024` = virridy_raw %>%
    filter(parameter %in% cal_params) %>%
    select(DT_round, site, parameter, mean) %>%
    split(f = list(.$site, .$parameter), sep = "-", drop = TRUE) %>%
    discard(~nrow(.) == 0)
)

# Step 4: Join calibration coefficients to sensor data ====
joined_data <- cal_join_sensor_calibration_data(
  sensor_data_list = virridy_sensor_list,
  calibration_data_list = calibration_data
)

# Step 5: Prepare calibration windows ====
prepped_windows <- cal_prepare_calibration_windows(joined_data)

# Step 6: Back-calibrate each chunk ====
# Default obs_column = "mean" produces: mean, mean_raw, mean_lm_trans, mean_cal, cal_check
back_calibrated <- prepped_windows %>%
  map(function(year) {
    year %>%
      map(function(site_param) {
        site_param %>%
          map_dfr(function(chunk) {
            cal_back_calibrate(chunk)
          })
      })
  })

# Step 7: Rename columns to match app expectations ====
# App expects: mean_cleaned, mean_cleaned_raw, mean_lm_trans, mean_cleaned_cal, cal_check
# Note: mean_lm_trans keeps its name (cal_lin_trans_lm always uses the first
# word of raw_col as its prefix, so "mean_raw" -> "mean_lm_trans" regardless)
virridy_cal_renamed <- back_calibrated[["2024"]] %>%
  map(function(df) {
    df %>%
      rename(
        mean_cleaned     = mean,
        mean_cleaned_raw = mean_raw,
        mean_cleaned_cal = mean_cal
      )
  })

# Step 8: Join field note columns and add NA columns not available for Virridy ====
virridy_final <- virridy_cal_renamed %>%
  map(function(df) {
    df %>%
      left_join(field_note_cols, by = c("DT_round", "site", "parameter")) %>%
      mutate(
        visit_comments     = NA_character_,
        cals_performed     = NA_character_,
        sensor_malfunction = NA_character_
      )
  })

# Step 9: Inject into tracking RDS ====
# tracking_data <- read_rds(tracking_rds)
# tracking_data[["2024"]] <- c(tracking_data[["2024"]], virridy_final)
# write_rds(tracking_data, tracking_rds)
