# libraries ----
library(targets)
library(tarchetypes)
library(tidyverse)
library(yaml)

# sourced functions ----
qaqc_functions <- list.files(path = "src/qaqc/download_and_flag_fxns", pattern = "\\.R$", full.names = T)
map(qaqc_functions, source)

# Use data from target `all_data_summary_stats_list` to test the unflagged data. ----

## The `all_data_summary_stats_list` object has incoming data that has been formatted
## and appended to the historical flagged data.
tar_make(all_data_summary_stats_list)
tar_load(all_data_summary_stats_list)

## Read in the look up tables for thresholds
tar_make(threshold_lookup)
tar_load(threshold_lookup)

## read in the sensor spec ranges
tar_make(sensor_spec_ranges)
tar_load(sensor_spec_ranges)

## ** This test was done using end_dt = 2023-11-29 14:26:54 MST in target incoming_data_csvs_upload **
## ** This test was done using a start_dt = 2023-11-28 12:00:00 in target incoming_data_csvs_upload **

# Remake flagging portion of tar_target(all_data_flagged) to test `add_suspect_flag()` ----
test_all_data_flagged <- map(all_data_summary_stats_list, function(data){
  data %>%
    add_field_flag() %>%
    add_spec_flag() %>%
    add_seasonal_flag() %>%
    add_na_flag() %>%
    add_repeat_flag()
    # we should also be incorporating the add_malfuntion_flag() here no?
})

# Explore the outputs of test_all_data_flagged ----
### Remove the data from future metadata parameters (Baro, External Voltage, Battery, Depth?)
metadata_params <- "Baro|Battery Level|External Voltage|Depth"
test_all_data_flagged_filtered <- test_all_data_flagged %>% discard(.p = str_detect(.,metadata_params))

### Set seed to randomly test data from test_all_data_flagged_filtered
set.seed(123)

# Start of pull ----
### select 3 random dfs from test_all_data_flagged_filtered list and take out what would
### be the equivalent of 6 hours of data from the top
start_samples <- sample(test_all_data_flagged_filtered, 3)

### filter out the data

### Write the data

## End of pull ----
### select 3 random dfs from test_all_data_flagged_filtered list and take out what would
### be the equivalent of 6 hours of data from the bottom
end_samples <- sample(test_all_data_flagged_filtered, 3)

## Random pulls ----
### select 3 random dfs from test_all_data_flagged_filtered list and take out what would
### be the equivalent of 6 hours of data randomly
