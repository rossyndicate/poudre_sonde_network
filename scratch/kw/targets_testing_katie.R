library(tidyverse)
library(targets)
library(tarchetypes)
# For testing:
# source("src/api_pull/hv_getdata_id.R")
# source("src/api_pull/hv_locations_all.R")
# source("src/api_pull/get_start_dates_df.R")
# source("src/api_pull/api_puller.R")
# # qa/qc functions
# list.files("src/qaqc/download_and_flag_fxns/", full.names = TRUE) %>% walk(~source(.))
# source("src/mWater_collate/clean_mwater_notes.R")
# source("src/mWater_collate/grab_sensor_notes.R")

tar_source(files = "src/qaqc/download_and_flag_fxns")
#tar_source(files = "src/mWater_collate")
tar_source(files = "src/api_pull")

check_incoming_api_dir(incoming_dir = "data/api/incoming_api_data/",
                       archive_dir = "data/api/archive_api_data/")

library(yaml)
hv_creds <- read_yaml("src/api_pull/credentials.yml")

library(httr2);library(HydroVuR)
hv_token <- hv_auth(client_id = as.character(hv_creds["client"]),
                    client_secret = as.character(hv_creds["secret"]))

flagged_data_dfs<- readRDS("data/flagged/all_data_flagged.RDS")

start_dates_df <- get_start_dates_df(incoming_historically_flagged_data_list = flagged_data_dfs)

library(httr2);library(HydroVuR)
incoming_data_csvs_upload <-  walk2(.x = start_dates_df$site,
                                    .y = start_dates_df$DT_round,
                                    ~api_puller(site = .x, start_dt = .y, end_dt = "2023-11-28 15:00:00 MST",
                                                api_token = hv_token, dump_dir = "data/api/incoming_api_data/",
                                                require = NULL))
library(readxl)
old_raw_field_notes <- read_excel("data/sensor_field_notes.xlsx")
api_puller()
old_tidy_field_notes <- clean_field_notes(old_raw_field_notes)

mWater_field_notes <- grab_mWater_sensor_notes()

field_notes <- rbind(old_tidy_field_notes, mWater_field_notes)

saveRDS(field_notes, 'data/clean_field_notes.RDS')
library(data)
incoming_data_collated_csvs <- munge_api_data(api_path = "data/api/incoming_api_data/")

# as a single target:
sites <- unique(incoming_data_collated_csvs$site)
params <- c("Battery Level",
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
site_param_combos <- crossing(sites, params)
#

library(padr)
# THIS IS WHERE WE SHOULD CBIND/JOIN BARO + BATTERY DATA TO OTHER PARAMETERS
all_data_summary_list <- map2(.x = site_param_combos$sites,
                              .y = site_param_combos$params,
                              ~summarize_site_param(site_arg = .x,
                                                    parameter_arg = .y,
                                                    api_data = incoming_data_collated_csvs,
                                                    notes = field_notes)) %>%
  set_names(paste0(site_param_combos$sites, "-", site_param_combos$params)) %>%
  keep(~ !is.null(.))


combined_data <- combine_hist_inc_data(incoming_data_list = all_data_summary_list,
                                       historical_data_list = flagged_data_dfs)

library(RcppRoll)
all_data_summary_stats_list <- combined_data %>%
  map(~generate_summary_statistics(.))

threshold_lookup <- read_csv("src/qaqc/seasonal_thresholds.csv")

library(yaml)
sensor_spec_ranges <- read_yaml("src/qaqc/sensor_spec_thresholds.yml")

all_data_flagged <- map(all_data_summary_stats_list, function(data) {
  data %>%
    add_field_flag() %>%
    add_spec_flag() %>%
    add_seasonal_flag() %>%
    add_na_flag() %>%
    add_repeat_flag() %>%
    add_suspect_flag() %>%
    add_sensor_malfunction() %>%
    # we should also be incorporating the add_malfunction_flag() here no?
    mutate(mean_public = ifelse(is.na(flag), mean, NA))
})

final_flag <- all_data_flagged %>%
  map(~network_check(.)) # to do (j): I want to rename site_comp_test to network_check.

all_data_flagged <- final_flag

update_historical_flag_data <- update_historical_flag_list(new_flagged_data = all_data_flagged,
                                                           historical_flagged_data = flagged_data_dfs)

