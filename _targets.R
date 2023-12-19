# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline: ----
library(targets)
library(tarchetypes)
devtools::install_github("steeleb/HydroVuR")

# Set target options: ----
tar_option_set(
  # need to make sure that we are using all of th
  packages = c("data.table", "tidyverse", "rvest",
  "readxl", "lubridate", "zoo",
  "padr","plotly", "feather",
  "RcppRoll", "yaml", "ggpubr",
  "profvis", "janitor", "HydroVuR") # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format.
    # should this be parquet?
)

# Run the R scripts in the R/ folder with your custom functions: ----
tar_source(files = c(
  # api pull functions
  "src/api_pull/hv_getdata_id.R",
  "src/api_pull/hv_locations_all.R",
  "src/api_pull/get_start_dates_df.R",
  "src/api_pull/api_puller.R",
  # qaqc functions
  "src/qaqc/download_and_flag_fxns/",
  "src/mWater_collate/clean_mwater_notes.R",
  "src/mWater_collate/grab_sensor_notes.R"
  # "src/qaqc/explore_and_fix_fxns.R"
))

list(
  # Pull in the API data -----------------------------------------------

  # accessing the API data ----
  tar_file_read(
    name = hv_creds,
    # to do (j): make sure that credentials are in a separate folder from scripts?
    "src/api_pull/credentials.yml",
    read = read_yaml(!!.x),
    packages = "yaml"
  ),

  # get a token for location lists and data access ----
  tar_target(
    name = hv_token,
    command = hv_auth(client_id = as.character(hv_creds["client"]),
                      client_secret = as.character(hv_creds["secret"])),
    packages = "httr2"
  ),

  # get the start times for each site ----

  ## read in the historically flagged data
  tar_file_read(
    name = flagged_data_dfs, # this data is from the RMD files. eventually it will be from this pipeline.
    "data/flagged/all_data_flagged.RDS",
    read = readRDS(!!.x)
  ),

  ## get the start dates for each site
  tar_target(
    name = start_dates_df,
    command = get_start_dates_df(incoming_flagged_data_dfs = flagged_data_dfs),
    packages = "tidyverse"
  ),

  # get the data for each site ----
  tar_target(
    name = incoming_data_csvs_upload, # this is going to have to append to the historical data
    command = walk2(.x = start_dates_df$site,
                    .y = start_dates_df$start_DT_round,
                    ~api_puller(site = .x, start_dt = .y, api_token = hv_token,
                                dump_dir = "data/api/incoming_api_data/")),
    packages = c("tidyverse", "HydroVuR", "httr2")
  ),

  # {
  #   end_dt = Sys.time()
  #   walk2(
  #     .x = start_dates_df$site,
  #     .y = start_dates_df$start_DT_round,
  #     ~api_puller(site = .x, start_dt = .y, api_token = hv_token, end_dt = Sys.time(), dump_dir = "data/api/incoming_api_data/")
  #   )
  # }
  # ),


  # QAQC the data -----------------------------------------------------

  # load xlsx field notes ----
  tar_file_read(
    name = raw_field_notes,
    "data/sensor_field_notes.xlsx",
    read = read_excel(!!.x),
    packages = "readxl"
  ),

  # clean xlsx field notes ----
  tar_target(
    name = old_field_notes,
    command = clean_field_notes(raw_field_notes),
    packages = "tidyverse"
  ),


  #Grab mWater field notes
  tar_target(
    name = mWater_field_notes,
    command = grab_mWater_sensor_notes(),
    packages = "tidyverse"),

  #bind .xlsx and mWater notes save to field notes for downstream use
  tar_target(
    name = field_notes,
    command = rbind(old_field_notes, mWater_field_notes)
  ),

  # load incoming API data ----
  # to do (j): try to convert this into a tar_file_read() function
  tar_target(
    name = incoming_data_collated_csvs,
    command =  munge_api_data(api_path = "data/api/incoming_api_data/"),
    packages = "tidyverse"
  ),

  # format data  ----

  ## generate a site parameter combination list
  tar_target(
    name = site_param_combos,
    command = {
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
    }
  ),

  ## summarize the data for each site parameter combination
  tar_target(
    name = all_data_summary_list, # to do (j): name this something else
    command = {
      # field_notes
      map2(.x = site_param_combos$sites,
           .y = site_param_combos$params,
           ~summarize_site_param(site_arg = .x,
                                 parameter_arg = .y,
                                 api_data = incoming_data_collated_csvs,
                                 notes = field_notes)) %>% # to do (j): why do I need to call field_notes here? I don't think we should need to do that...
        set_names(paste0(site_param_combos$sites, "-", site_param_combos$params)) %>%
        keep(~ !is.null(.))
    },
    packages = c("tidyverse", "padr")
  ),

  # append to chunk of historical flagged data  ----

  ## get the last 3 hours of the historically flagged data and append it to the incoming data
  tar_target(
    name = combined_data, # API data chunk to process (to do (j): rename this)
    command = combine_hist_inc_data(incoming_data_list = all_data_summary_list, historical_data_list = flagged_data_dfs),
    packages = "tidyverse"
  ),

  # generate summary statistics for each site parameter combination ----
  tar_target(
    name = all_data_summary_stats_list,
   command =   {
      all_data_summary_stats_list <- map(combined_data, generate_summary_statistics)
    },
   packages = c("tidyverse", "RcppRoll")
  ),

  # read in look up table for thresholds ----
  tar_file_read(
    name = threshold_lookup,
    "data/summary_stats/threshold_lookup.RDS",
    read = readRDS(!!.x)
  ),

  tar_file_read(
    sensor_spec_ranges,
    "src/qaqc/sensor_spec_thresholds.yml",
    read = read_yaml(!!.x)
  ),

  # flag data ----
  tar_target(
    all_data_flagged,
    {
      # set sensor spec ranges as global variable
      sensor_spec_ranges <<- sensor_spec_ranges # to do (j): again, why do we need to call these objects here?
      # set threshold lookup as global variable
      threshold_lookup <<- threshold_lookup

      # first pass of flags
      all_data_flagged <- map(all_data_summary_stats_list, function(data) {
        data %>%
          add_field_flag() %>%
          add_spec_flag() %>%
          add_seasonal_flag() %>%
          add_na_flag() %>%
          add_repeat_flag() %>%
          add_suspect_flag() %>%
          # we should also be incorporating the add_malfuntion_flag() here no?
          mutate(mean_public = ifelse(is.na(flag), mean, NA)) %>%
          mutate(historical_flagged_data_1 = TRUE)
      })
      # network check
      final_flag <- map(all_data_flagged, site_comp_test) # to do (j): I want to rename site_comp_test to network_check.

      all_data_flagged <- final_flag
    }
  ),

  # update the historically flagged data ----
  tar_target(
    update_historical_flag_data,
    {
      update_historical_flag_data <- update_historical_flag_list(
        new_flagged_data = all_data_flagged,
        historical_flagged_data = flagged_data_dfs
      )
    }
  ),

  # save the updated flagged data ----
  tar_target(
    write_flagged_data_RDS,
    saveRDS(update_historical_flag_data, "data/flagged/test_all_data_flagged.RDS")
  ),

  # connect to FC system

  # update FC system

  #  append incoming data to the historical API data and remove data from incoming data folder ----
  # tar_target(
  #   append_inc_hist_api_data,
  #   append_historical_api_data(
  #     hist_dir = "data/api/historical_api_data/",
  #     inc_dir = "data/api/incoming_api_data/"
  #   )
  # )
)

