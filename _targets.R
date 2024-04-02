# Architecture created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tidyverse")
)

# Run the R scripts in the R/ folder with your custom functions
# 'files` = the file and directory path to look for R scripts to run
tar_source(files = "src/qaqc/download_and_flag_fxns")
tar_source(files = "src/mWater_collate")
tar_source(files = "src/api_pull")

list(

  # Confirm incoming_api_data folder is empty, which indicates the last
  # iteration of the data pull + QAQC ran properly.
  tar_target(
    name = verify_incoming_data_dir,
    command = {
      check_incoming_api_dir(incoming_dir = "data/api/incoming_api_data/",
                             archive_dir = "data/api/archive_api_data/")
    }
    # , # commenting this out to test require in API_puller(this is a silent output)
    # cue = tar_cue(mode = "always"),
    # priority = 1
  ),

  # Pull in the API data ----
  # To access the API, you need credentials that must be formulated
  # like the example "src/api_pull/CopyYourCreds.yml" file. Contact Katie Willi
  # to request access.
  tar_file_read(
    name = hv_creds,
    "src/api_pull/credentials.yml",
    read = read_yaml(!!.x),
    packages = "yaml"
  ),

  # get a token for location lists and data access
  tar_target(
    name = hv_token,
    command = hv_auth(client_id = as.character(hv_creds["client"]),
                      client_secret = as.character(hv_creds["secret"]),
                      url = "https://www.hydrovu.com/public-api/oauth/token"),
    packages = c("httr2", "HydroVuR")
  ),

  # Download new data from API based on current QAQC data frame timestamps

  ## read in the historically flagged data...
  tar_file_read(
    name = flagged_data_dfs, # this data is from the RMD files. eventually it will be from this pipeline.
    "data/flagged/all_data_flagged.RDS",
    read = readRDS(!!.x)
  ),

  # Find the last DT that data was downloaded per each site. This will be the
  # start DT for the API pull:
  ## ... get the start dates per site based on that flagged data...
  tar_target(
    name = start_dates_df,
    command = get_start_dates_df(incoming_historically_flagged_data_list = flagged_data_dfs),
    packages = "tidyverse"
  ),

  # ... using those start dates, download new API data
  tar_target(
    name = incoming_data_csvs_upload, # this is going to have to append to the historical data
    command = {
      api_puller(site = start_dates_df$site,
                 start_dt = start_dates_df$DT_round,
                 end_dt = "2023-11-29 14:26:54 MST", # Sys.time(), # REPLACE TO Sys.time() ONCE PIPELINE INTEGRATED INTO FC
                 api_token = hv_token,
                 dump_dir = "data/api/incoming_api_data/",
                 require = verify_incoming_data_dir)
      },
    pattern = map(start_dates_df),
    packages = c("tidyverse", "HydroVuR", "httr2")
  ),


  # load xlsx field notes
  tar_file_read(
    name = old_raw_field_notes,
    "data/sensor_field_notes.xlsx",
    read = read_excel(!!.x),
    packages = "readxl"
  ),

  # clean xlsx field notes
  tar_target(
    name = old_tidy_field_notes,
    command = clean_field_notes(old_raw_field_notes),
    packages = "tidyverse"
  ),

  # grab mWater field notes
  tar_target(
    name = mWater_field_notes,
    command = grab_mWater_sensor_notes(),
    packages = "tidyverse"
  ),

  # bind .xlsx and mWater notes save to field notes for downstream use
  tar_target(
    name = field_notes,
    command = rbind(old_tidy_field_notes, mWater_field_notes)
  ),

  # Load incoming API data:
  tar_target(
    name = incoming_data_collated_csvs,
    command = munge_api_data(api_path = "data/api/incoming_api_data/",
                             require = incoming_data_csvs_upload),
    packages = c("tidyverse", "data.table")
  ),

  # Generate a site parameter combination list to iterate over:
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
    },
    packages = "tidyverse"

  ),

  # Link field notes to data stream, average observations if finer resolution
  # than 15 minutes (this is rare, but it does happen sometimes).
  # KATIE REQUEST: HERE IS WHERE WE SHOULD JOIN/CBIND BATTERY AND BARO DATA,
  # AND REMOVE FROM FUTURE STEPS
  tar_target(
    name = all_data_summary_list,
    command = {
      all_data_summary_list <- summarize_site_param(site_arg = site_param_combos$sites,
                                                    parameter_arg = site_param_combos$params,
                                                    api_data = incoming_data_collated_csvs,
                                                    notes = field_notes)
      },
    pattern = map(site_param_combos), # look up other pattern options, might be other things you can do here
    iteration = "list",
    packages = c("tidyverse", "padr")
  ),

  # Set the names of the indexes of the summarized incoming data
  tar_target(
    name = summarized_incoming_data,
    command = {
      summarized_incoming_data <- all_data_summary_list %>%
        set_names(paste0(site_param_combos$sites, "-", site_param_combos$params)) %>%
        keep(~ !is.null(.))
    },
    packages = c("tidyverse")
  ),

  # Get the last 24 hours of the historically flagged data and append it to the incoming data.
  # Necessary for some of the rolling statistics we develop for flagging.
  tar_target(
    name = combined_data, # API data chunk to process (to do (j): rename this)
    command = combine_hist_inc_data(incoming_data_list = summarized_incoming_data,
                                    historical_data_list = flagged_data_dfs),
    packages = "tidyverse",
    iteration = "list"
  ),

  # Generate summary statistics for each site parameter combination, such as
  # rolling average, slope between observations, etc.
  tar_target(
    name = all_data_summary_stats_list,
    command = {
      all_data_summary_stats_list <- generate_summary_statistics(combined_data)
      },
    pattern = map(combined_data),
    iteration = "list",
    packages = c("tidyverse", "RcppRoll")
  ),

  # read in look up tables for thresholds ----
  # Load in our static season-based thresholds:
  tar_file_read(
    name = threshold_lookup,
    "src/qaqc/seasonal_thresholds.csv",
    read = read_csv(!!.x),
    packages = c("tidyverse")
  ),

  # Load in the static sensor spec thresholds:
  tar_file_read(
    name = sensor_spec_ranges,
    "src/qaqc/sensor_spec_thresholds.yml",
    read = read_yaml(!!.x),
    packages = "yaml"
  ),

  # Flag the data using our pre-developed and static thresholds:
  tar_target(
    name = all_data_flagged,
    command = {
      # set sensor spec ranges as global variable
      sensor_spec_ranges <<- sensor_spec_ranges # to do (j): again, why do we need to call these objects here?
      # set threshold lookup as global variable
      threshold_lookup <<- threshold_lookup

      # first pass of flags
      all_data_flagged <- flag_all_data(data = all_data_summary_stats_list,
                                        require = c(sensor_spec_ranges,
                                                    threshold_lookup))

      # might need to require sensor spec ranges and threshold lookup

      },
    pattern = map(all_data_summary_stats_list),
    iteration = "list",
    packages = c("tidyverse", "yaml")
  ),

  tar_target(
    name = checked_flagged_data,
    command = {
      # network check
      checked_flagged_data <- network_check(all_data_flagged)
    },
    pattern = map(all_data_flagged),
    iteration = "list",
    packages = c("tidyverse")
  ),

  # Set the names of the indexes of the network checked flagged data
  tar_target(
    name = indexed_checked_flagged_data,
    command = {
      indexed_checked_flagged_data <- set_names(checked_flagged_data, names(combined_data))
    },
    iteration = "list",
    packages = c("tidyverse")
    ),

  # update the historically flagged data ---- #***


  # update the historically flagged data by appending the new data with the
  # old:
  tar_target(
    name = update_historical_flag_data,
    command = {
      update_historical_flag_data <- update_historical_flag_list(
        new_flagged_data = indexed_checked_flagged_data,
        historical_flagged_data = flagged_data_dfs
      )
    }
  ),

  # save the updated flagged data
  # KW: WE ARE STORING THIS UPDATED FLAGGED DATA AS TEST_ALL_DATA_FLAGGED
  # SO THAT ALL_DATA_FLAGGED DOESN'T GET OVERWRITTEN. ONCE WE ARE DONE
  # TESTING THE PIPELINE, THIS WILL BE SWAPPED TO "ALL_DATA_FLAGGED.RDS" TO UPDATE IT.
  tar_target(
    name = write_flagged_data_RDS,
    command = saveRDS(update_historical_flag_data, "data/flagged/test_all_data_flagged.RDS")
  ),

  # connect to FC system

  # update FC system

  #  append incoming data to the historical API data and remove data from incoming data folder ---- #***
  # tar_target(
  #   append_inc_hist_api_data,
  #   append_historical_api_data(
  #     hist_dir = "data/api/historical_api_data/",
  #     inc_dir = "data/api/incoming_api_data/"
  #   )
  # )

  # clear out the incoming API data folder and move those files to an archive folder.
  tar_target(
    name = empty_incoming_data_dir,
    command = {
      clear_incoming_data_dir(incoming_dir = "data/api/incoming_api_data/",
                              archive_dir = "data/api/archive_api_data/",
                              require = write_flagged_data_RDS)
    }
  )
)
