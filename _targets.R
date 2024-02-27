# Architecture created by use_targets().

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# git # what is this doing here?
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
  # Make sure directory is structured correctly ----
  # For now we will use the data directory structure that we have been using, but
  # this will need to be updated with the directory structure in the fc system.

  ## Incoming API data and related API data archive folder ----
  tar_target(
    name = verify_incoming_data_dir,
    command = {
      check_incoming_api_dir(incoming_dir = "data/api/incoming_api_data/",
                             archive_dir = "data/api/archive_api_data/")
      },
    cue = tar_cue(mode = "always"),
    priority = 1
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


  ## ... get the start dates per site based on that flagged data...
  tar_target(
    name = start_dates_df,
    command = get_start_dates_df(incoming_flagged_data_dfs = flagged_data_dfs),
    packages = "tidyverse"
  ),

  # ... using those start dates, download new API data
  tar_target(
    name = incoming_data_csvs_upload, # this is going to have to append to the historical data
    command = walk2(.x = start_dates_df$site,
                    .y = start_dates_df$start_DT_round,
                    ~api_puller(site = .x, start_dt = .y, end_dt = "2023-11-29 14:26:54 MST", # Sys.time(), # REPLACE TO Sys.time() ONCE PIPELINE INTEGRATED INTO FC
                                api_token = hv_token, dump_dir = "data/api/incoming_api_data/")),
    packages = c("tidyverse", "HydroVuR", "httr2")
  ),

  # QAQC the data

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

  # load incoming API data
  # to do (j): try to convert this into a tar_file_read() function
  tar_target(
    name = incoming_data_collated_csvs,
    command =  {
      incoming_data_csvs_upload
      munge_api_data(api_path = "data/api/incoming_api_data/")
      },
    packages = "tidyverse"
  ),

  # format data

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
    },
    packages = "tidyverse"

  ),

  ## summarize the data for each site parameter combination
  # KATIE REQUEST: HERE IS WHERE WE SHOULD JOIN/CBIND BATTERY AND BARO DATA,
  # AND REMOVE FROM FUTURE STEPS
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
    command = combine_hist_inc_data(incoming_data_list = all_data_summary_list,
                                    historical_data_list = flagged_data_dfs),
    packages = "tidyverse"
  ),

  # generate summary statistics for each site parameter combination ----
  tar_target(
    name = all_data_summary_stats_list,
    #command =   {
    #all_data_summary_stats_list <-
    command = combined_data %>% map(~generate_summary_statistics(.)),
    #},
    packages = c("tidyverse", "RcppRoll")
  ),

  # read in look up table for thresholds ----
  tar_file_read(
    name = threshold_lookup,
    "src/qaqc/seasonal_thresholds.csv",
    read = read_csv(!!.x),
    packages = c("tidyverse")
  ),

  tar_file_read(
    name = sensor_spec_ranges,
    "src/qaqc/sensor_spec_thresholds.yml",
    read = read_yaml(!!.x),
    packages = "yaml"
  ),

  # flag data ----
  tar_target(
    name = all_data_flagged,
    command = {
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
          mutate(mean_public = ifelse(is.na(flag), mean, NA)) #%>%
        #mutate(historical_flagged_data_1 = TRUE)
      })
      # network check
      final_flag <- all_data_flagged %>%
        map(~network_check(.))

      all_data_flagged <- final_flag
    }
  ),

  # update the historically flagged data ----
  tar_target(
    name = update_historical_flag_data,
    command = {
      update_historical_flag_data <- update_historical_flag_list(
        new_flagged_data = all_data_flagged,
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

  #  append incoming data to the historical API data and remove data from incoming data folder ----
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
      write_flagged_data_RDS
      clear_incoming_data_dir(incoming_dir = "data/api/incoming_api_data/",
                              archive_dir = "data/api/archive_api_data/")
    }
  )
)





