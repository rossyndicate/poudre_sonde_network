# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
devtools::install_github("steeleb/HydroVuR")

# Set target options:
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

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = c(
  # to do (j): make sure that the functions pulled in are in correct {targets} format
  # api pull functions
  "src/api_pull/hv_getdata_id.R",
  "src/api_pull/hv_locations_all.R",
  "src/api_pull/get_start_dates_df.R",
  "src/api_pull/api_puller.R",
  # qaqc functions
  "src/qaqc/download_and_flag_fxns/"
  # "src/qaqc/explore_and_fix_fxns.R"
))

# to do (j): add in argument names (`name = ...`) for each target
list(
  # Pull in the API data -----------------------------------------------

  # accessing the API data
  tar_file_read(
    hv_creds,
    # to do (j): make sure that credentials are in a separate folder from scripts?
    "src/api_pull/credentials.yml",
    read = read_yaml(!!.x)
  ),

  # get a token for location lists and data access
  tar_target(
    hv_token,
    hv_auth(client_id = as.character(hv_creds["client"]),
            client_secret = as.character(hv_creds["secret"]))
  ),

  # get the start times for each site

  ## read in the previously flagged data
  tar_file_read(
    flagged_data_dfs,
    "data/flagged/all_data_flagged.RDS",
    read = readRDS(!!.x)
  ),

  ## get the start dates for each site
  tar_target(
    start_dates_df,
    get_start_dates_df(flagged_data_dfs)
  ),

  # get the data for each site
  # tar_target(
  #   incoming_data_csvs,
  #   {
  #     walk2(
  #       .x = start_dates_df$site, 
  #       .y = start_dates_df$last_DT_round,
  #       ~api_puller(site = .x, start_dt = .y, end_dt = Sys.time(), api_token = hv_token, dump_dir = "scratch/scratch_data/")
  #     ) 
  #   }
  # ),

  # append to historical data
  # to do (j): append to historical data

  # QAQC the data -----------------------------------------------------

  # load field notes
  tar_file_read(
    raw_field_notes,
    "data/sensor_field_notes.xlsx",
    read = read_excel(!!.x)
  ),

  # clean field notes
  tar_target(
    field_notes,
    clean_field_notes(raw_field_notes)
  ),

  # load incoming API data
    # to do (j): try to convert this into a tar_file_read() function
  tar_target(
    all_api_data,
    munge_api_data(api_path = "scratch/scratch_data/") # to do (j): make sure that this is the correct path
  ),

  # format data

  ## generate a site parameter combination list
  tar_target(
    site_param_combos,
    {
      sites <- unique(all_api_data$site)
      params <- c("Battery Level", "Baro", "Chl-a Fluorescence", 
        "Depth", "DO", "External Voltage", "ORP", 
        "pH", "Specific Conductivity", "Temperature", "Turbidity")
      site_param_combos <- crossing(sites, params)
    }
  ),

  ## summarize the data for each site parameter combination
  tar_target(
    all_data_summary_list,
    {
      field_notes
      all_data_summary_list <- map2(.x = site_param_combos$sites,
                                    .y = site_param_combos$params,
                                    ~summarize_site_param(site_arg = .x, 
                                                          parameter_arg = .y, 
                                                          api_data = all_api_data,
                                                          field_notes = field_notes)) %>%
        set_names(paste0(site_param_combos$sites, "_", site_param_combos$params)) %>%
        keep(~ !is.null(.))
    }
  ),

  # generate summary statistics for each site parameter combination
  tar_target(
    all_data_summary_stats_list,
    {
      all_data_summary_stats_list <- map(all_data_summary_list, generate_summary_statistics)
    }
  ),

  # read in look up table for thresholds
  tar_file_read(
    threshold_lookup,
    "data/summary_stats/threshold_lookup.RDS",
    read = readRDS(!!.x)
  ),

  tar_file_read(
    sensor_spec_ranges,
    "src/qaqc/sensor_spec_thresholds.yml",
    read = read_yaml(!!.x)
  ),

  # flag data
  tar_target(
    all_data_flagged,
    {
      # set sensor spec ranges as global variable
      sensor_spec_ranges <<- sensor_spec_ranges 
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
          kw_add_large_anomaly_flag() %>%
          mutate(mean_public = ifelse(is.na(flag), mean, NA)) %>%
          mutate(historical_data = TRUE)
      })
      # network check
      final_flag <- map(all_data_flagged, site_comp_test)

      all_data_flagged <- final_flag
    }
  )
)

