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
  "src/api_pull/api_puller.R"
  # qaqc functions
  # "src/qaqc/download_and_flag_fxns/"
  # "src/qaqc/explore_and_fix_fxns.R"
))

# to do (j): add in argument names (`name = ...`) for each target
list(
  # Pull in the API data -----------------------------------------------

  # accessing the API data
  tar_target(
    hv_creds,
    # to do (j): make sure that credentials are in a separate folder from scripts?
    read_yaml("src/api_pull/credentials.yml")
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
  # this is fully not working right now
  tar_target(
    incoming_data_csvs,
    walk2(start_dates_df,
    ~api_puller(site = .$site,
      start = .$last_DT_round))
  )

  # append to historical data
  # to do (j): append to historical data

  # QAQC the data -----------------------------------------------------

)
