# loading packages
package_loader <- function(x) {
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
}

invisible(
  lapply(c("arrow",
           "data.table",
           "httr2",
           "tidyverse",
           "lubridate",
           "zoo",
           "padr",
           "stats",
           "RcppRoll",
           "yaml",
           "here",
           #"fcw.qaqc",
           "furrr"
  ),
  package_loader)
)

#manually load in fcw_qaqc functions
walk(list.files('fcw_QAQC_func/', pattern = "*.R", full.names = TRUE, recursive = TRUE), source)

# Set up parallel processing
num_workers <- min(availableCores() - 1, 4) # Use at most 4 workers
plan(multisession, workers = num_workers)
furrr_options(
  globals = TRUE,
  packages = c("arrow", "data.table", "httr2", "tidyverse", "lubridate", "zoo",
               "padr", "stats", "RcppRoll", "yaml", "here", "fcw.qaqc")
)

# suppress scientific notation to ensure consistent formatting
options(scipen = 999)

# make a site fixing function to save space and increase readability
fix_sites <- function(df) {
  fixed_df <- df %>%
    mutate(site = tolower(site)) %>%
    # renaming all the sites, just in case
    mutate(site = case_when(
      grepl("tamasag", site, ignore.case = TRUE) ~ str_replace(site, "tamasag", "bellvue"),
      grepl("legacy", site, ignore.case = TRUE) ~ str_replace(site, "legacy", "salyer"),
      grepl("lincoln", site, ignore.case = TRUE) ~ str_replace(site, "lincoln", "udall"),
      grepl("timberline", site, ignore.case = TRUE) ~ str_replace(site, "timberline", "riverbend"),
      grepl("prospect", site, ignore.case = TRUE) ~ str_replace(site, "prospect", "cottonwood"),
      grepl("boxelder", site, ignore.case = TRUE) ~ str_replace(site, "boxelder", "elc"),
      grepl("archery", site, ignore.case = TRUE) ~ str_replace(site, "archery", "archery"),
      grepl("river bluffs", site, ignore.case = TRUE) ~ str_replace(site, "river bluffs", "riverbluffs"),
      TRUE ~ site)
    )
  return(fixed_df)
}

# Establishing directory paths.
staging_directory <- here("data","sharing","quarterly_meetings","2025_Q2", "raw_data")
flagged_directory <- here("data","sharing","quarterly_meetings","2025_Q2", "flagged_data")

# Read in the threshold data first
sensor_thresholds <- read_yaml(here("data","manual_data_verification","2024_cycle", "hydro_vu_pull", "thresholds", "sensor_spec_thresholds.yml"))
season_thresholds <- read_csv(here("data","manual_data_verification","2024_cycle", "hydro_vu_pull", "thresholds", "outdated_seasonal_thresholds.csv"), show_col_types = FALSE) %>%
  fix_sites()

# Read in credentials
mWater_creds <- read_yaml(here("creds", "mWaterCreds.yml"))

hv_creds <- read_yaml(here("creds", "HydroVuCreds.yml"))
hv_token <- hv_auth(client_id = as.character(hv_creds["client"]),
                    client_secret = as.character(hv_creds["secret"]))

# Pulling in the data from mWater
mWater_data <- load_mWater(creds = mWater_creds)
all_field_notes <- grab_mWater_sensor_notes(mWater_api_data = mWater_data)%>%
  mutate(DT_round = with_tz(DT_round, tzone = "UTC"),
         last_site_visit = with_tz(last_site_visit, tzone = "UTC"),
         DT_join = as.character(DT_round))
sensor_malfunction_notes <- grab_mWater_malfunction_notes(mWater_api_data = mWater_data)%>%
  mutate(start_DT = with_tz(start_DT, tzone = "UTC"),
         end_DT = with_tz(end_DT, tzone = "UTC"))

# Pulling in the data from hydrovu
# Making the list of sites that we need
hv_sites <- hv_locations_all(hv_token) %>%
  filter(!grepl("vulink", name, ignore.case = TRUE))

mst_start <- ymd_hms("2025-06-30 00:00:00", tz = "America/Denver")
mst_end <- ymd_hms("2025-08-04 23:59:59", tz = "America/Denver")

# Upload the hv data
sites <- c("archery",
           "bellvue",
           "boxelder",
           "cottonwood",
           "elc",
           "riverbluffs",
           "salyer",
           "udall",
           "riverbend")


walk(sites,
     function(site) {
       message("Requesting HV data for: ", site)
       api_puller(
         site = site,
         network = "all",
         start_dt = with_tz(mst_start, tzone = "UTC"),
         end_dt = with_tz(mst_end, tzone = "UTC"),
         api_token = hv_token,
         #hv_sites_arg = hv_sites,
         dump_dir = staging_directory
         #synapse_env = FALSE,
         #fs = NULL
       )
     }
)


# Load in all the raw files
# Since the munge api data is different, we are going to do this with a "custom munge"
# (this is basically the regular munge, we are just not removing the virridy sites)
hv_data <- list.files(staging_directory, full.names = TRUE) %>%
  future_map_dfr(function(file_path){
    site_df <- read_parquet(file_path, as_data_frame = TRUE)
    return(site_df)
  }, .progress = TRUE)

data_2025 <- hv_data %>%
  data.table() %>%
  select(-id) %>%
  mutate(units = as.character(units)) %>%
  filter(!grepl("vulink", name, ignore.case = TRUE)) %>%
  filter(!grepl("virridy", site, ignore.case = TRUE)) %>%
  mutate(
    DT = timestamp,
    DT_round = round_date(DT, "15 minutes"),
    DT_join = as.character(DT_round),
    site = tolower(site),
    site = ifelse(grepl("virridy", name, ignore.case = TRUE), str_replace(site, " virridy", "_virridy"), site)
  ) %>%
  select(-name) %>%
  fix_sites() %>%
  distinct(.keep_all = TRUE) %>%
  split(f = list(.$site, .$parameter), sep = "-") %>%
  keep(~nrow(.) > 0)

# Tidy all the raw files
tidy_data <- data_2025 %>%
  future_map(~tidy_api_data(api_data = .), .progress = TRUE) %>%  # the summarize interval default is 15 minutes
  keep(~!is.null(.))

# Add the field note data to all of the data
# Quick name fix for mountain campus
all_field_notes <- all_field_notes %>%
  fix_sites() %>%
  mutate(site = ifelse(site == "mountaincampus", "mtncampus", site))

combined_data <- tidy_data %>%
  future_map(~add_field_notes(df = ., notes = all_field_notes), .progress = TRUE)

# Add summary statistics
summarized_data <- combined_data %>%
  map(~generate_summary_statistics(.))

# Chunk the data for furrr
summarized_data_chunks <- split(1:length(summarized_data),
                                ceiling(seq_along(1:length(summarized_data))/10))
# Flag data...
# Single parameter flags
single_sensor_flags <- list()
for (chunk_idx in seq_along(summarized_data_chunks)) {
  message("\n=== Processing chunk ", chunk_idx, " of ", length(summarized_data_chunks), " ===")

  # Get the indices for this chunk
  indices <- summarized_data_chunks[[chunk_idx]]
  chunk_data <- summarized_data[indices]

  # Process the chunk in parallel
  chunk_results <- chunk_data %>%
    future_map(
      function(data) {
        flagged_data <- data %>%
          data.table(.) %>%
          # flag field visits
          add_field_flag(df = .) %>%
          # flag missing data
          add_na_flag(df = .) %>%
          # flag DO noise
          find_do_noise(df = .) %>%
          # flag repeating values
          add_repeat_flag(df = .) %>%
          # find times when sonde was moved up/down in housing
          add_depth_shift_flag(df = ., level_shift_table =  all_field_notes, post2024 = TRUE) %>%
          # find instances of sensor drift (FDOM, Chl-a, Turbidity only)
          add_drift_flag(df = .)

        if (unique(data$parameter) %in% names(sensor_thresholds)) {
          # flag instances outside the spec range
          flagged_data <- flagged_data %>%
            data.table(.) %>%
            add_spec_flag(df = ., spec_table = sensor_thresholds)
        }

        if (unique(data$parameter) %in% unique(season_thresholds$parameter)) {
          # flag instances outside the spec range
          flagged_data <- flagged_data %>%
            data.table(.) %>%
            add_seasonal_flag(df = ., threshold_table = season_thresholds)
        }

        flagged_data <- flagged_data %>%
          data.table(.)

        return(flagged_data)
      },
      .progress = TRUE
    )

  # Add chunk to list
  single_sensor_flags <- c(single_sensor_flags, chunk_results)

  if (chunk_idx < length(summarized_data_chunks)) {
    message("Taking a short break before next chunk...")
    gc()
    Sys.sleep(0.1)
  }
}

# Intrasensor flags
intrasensor_flags <- single_sensor_flags %>%
  rbindlist(fill = TRUE) %>%
  split(by = "site")

# Chunk the data for furrr
intrasensor_data_chunks <- split(1:length(intrasensor_flags),
                                 ceiling(seq_along(1:length(intrasensor_flags))/3))

intrasensor_flags_list <- list()
for (chunk_idx in seq_along(intrasensor_data_chunks)) {
  message("\n=== Processing chunk ", chunk_idx, " of ", length(intrasensor_data_chunks), " ===")

  # Get the indices for this chunk
  indices <- intrasensor_data_chunks[[chunk_idx]]
  chunk_data <- intrasensor_flags[indices]
  # Process the chunk in parallel
  chunk_results <- chunk_data %>%
    map(
      function(data) {
        # A chunk is a site df
        flagged_data <- data %>%
          data.table() %>%
          # flag times when water was below freezing
          add_frozen_flag(.) %>%
          # overflagging correction. remove slope violation flag if it occurs concurrently
          # with temp or depth
          intersensor_check(.) %>%
          # add sonde burial. If DO is noise is long-term, likely burial:
          add_burial_flag(.) %>%
          # flag times when sonde was unsubmerged
          add_unsubmerged_flag(.)

        return(flagged_data)
      }, .progress = TRUE
    ) %>%
    rbindlist(fill = TRUE) %>%
    # lil' cleanup of flag column contents
    dplyr::mutate(flag = ifelse(flag == "", NA, flag)) %>%
    # transform back to site-parameter dfs
    split(f = list(.$site, .$parameter), sep = "-") %>%
    purrr::discard(~ nrow(.) == 0) %>%
    # Add in KNOWN instances of sensor malfunction
    map(~add_malfunction_flag(df = ., malfunction_records = sensor_malfunction_notes))

  # Add chunk to list
  intrasensor_flags_list <- c(intrasensor_flags_list, chunk_results)

  if (chunk_idx < length(intrasensor_data_chunks)) {
    message("Taking a short break before next chunk...")
    gc()
    Sys.sleep(0.1)
  }
}
# Let's temporarily save this data so i can remove everything else
iwalk(intrasensor_flags_list, ~write_csv(.x, here("data","sharing","quarterly_meetings", "2025_Q2", "flagged_data_temp", paste0(.y, ".csv"))))

# Because we are pulling in all of the data for all of the sites, and the
# network check to do that is not applicable, here we make a custom net work check function
custom_network_check <- function(df, intrasensor_flags_arg = intrasensor_flags_list) {

  site_name <- unique(na.omit(df$site))
  parameter_name <- unique(na.omit(df$parameter))

  sites_order <-  c("joei",
                    "cbri",
                    "chd",
                    "pfal",
                    "pbd",
                    "bellvue",
                    "salyer",
                    "udall",
                    "riverbend",
                    "cottonwood",
                    "elc",
                    "archery",
                    "riverbluffs")

  if (site_name %in% c("penn", "sfm", "lbea")){
    sites_order <- c("penn", "sfm", "lbea")
  }

  if(site_name == "springcreek"){
    sites_order <- c("riverbend_virridy",
                     "springcreek",
                     "cottonwood_virridy")
  }

  if(site_name == "boxcreek"){
    sites_order <- c("elc",
                     "boxcreek",
                     "archery_virridy")
  }
  # missing sites: mtncampus, pbr, pman
  if (site_name %in% c("mtncampus", "pbr", "pman")){
    return(df)
  }

  # exlcude virridy sites
  if (site_name %in% c("riverbend_virridy","cottonwood_virridy","timberline_virridy", "prospect_virridy", "elc", "archery_virridy")){
    return(df)
  }

  # Find the index of current site in ordered list
  site_index <- which(sites_order == sites_order[grep(site_name, sites_order, ignore.case = TRUE)])

  # Create site-parameter identifier
  site_param <- paste0(site_name, "-", parameter_name)

  # Initialize empty dataframes for upstream/downstream sites
  upstr_site_df <- tibble(DT_round = NA)
  dnstr_site_df <- tibble(DT_round = NA)

  # Try to get upstream site data
  tryCatch({
    # Skip trying to find upstream sites for first sites
    if (site_index != 1){
      previous_site <- paste0(sites_order[site_index-1], "-", parameter_name)
      upstr_site_df <- intrasensor_flags_arg[[previous_site]] %>%
        select(DT_round, site_up = site, flag_up = flag) %>%
        data.table()
    }
  },
  error = function(err) {
    message(paste0("No UPSTREAM data found for ", site_param, ". Expected at site '", previous_site, "'."))
  })

  # Try to get downstream site data
  tryCatch({
    # Skip trying to find downstream sites for first sites
    if (site_index != length(sites_order)){
      next_site <- paste0(sites_order[site_index+1], "-", parameter_name)
      dnstr_site_df <- intrasensor_flags_arg[[next_site]] %>%
        select(DT_round, site_down = site, flag_down = flag) %>%
        data.table()
    }
  },
  error = function(err) {
    message(paste0("No DOWNSTREAM data found for ", site_param, ". Expected at site '", next_site, "'."))
  })

  # Join current site data with upstream and downstream data
  up_down_join <- df %>%
    left_join(upstr_site_df, by = "DT_round") %>%
    left_join(dnstr_site_df, by = "DT_round")

  # Helper functions
  # Function to add column if it doesn't exist
  add_column_if_not_exists <- function(df, column_name, default_value = NA) {
    if (!column_name %in% colnames(df)) {
      df <- df %>% dplyr::mutate(!!sym(column_name) := default_value)
    }
    return(df)
  }

  # Function to check if any flags exist in a time window
  check_2_hour_window_fail <- function(x) {
    sum(x) >= 1
  }

  # Establish helper objects
  # String object that is used to ignore flags that we do not want to remove.
  ignore_flags <- "drift|DO interference|repeat|sonde not employed|frozen|
  unsubmerged|missing data|site visit|sv window|sensor malfunction|burial|
  sensor biofouling|improper level cal|sonde moved"

  # Numeric object that determines the width for the rolling window check (2 hours)
  width_fun = 17

  # Process flags based on upstream/downstream patterns
  final_df <- up_down_join %>%
    # Add placeholder columns if joinging didn't provide them
    add_column_if_not_exists("flag_down") %>%
    add_column_if_not_exists("flag_up") %>%
    add_column_if_not_exists("site_down") %>%
    add_column_if_not_exists("site_up") %>%
    # Create binary indicator for upstream/downstream flags
    ## 0 = no relevant flags upstream/downstream
    ## 1 = at least one site has relevant flags
    dplyr::mutate(flag_binary = dplyr::if_else(
      (is.na(flag_up) | grepl(ignore_flags, flag_up)) &
        (is.na(flag_down) | grepl(ignore_flags, flag_down)), 0, 1
    )) %>%
    # Check for flags in 4-hour window (+/-2 hours around each point, 17 observations at 15-min intervals)
    dplyr::mutate(overlapping_flag = zoo::rollapply(flag_binary, width = width_fun, FUN = check_2_hour_window_fail, fill = NA, align = "center")) %>%
    add_column_if_not_exists(column_name = "auto_flag") %>%
    # If flag exists but is also present up/downstream, it likely represents a real event
    # In that case, remove the flag (set auto_flag to NA)
    dplyr::mutate(auto_flag = ifelse(!is.na(flag) & !grepl(ignore_flags, flag) &
                                       (overlapping_flag == TRUE & !is.na(overlapping_flag)), NA, flag)) %>%
    dplyr::select(-c(flag_up, flag_down, site_up, site_down, flag_binary, overlapping_flag))

  return(final_df)
}

# I really don't understand how network check would work in parallel, so I don't.
final_flags <- intrasensor_flags_list %>%
  purrr::map(~custom_network_check(df = ., intrasensor_flags_arg = intrasensor_flags_list)) %>%
  rbindlist(fill = TRUE) %>%
  tidy_flag_column() %>%
  split(f = list(.$site, .$parameter), sep = "-") %>%
  purrr::map(~add_suspect_flag(.)) %>%
  rbindlist(fill = TRUE)

v_final_flags <- final_flags%>%
  dplyr::mutate(auto_flag = ifelse(is.na(auto_flag), NA,
                                   ifelse(auto_flag == "suspect data" & is.na(lag(auto_flag, 1)) & is.na(lead(auto_flag, 1)), NA, auto_flag))) %>%
  dplyr::select(c("DT_round", "DT_join", "site", "parameter", "mean", "units", "n_obs", "spread", "auto_flag", "mal_flag", "sonde_moved","sonde_employed", "season", "last_site_visit")) %>%
  dplyr::mutate(auto_flag = ifelse(is.na(auto_flag), NA, ifelse(auto_flag == "", NA, auto_flag))) %>%
  split(f = list(.$site, .$parameter), sep = "-") %>%
  keep(~nrow(.) > 0)

# Save the data individually.
iwalk(v_final_flags, ~write_csv(.x, here("data","sharing","quarterly_meetings", "2025_Q2","flagged_final", paste0(.y, ".csv"))))




