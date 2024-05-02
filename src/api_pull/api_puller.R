# to do (j): document this function
api_puller <- function(site, start_dt, end_dt = Sys.time(), api_token, dump_dir, require = NULL) {

  locs <- hv_locations_all(api_token)

  # make a list of site names

  options(scipen = 999)

  site_loc <- locs %>%
    dplyr::mutate(name = tolower(name)) %>%
    dplyr::filter(grepl(site, name, ignore.case = TRUE))

  site_loc_list <- site_loc$id

  # Get data for each site location. Note this maps over the entire list of locations,
  # many of which are unlikely to be active during the time you specify. Don't freak out if
  # you see a bunch of '404 Not Found' errors, you're just seeing the list of locations
  # that are not active. The data frame 'alldata' should contain your data from all applicable
  # sites during the time frame indicated. Note that this will take some time (one month of
  # data for 5 sites takes ~10 mins. Be patient!

  # Add date range you are interested in; data are stored in HydroVu in UTC
  # Here, a way to find the most recent download of the data. Use this as the start date to
  # reduce overlapping data

  # tz weirdness
  # utc_start_date <- format(as.POSIXct(start_dt, tz = "UTC") + hours(7), format = "%Y-%m-%d %H:%M:%S")
  #
  # utc_end_date <-   format(as.POSIXct(end_dt, tz = "UTC") + hours(7), format = "%Y-%m-%d %H:%M:%S")

  # doing this fixes the mismatch in date times during the combined_data step - jd
  utc_start_date <- format(as.POSIXct(start_dt, tz = "UTC") + lubridate::hours(0), format = "%Y-%m-%d %H:%M:%S")

  utc_end_date <-   format(as.POSIXct(end_dt, tz = "UTC") + lubridate::hours(0), format = "%Y-%m-%d %H:%M:%S")

  timezone = "UTC"

  # Map over the location ids
  alldata <- site_loc_list %>% purrr::map(~hv_data_id(.,
                                                      start_time = utc_start_date,
                                                      end_time = utc_end_date,
                                                      token = api_token,
                                                      tz = timezone))

  # grab only locations with data (stored as a data frame) / drop 404 errors
  filtered <- purrr::keep(alldata, is.data.frame)

  if(length(filtered) == 0){

    print(paste0("No data at ", site, " during this time frame"))

  } else {

    # bind lists together (now that all are dataframes, we can just collate quickly)
    one_df <- dplyr::bind_rows(filtered) %>%
      dplyr::rename(id = Location,
                    parameter = Parameter,
                    units = Units) %>%
      dplyr::left_join(., site_loc, by = "id") %>%
      dplyr::mutate(site = tolower(site)) %>%
      dplyr::select(site, id, name, timestamp, parameter, value, units)

    ## Save your data

    readr::write_csv(one_df,
                     paste0(dump_dir, "/", site, "_", stringr::str_replace(stringr::str_replace(substr(end_dt, 1, 16), "[:\\s]", "_"), ":", ""), ".csv"))
  }

}
