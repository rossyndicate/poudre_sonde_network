# to do (j): document this function
api_puller <- function(site, start_dt, api_token, dump_dir) {

  end_dt <- "2023-11-30 14:26:54 EST" #Sys.time()

  locs <- hv_locations_all(api_token)

  # make a list of site names

  options(scipen = 999)

  site_loc <- locs %>%
    mutate(name = tolower(name)) %>%
    filter(grepl(site, name, ignore.case = TRUE))

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
  utc_start_date <- format(as.POSIXct(format(start_dt + hours(7), tz = "MST"), tz = "UTC"), format = "%Y-%m-%d %H:%M:%S")

  utc_end_date <- format(as.POSIXct(format(end_dt, tz = "UTC"), tz = "UTC"), format = "%Y-%m-%d %H:%M:%S")

  timezone = "UTC"

  # Map over the location ids
  alldata <- site_loc_list %>% map(~hv_data_id(.,
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
    one_df <- bind_rows(filtered) %>%
      rename(id = Location,
             parameter = Parameter,
             units = Units) %>%
      left_join(., site_loc, by = "id") %>%
      mutate(site = tolower(site)) %>%
      select(site, id, name, timestamp, parameter, value, units)

    ## Save your data

    write_csv(one_df,
              paste0(dump_dir, "/", site, "_", lubridate::as_date(end_dt), ".csv"))

    # return(one_df)
  }

}


# delete this when done
# test_api_puller <- api_puller(site = "archery", start_dt = "2023-05-22 11:30:00", dump_dir = "scratch/scratch_data/") #, dump_dir = "data/api/incoming_api_data/")


