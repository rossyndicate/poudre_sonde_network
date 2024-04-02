# historic data ----
View(flagged_data_dfs[[3]])
# When we load our historic data in the DT_round column comes in as <POSIXct> [3055],
# and it seems to have the time zone MST

# set api pull data ----

# on the GUI the date time is set to america/denver
# populate the incoming data: ----
tar_make(incoming_data_csvs_upload)

# read in a sample api pull
archery_sample <- read_csv("data/api/incoming_api_data/archery_2023-11-29_1426.csv") %>%  # this is in UTC
  filter(!grepl("vulink", name, ignore.case = TRUE)) %>% # these will throw us errors if there is none?
  # remove Virridy data (for now)
  filter(!grepl("virridy", name, ignore.case = TRUE))

# munge the API data ----
tar_load(incoming_data_collated_csvs) # the DT from the above step gets converted successfully during the munge_api_data step, so now the incoming data is in MST and lines up with HydroVu

# lets explore the old raw field notes ----
# it says that the start time is in MST and that seems to be in line with the historically flagged data, which I think is in MST

# clean the field notes ----
clean_field_notes <- clean_field_notes(old_raw_field_notes) # the DT from the raw to clean field notes does not change and both are in MST and in line with the historically flagged data

# grab the mWater field notes ----

mWater_notes <- grab_mWater_sensor_notes()
View(mWater_notes) # according to the functions these should be in MST tbh

# bind all the field notes together ----
field_notes <- rbind(clean_field_notes, mWater_notes)
View(field_notes)

# check on the written flagged data to make sure that all of the data is sequential

flagged_data <- readRDS("data/flagged/test_all_data_flagged.RDS")

# Assuming your datetime column is called 'datetime_column'
datetime_diff <- diff(flagged_data[[1]]$DT_round)

# Convert time differences to minutes
datetime_diff_minutes <- as.numeric(datetime_diff, units = "mins")

# Check if all time differences are exactly 15 minutes
all_15_minutes <- all(datetime_diff_minutes == 15)

if (all_15_minutes) {
  print("All rows in the column are 15 minutes apart.")
} else {
  print("Not all rows in the column are 15 minutes apart.")
}

flagged_data[[1]] %>% View()
