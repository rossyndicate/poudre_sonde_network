
library(tidyverse)

#grab site metadata
sampling_sites <- read_csv("data/water_sampling_sites.csv",show_col_types = FALSE)
# sort for sites in upper network (ie. acronyms rather than street names)
upper_sites <- sampling_sites%>%
  filter(watershed != "CLP  Mainstem-Fort Collins")%>%
  #this is to help match with user input
  mutate(site_code = tolower(site_code))

`%nin%` = Negate(`%in%`)

# API Pull of mWater submitted notes


# Grab API url from yml
# Contact Sam Struthers if you need access
creds = yaml::read_yaml("src/mWater_collate/mWater_API.yml")
api_url = as.character(creds["url"])


#read in datasheet from mWater portal API
all_notes <- read_csv(url(api_url),show_col_types = FALSE)

rm(creds, api_url)

# Tidying for downstream use

# This is basic tidying of data set to:
# correct datetime from UTC to Denver time
# correct columns where Other input is allowed (Site, visit type, photos downloaded, sensor malfunction)
# Add rounded date time



all_notes_cleaned <- all_notes%>%
  mutate(
    #start and end dt comes in as UTC -> to MST
    start_dt_mst = with_tz(ymd_hms(start_dt), tz = "MST"),
    end_dt = with_tz(ymd_hms(end_dt), tz = "MST"),
    malfunction_end_dt = with_tz(ymd_hms(malfunction_end_dt), tz = "MST"),
    date = as.Date(start_dt_mst, tz = "MST"),
    time_start = format(start_dt_mst, "%H:%M"),

    # If other is chosen, make site == other response
    site = ifelse(site == "Other (please specify)", tolower(str_replace_all(site_other, " ", "")), site),
    #correct names if it is in our upper sites (acronyms)
    site = ifelse(site %in% upper_sites$site_code, toupper(site), site),
    site_other = NULL,

    #When I changed the mWater survey, I accidentally introduced ??? in the place of Sensor Calibration option, Replacing that here
    visit_type = case_when(str_detect(visit_type, "\\?\\?\\?") ~ str_replace(string = visit_type,
                                                                             pattern =  "\\?\\?\\?",
                                                                             replacement = "Sensor Calibration or Check"),
                           TRUE~ visit_type),
    # Merging visit_type  and visit type other
    visit_type = case_when(str_detect(visit_type, "Other") ~ str_replace(string = visit_type,
                                                                         pattern =  "Other \\(please specify\\)",
                                                                         replacement = visit_type_other),
                           TRUE ~ visit_type),
    visit_type_other = NULL,
    # Merge sensor malfunction + sensor malfunction other
    other_which_sensor_malfunction = as.character(other_which_sensor_malfunction),
    which_sensor_malfunction = case_when(str_detect(which_sensor_malfunction, "Other") ~ str_replace(string = which_sensor_malfunction,
                                                                                                     pattern =  "Other \\(please specify\\)",
                                                                                                     replacement = other_which_sensor_malfunction),
                                         TRUE ~ which_sensor_malfunction),
    other_which_sensor_malfunction = NULL,

    #If other is chosen, make photos downloaded equal to response
    photos_downloaded = ifelse(photos_downloaded == "Other (please specify)", photos_downloaded_other, photos_downloaded),
    photos_downloaded_other = NULL,
    #Rounded start date time
    DT_round = floor_date(start_dt_mst, "15 minutes")) %>%
  #arrange by most recent visit
  arrange(DT_round)


# Sensor Notes

# These are the notes that will be added to the QAQC workflow notes Most of the code in this chunk is to get the df to
# match the one in the QAQC workflow It can be saved as a CSV or pulled directly into QAQC workflow
#grab only notes where technician is interacting with sensor on site (excludes sensor malfunction notes)

sensor_notes <- all_notes_cleaned%>%
  filter(grepl("Sensor",visit_type, ignore.case = TRUE) & !grepl("Sensor malfunction",visit_type, ignore.case = TRUE))%>%
  select(-c( start_dt, chla_volume_ml, vol_filtered_blank_dup, sample_collected,do_mgl,cond_ms_cm, temp_c, upstream_pic,
             downstream_pic, clarity, filter_pic, other_pic, other_pic_descriptor, which_sensor_malfunction, malfunction_end_dt,
             log1_type, log2_type, log1_mmdd, log2_mmdd, log_downloaded))%>%
  # determining sonde employed status based on sensor_change
  mutate(sonde_employed = case_when(is.na(sensor_change)  ~ 1,
                                    sensor_change == "Pulled" ~ 0,
                                    sensor_change %in% c("Swapped", "Deployed") ~ 1),
        sensor_pulled = as.character(sn_removed),
        sensor_deployed = as.character(sn_deployed),
         #Sensor swapped notes
         sensor_swapped_notes = case_when(is.na(sensor_change)  ~ NA,
                                    sensor_change == "Pulled" &!is.na(sensor_pulled) ~ paste0("SN Removed: ", sensor_pulled),
                                    sensor_change == "Swapped" ~ paste0("SN Removed: ", sensor_pulled, " SN Deployed: ", sensor_deployed),
                                    sensor_change == "Deployed" ~ sensor_deployed),
         #Date/field season columns to match QAQC workflow
         DT_join = as.character(DT_round),
         field_season = year(DT_round),
         last_site_visit = DT_round,
        date = as.character(date)
  )%>%
  arrange(desc(DT_round))%>%
  rename(start_DT = start_dt_mst, start_time_mst = time_start)%>%
  select(-c(sn_removed, sn_deployed, sensor_change))

#write to CSV
write_csv(sensor_notes, "data/mWater_sensor_field_notes.csv")


# Grab notes about sensor malfunction
malfunction_records <- all_notes_cleaned%>%
  filter(grepl("Sensor malfunction",visit_type, ignore.case = TRUE))%>%
  select(start_dt_mst, site, crew, which_sensor_malfunction,malfunction_end_dt, notes = visit_comments )
#write to csv

write_csv(malfunction_records, "data/mWater_malfunciton_records.csv")



## Determining uploads

#This function looks at the user inputs for calibration report collect and logs collected.
#Based on these inputs, it looks at all the uploaded logs or calibration reports
#and will print out what logs are missing and who to contact to get those files uploaded.

source("src/mWater_collate/files_missing.R")

files_missing()



## Water Sampling Data:

# Goal:
#Save data in the correct format for RMRS spreadsheet
#Save all water sampling probe values in a spreadsheet


#source function
source("src/mWater_collate/sampling_spreadsheet_creator.R")
# To get the RMRS style data for a specfic date of sampling,
# Input the date of interest in sampling_spreadsheet_creator
#sampling_spreadsheet_creator(date_oi = "2023-11-17")

# To get all the water sampling data and save to CSV in sampling notes
# This also returns the df sampling_notes in case you want to review in R
#sampling_spreadsheet_creator(all_dates = TRUE)


## Photos

# Goals:
# Download all user created photos ( upstream, downstream, clarity, filter and other pictures)
# Label according to site, date, description in the format site_YYYYMMDD_descriptor.jpg
# Only download photos which have not yet been downloaded


source("src/mWater_collate/download_pictures.R")

#RUN TO DOWNLOAD NEW PICTURES
# It takes about 2-5 minutes to download ~25-50 photos
# Sometimes the request to mWater time out, just re run the function below if that happens

download_pictures()


# Remove extra objects not needed in QAQC pipeline

rm(all_notes, sampling_sites, upper_sites, all_notes_cleaned)

