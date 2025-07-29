## Water Sampling Data:

# Goal:
#Save data in the correct format for RMRS spreadsheet
#Save all water sampling probe values in a spreadsheet
# To get the RMRS style data for a specfic date of sampling,
# Input the date of interest in sampling_spreadsheet_creator
#sampling_spreadsheet_creator(date_oi = "2023-11-17")

# To get all the water sampling data and save to CSV in sampling notes
# This also returns the df sampling_notes in case you want to review in R
#sampling_spreadsheet_creator(all_dates = TRUE)


sampling_spreadsheet_creator <- function(date_oi = "2023-10-16", all_dates = FALSE ){

  #source clean mwater script for all notes cleaned

  source("src/load_mWater_notes.R")

  #pull in site meta data
  site_meta <- read_csv("data/metadata/water_sampling_sites.csv",show_col_types = FALSE)%>%
    select(site = site_code, Site_Name, site_label_rmrs)
  # sort for sites in upper network (ie. acronyms rather than street names)
  upper_sites <- read_csv("data/metadata/water_sampling_sites.csv",show_col_types = FALSE)%>%
    filter(watershed != "CLP  Mainstem-Fort Collins")%>%
    #this is to help match with user input
    mutate(site_code = tolower(site_code))

  # create df of all water samples and save DT, handheld probe and chla volume data
  sampling_notes <- load_mWater_notes()%>%
    filter(grepl("Sampling",visit_type))%>%
    mutate(all_pics_taken = case_when(!is.na(downstream_pic)&!is.na(upstream_pic)&!is.na(clarity)&!is.na(filter_pic) ~ TRUE, TRUE ~ FALSE),
           #correct names if it is in our upper sites (acronyms)
           site = ifelse(site %in% upper_sites$site_code, toupper(site), site),
           DT_round = round_date(start_DT, "15 minutes"))%>%
    select(site,crew, DT_round, date, time = start_time_mst, sample_collected, chla_volume_ml, vol_filtered_blank_dup, do_mgl, cond_ms_cm, temp_c, visit_comments, all_pics_taken, q_cfs)

  # Distinguish BLANK and DUPLICATE values
  blanks_dups <- sampling_notes %>%
    #find all values that have blank or dup
    filter(grepl("DUPLICATE|BLANK", sample_collected)) %>%
    # change sample collected to match BLANK/DUP
    mutate(sample_collected = ifelse(grepl("DUPLICATE", sample_collected), "DUPLICATE", "BLANK"),
           # Volume filtered blank dup becomes chla volume
           chla_volume_ml = vol_filtered_blank_dup,
           #drop vol_filtered_blank/dup
           vol_filtered_blank_dup = NULL)

  # Add blank and duplicate values back to main
  sampling_notes <- sampling_notes%>%
    #get rid of blank/dup in sample collcected
    mutate(sample_collected = gsub("DUPLICATE|BLANK| |,", "", sample_collected),
           #drop vol_filtered_blank/dup
           vol_filtered_blank_dup = NULL)%>%
    #bring in blank and dup rows
    rbind(blanks_dups)%>%
    #arrange by datetime and site (Blanks and dups go second)
    arrange(DT_round, site)%>%
    # join with RMRS friendly metadata
    left_join(site_meta, by = "site")



 if (all_dates == TRUE) {

   sampling_notes_output <- sampling_notes%>%
     # select only the needed columns, saved in the correct order and fix column names
     select(site_code = site, Date = date, DT_round, SampleType = sample_collected, time_mst = time,chla_volume_ml,  do_mgl, cond_ms_cm, temp_c, visit_comments)

   # write to csv
   write_csv(x = sampling_notes_output, file = paste0("data/field_notes/all_samples_as_of_",as.character(Sys.Date()),".csv" ))
 }else{
   #grab desired date
   date_oi_clean <- as.Date(date_oi, tz = "America/Denver")
   # filter sampling notes df by desired date
   samples_of_day <- filter(sampling_notes,date == date_oi_clean )%>%
     #match date to RMRS style
     mutate(Date = format(date, "%d-%b-%y"),
            #create SiteDescr column for RMRS sheet
            SiteDescr = paste0(site, "_",format(date, "%m%d%y")))%>%
     # select only the needed columns, saved in the correct order and fix column names
     select(Site = site ,SiteDescr, SiteLabel = site_label_rmrs , Date, SampleType = sample_collected, q_cfs,
            time, do_mgl, cond_ms_cm, temp_c, notes = visit_comments  )
   # write to csv
   write_csv(x = samples_of_day, file = paste0("data/field_notes/samples_of_",date_oi,".csv" ))
 }
}

