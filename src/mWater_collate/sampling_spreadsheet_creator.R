sampling_spreadsheet_creator <- function(date_oi = "2023-10-16" ){

  #pull in site meta data
  site_meta <- read_csv("data/water_sampling_sites.csv")%>%
    select(site = site_code, Site_Name, site_label_rmrs)
#grab desired date
  date_oi_clean <- as.Date(date_oi)
# filter sampling notes df by desired date
  samples_of_day <- filter(sampling_notes,date == date_oi_clean )%>%
    # join with RMRS friendly metadata
    left_join(site_meta, by = "site")%>%
    #match date to RMRS style
    mutate(Date = format(date, "%d-%b-%y"),
           #create SiteDescr column for RMRS sheet
           SiteDescr = paste0(site, "_",format(date, "%m%d%y")))%>%
    # select only the needed columns, saved in the correct order and fix column names
    select(Site = site_label_rmrs ,SiteDescr, SiteLabel = site, Date, SampleType = sample_collected,
           time, do_mgl, cond_ms_cm, temp_c, notes = visit_comments  )
# write to csv
  write_csv(x = samples_of_day, file = paste0("data/sampling_notes/samples_of_",date_oi,".csv" ))
}
