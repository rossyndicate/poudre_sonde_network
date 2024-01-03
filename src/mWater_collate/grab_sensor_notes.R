grab_mWater_sensor_notes <- function(){

  #source clean mwater script for all notes cleaned

  #source("src/mWater_collate/clean_mwater_notes.R")

  # Sensor Notes

  # These are the notes that will be added to the QAQC workflow notes Most of the code in this chunk is to get the df to
  # match the one in the QAQC workflow It can be saved as a CSV or pulled directly into QAQC workflow
  #grab only notes where technician is interacting with sensor on site (excludes sensor malfunction notes)

  mWater_field_notes <- clean_mwater_notes() %>%
    filter(grepl("Sensor",visit_type, ignore.case = TRUE) & !grepl("Sensor malfunction",visit_type, ignore.case = TRUE))%>%
    # determining sonde employed status based on sensor_change
    mutate(sonde_employed = case_when(is.na(sensor_change)  ~ NA,
                                      sensor_change == "Swapped" ~ NA,
                                      sensor_change == "Pulled" ~ 1,
                                      sensor_change == "Deployed" ~ 0),
                                      #sensor_change %in% c("Swapped", "Deployed") ~ 1),

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
    #order columns in easily readable ordering
    select(site, crew, DT_round,sonde_employed,  sensors_cleaned, wiper_working, rdo_cap_condition, rdo_cap_replaced , ph_junction_replaced ,
           cals_performed, cal_report_collected , sensor_malfunction,sensor_pulled,sensor_deployed, sensor_swapped_notes,
           visit_type,start_time_mst,DT_join,  start_DT, end_dt,date,  visit_comments,photos_downloaded, field_season, last_site_visit)

  # back up to CSV
  # write_csv(sensor_notes, "data/mWater_sensor_field_notes.csv")

  # rm(all_notes_cleaned)
  return(mWater_field_notes)

}

