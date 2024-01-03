grab_malfunction_records <- function(){

#source clean mwater script for all notes cleaned

source("src/mWater_collate/clean_mwater_notes.R")

# Grab notes about sensor malfunction
malfunction_records <- all_notes_cleaned%>%
  filter(grepl("Sensor malfunction",visit_type, ignore.case = TRUE))%>%
  select(start_DT, site, crew, which_sensor_malfunction, malfunction_end_dt, notes = visit_comments)

#write to csv
write_csv(malfunction_records, "data/mWater_malfunciton_records.csv")

}
