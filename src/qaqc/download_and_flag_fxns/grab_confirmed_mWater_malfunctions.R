grab_confirmed_mWater_malfunctions <- function(field_notes){

    # Grab notes about sensor malfunction
    malfunction_records <- all_notes_cleaned %>%
      dplyr::filter(grepl("Sensor malfunction", visit_type, ignore.case = TRUE)) %>%
      dplyr::select(start_DT, site, crew, which_sensor_malfunction, malfunction_end_dt, notes = visit_comments)

    # write to csv
    write_csv(malfunction_records, "data/mWater_malfunciton_records.csv")

  }

}
