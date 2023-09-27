# Munge field notes

field_notes_clean <- field_notes %>%
  mutate(sonde_impact = case_when(grepl('sonde', visit_comments, ignore.case = T) ~ 'y',
                                  grepl('calib', visit_comments, ignore.case = T) ~ 'y',
                                  sensor_pulled == 'x' ~ 'y',
                                  sensor_deployed == 'x' ~ 'y',
                                  grepl('yes', sensors_cleaned, ignore.case = T) ~ 'y'
  ),
  end_DT = case_when(sensor_pulled == 'x' ~ ymd_hm(paste0(season, '-12-31 23:59'), tz = 'MST'),
                     sensor_deployed == 'x' ~ start_DT,
                     TRUE ~ start_DT + minutes(15)),
  start_DT = case_when(sensor_deployed == 'x' ~ ymd_hm(paste0(season, '-01-01 00:00'), tz = 'MST'),
                       TRUE ~ start_DT)) %>%
  filter(sonde_impact == 'y', !is.na(start_DT))
