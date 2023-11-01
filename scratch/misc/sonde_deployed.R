# Determining when the sonde was employed (SE) based on `sensor_pulled` (SP) and `sensor_deployed` (SD) columns.
## when (SP & SD) columns are NOT empty, SE = 0 (employed).
## when (SP) column is NOT empty, but (SD) column is empty, SE = 1 (NOT employed).
## when (SP) column is empty, but (SD) column is NOT empty, SE = 0 (employed).
## when (SP & SD) columns are empty, SE = NA (unknown).

## Downstream, fill() (fill in missing values with previous value)
## is used on the sonde_employed column after it has been joined
## to the pulled API data to determine if the sonde was employed for that data.
deployment_record <- field_notes %>%
  # filter for years that are 2022 and greater
  filter(year(DT_round) >= 2022) %>%
  arrange(site, DT_round) %>%
  group_by(site) %>%
  # `sonde_employed` determines if the sonde is deployed or not. 0 = sonde deployed, 1 = sonde is not deployed
  mutate(sonde_employed = case_when(!is.na(sensor_pulled) & !is.na(sensor_deployed) ~ 0,
                           !is.na(sensor_pulled) & is.na(sensor_deployed) ~ 1,
                           is.na(sensor_pulled) & !is.na(sensor_deployed) ~ 0,
                           is.na(sensor_pulled) & is.na(sensor_deployed) ~ NA)) %>% #,
         #last_site_visit = DT_round) %>%
  filter(!is.na(sonde_employed))
