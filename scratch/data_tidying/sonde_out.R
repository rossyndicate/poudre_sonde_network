### Recoding data where sonde was out of water

# Filter instances in which a sonde was pulled out of field mid-season (basically a backup filter in case a sonde was pulled out of water but the log didn't get stopped)

#this function is not working... not going to debug right now, but this is meant to do the lifting of this code block, but in an automated way...

recode_for_maintenance = function(start, end, site) {
  # start_DT = ymd_hm(start, tz = 'MST')
  # end_DT = ymd_hm(end, tz = 'MST')
  all_data %>%
    setDT(.) %>%
    mutate(value = if_else(ymd_hms(DT) >= ymd_hms(start) &
                             ymd_hms(DT) <= ymd_hms(end) &
                             site == site,
                           NA_real_,
                           value),
           flag = if_else(ymd_hms(DT) >= ymd_hms(start) &
                            ymd_hms(DT) <= ymd_hms(end) &
                            site == site,
                          'recoded for sensor maintenance',
                          NA_character_))
}

# preserving this for now.
all_data <- all_data %>%
  setDT(.) %>%
  # Rist
  filter(!(ymd_hms(DT_round) >= ymd_hms("2021-09-11 09:00:00") & ymd_hms(DT_round) <= ymd_hms("2022-04-22 14:00:00") & site == "rist"),
         !(ymd_hms(DT_round) <= ymd_hms("2022-05-30 09:00:00") & site == "rist"),
         !(ymd_hms(DT_round) >= ymd_hms('2022-05-06 12:00:00') & ymd_hms(DT_round) <= ymd_hms('2022-05-09 14:30:00') & site == "rist"),
         # Legacy
         !(ymd_hms(DT_round) >= ymd_hms('2021-12-04 19:30:00') & ymd_hms(DT_round) < ymd_hms('2022-04-06 17:30:00') & site == "legacy"),
         !(ymd_hms(DT_round) >= ymd_hms('2022-05-24 09:30:00') & ymd_hms(DT_round) < ymd_hms('2022-06-01 13:30:00') & site == "legacy"),
         !(ymd_hms(DT_round) > ymd_hms('2022-07-08 14:00:00') & ymd_hms(DT_round) <= ymd_hms('2022-07-12 10:00:00') & site == "legacy"),
         !(ymd_hms(DT_round) >= ymd_hms('2022-08-04 09:50:00') & ymd_hms(DT_round) <= ymd_hms('2022-08-25 16:15:00') & site == "legacy"),
         !(ymd_hms(DT_round) > ymd_hms('2022-09-07 06:57:00') & ymd_hms(DT_round) <= ymd_hms('2022-09-18 07:00:00') & site == "legacy"),
         # Timberline
         !(ymd_hms(DT_round) > ymd_hms('2022-01-01 08:15:00') & ymd_hms(DT_round) <= ymd_hms('2022-04-06 08:15:00') & site == "timberline"),
         # Archery
         !(ymd_hms(DT_round) > ymd_hms('2022-10-04 15:00:00') & ymd_hms(DT_round) <= ymd_hms('2022-10-07 16:00:00') & site == "archery"))
