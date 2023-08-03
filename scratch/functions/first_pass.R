# First flagging pass. This is AFTER removing data where sensors were not in the field.

first_pass <- function(what, parama, max = 100000, min = -100000){

  it <- what %>%
    ungroup() %>%
    dplyr::select("DT", parama, "site") %>%
    rename(raw = 2) %>%
    arrange((DT))

  it$raw[is.nan(it$raw)] <- NA

  it <- it %>%
    # Pass one (p1): remove any data outside what appears to be the natural range:
    mutate(p1 = ifelse(raw >= max, NA,
                ifelse(raw <= min , NA, raw)),
           removed_flag = ifelse(p1 != raw, 1, 0)) %>%
    # For sites with more than one measurement within the 15-minute increment, average it
    # and preserve how many data points are contained within that increment (n_obs), and
    # the difference between the lowest and greatest data points.
    group_by(DT, site) %>%
    summarize(p1 = as.numeric(mean(p1, na.rm = T)),
              diff = abs(min(p1, na.rm = T) - max(p1, na.rm = T)),
              n_obs = n(),
              removed_flag = sum(removed_flag)) %>%
  group_by(site) %>%
   mutate(
     front1=lead(p1, n = 1),
     front2=lead(p1, n = 2),
     front3=lead(p1, n = 3),
     back1=lag(p1, n = 1),
     back2=lag(p1, n = 2),
     back3=lag(p1, n = 3)) %>%
   group_by(site, DT) %>%
   mutate(rollsd = sd(unlist(select(cur_data(), front1:back3))),
          rollavg=mean(unlist(select(cur_data(), front1:back3)))) %>%
   ungroup() %>%
   mutate(sd3_flag = ifelse((p1 <= rollavg-(3*rollsd) | p1 >= rollavg+(3*rollsd)), 1, 0))
 # Step 5: Flagging bad data
  water_quality_data_clean$Flagged <- abs(water_quality_data_clean$parameter - water_quality_data_clean$RollingAverage) > num_sd_threshold * water_quality_data_clean$RollingSD


  return(it)
}
