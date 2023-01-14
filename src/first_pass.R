# First flagging pass. This is AFTER removing data where sensors were not in the field.

first_pass <- function(what, parama, max = 100000, min = -100000){

  it <- what %>%
    dplyr::select("DT", "DT_instrument", parama) %>%
    rename(raw=3) %>%
    arrange(ymd_hms(DT)) %>%
    # Pass one (p1): remove any data outside what appears to be the natural range:
    mutate(p1 = ifelse(raw >= max, NA,
                ifelse(raw <= min , NA, raw)),
           removed_flag = ifelse(p1 != raw, 1, 0)) %>%
    # For sites with more than one measurement within the 15-minute increment, average it
    # and preserve how many data points are contained within that increment (n) the difference
    # between the lowest and greatest data points.
    group_by(DT) %>%
    summarize(p1 = as.numeric(mean(p1, na.rm = T)),
              diff = abs(min(p1, na.rm = T) - max(p1, na.rm = T)),
              n_obs = n(),
              removed_flag = sum(removed_flag)) %>%
  ungroup() %>%
   mutate(
     front1=lead(p1, n=1),
     front2=lead(p1, n=2),
     front3=lead(p1, n=3),
     back1=lag(p1, n=1),
     back2=lag(p1, n=2),
     back3=lag(p1, n=3)) %>%
   group_by(DT) %>%
   mutate(rollsd = sd(unlist(select(cur_data(), front1:back3))),
          rollavg=mean(unlist(select(cur_data(), front1:back3)))) %>%
   ungroup() %>%
   mutate(sd3_flag = ifelse((p1 <= rollavg-(3*rollsd) | p1 >= rollavg+(3*rollsd)), 1, 0))

  return(it)
}
