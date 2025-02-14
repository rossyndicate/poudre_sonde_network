# "Placeholder" function for determining when sonde was moved in its housing. Will integrate this information into field notes
# in the future so that this csv doesn't need to exist in the future.

add_depth_shift_flag <- function(df, level_shift_table){

  depth_shifts <- level_shift_table %>%
    dplyr::filter(type == "sonde moved") %>%
    dplyr::mutate(DT_join = as.character(lubridate::ymd_hms(DT_join)))

  add_shifts <- df %>%
    dplyr::left_join(., depth_shifts, by = c("site", "DT_join")) %>%
    dplyr::mutate(depth_change = ifelse(!is.na(type), "sonde moved", NA)) %>%
    dplyr::select(-type)

  return(add_shifts)

}

