#' @title Network Check
#'
#' @description
#' This function performs a network check on a given data frame, flagging potential 
#' issues in the data based on upstream and downstream sites.
#'
#' @param df A site-parameter data frame that has gone through the initial flagging
#' process.
#' 
#' @return A modified data frame flags that have been altered based on the network check.
#' 
#' @examples
#' network_check(df = all_data_flagged$`archery-Actual Conductivity`)

network_check <- function(df) {

  df <- df

  # get the site name from the site column in the df of interest
  site_name <- unique(na.omit(df$site))
  # get the parameter from the parameter column in the df of interest
  parameter_name <- unique(na.omit(df$parameter))

  # vector of sites in the order that they are in spatially
  # some sites have some funkiness going on
  sites_order <- c("tamasag", # rist
                   "legacy",
                   "lincoln",
                   "timberline",
                   "prospect",
                   "boxelder", # elc
                   "archery",
                   "river bluffs")

  width_fun = ifelse(site_name == "tamasag", 17, # 2 hours before/after
              ifelse(site_name == "legacy", 17,
              ifelse(site_name == "lincoln", 17,
              ifelse(site_name =="timberline", 17,
              ifelse(site_name ==  "prospect", 17,
              ifelse(site_name == "boxelder", 17,
              ifelse(site_name ==   "archery", 17,
              ifelse(site_name == "river bluffs", 17, NA))))))))

  # determining the index for the site of interest.
  site_index <- which(sites_order == site_name)

  # Generating df name to pull from df_list list
  site_param <- paste0(site_name, "-", parameter_name)

  prev_site_df <- tibble(DT_round = NA)
  next_site_df <- tibble(DT_round = NA)

  tryCatch({
    previous_site <- paste0(sites_order[site_index-1],"-",parameter_name)
    prev_site_df <- all_data_flagged[[previous_site]] %>%
      select(DT_round, site_up = site, flag_up = flag) %>%
      data.table()},
    error = function(err) {
      cat("No upstream site.\n")})

  tryCatch({
    next_site <- paste0(sites_order[site_index+1],"-",parameter_name)
    next_site_df <- all_data_flagged[[next_site]] %>%
      select(DT_round, site_down = site, flag_down = flag) %>%
      data.table()},
    error = function(err) {
      cat("No downstream site.\n")})


  join <- df %>%
    left_join(., prev_site_df, by = "DT_round") %>%
    left_join(., next_site_df, by = "DT_round")

  if(!("flag_down" %in% colnames(join))) {join$flag_down <- NA}
  if(!("flag_up" %in% colnames(join))) {join$flag_up <- NA}
  if(!("site_down" %in% colnames(join))) {join$site_down <- NA}
  if(!("site_up" %in% colnames(join))) {join$site_up <- NA}


  # Define a function to check if a given 2-hour window has any instances of the same word
  check_2_hour_window_fail <- function(x) {
    sum(x) >= 1
  }

  df_test <- join %>%
    # No upstream/downstream flag = 0
    mutate(flag_binary = ifelse(
        (is.na(flag_up) | grepl("repeat|sonde not employed|missing data|site visit|sv window|sensor malfunction", flag_up)) &
        (is.na(flag_down) | grepl("repeat|sonde not employed|missing data|site visit|sv window|sensor malfunction", flag_down)), 0, 1)) %>%
    mutate(overlapping_flag = zoo::rollapply(flag_binary, width = width_fun, FUN = check_2_hour_window_fail, fill = NA, align = "center")) %>%
    # If there is a flag (flags associated with spikes in concentration or funkiness like that), and there is also a flag up/downstream at the same time (2 hour window) it is likely a real
    # WQ event and should therefore not be considered "poor" data:
    mutate(cleaner_flag = ifelse(!is.na(flag) & !grepl("repeat|sonde not employed|missing data|site visit|sv window|sensor malfunction", flag) & overlapping_flag == TRUE, NA, flag)) %>%
    select(-c(flag_up, flag_down, site_up, site_down, flag_binary, overlapping_flag)) %>%
    mutate(mean_public = ifelse(is.na(flag), mean, NA)) #***

  # df_test <- join %>%
  #   mutate(flag_binary = ifelse(#grepl("slope|suspect", flag) &
  #     (is.na(flag_up) | grepl("seasonal range|repeat|sonde not employed|missing data|site visit|sv window", flag_up)) &
  #       (is.na(flag_down) | grepl("seasonal range|repeat|sonde not employed|missing data|site visit|sv window", flag_down)), 0, 1)) %>%
  #   #arrange(timestamp) %>%
  #   mutate(overlapping_flag = zoo::rollapply(flag_binary, width = width_fun, FUN = check_2_hour_window_fail, fill = NA, align = "center")) %>%
  #   mutate(cleaner_flag = ifelse(!is.na(flag) & !grepl("seasonal range|repeat|sonde not employed|missing data|site visit|sv window", flag) & overlapping_flag == TRUE, NA, flag)) %>%
  #   select(-c(flag_up, flag_down, site_up, site_down, flag_binary, overlapping_flag))

  return(df_test)

}
