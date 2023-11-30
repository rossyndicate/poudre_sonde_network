# to do (j): document this function
get_start_dates_df <- function(incoming_flagged_data_dfs) {
    find_start_dates <- map(incoming_flagged_data_dfs, function(df) {max(df$DT_round)})
    start_dates_df <- tibble(site = names(incoming_flagged_data_dfs), start_DT_round = unlist(find_start_dates)) %>%
                        mutate(
                          # pull the site name from the index
                          site = sub("-.*", "", site),
                          # convert the Unix timestamp to a POSIXct object
                          start_DT_round = as.POSIXct(.$start_DT_round, origin = "1970-01-01") # is this the right tz?
                          ) %>%
                        # summarize and find the earliest date for each site
                        group_by(site) %>%
                        summarize(start_DT_round = as.character(min(start_DT_round)))
    return(start_dates_df)
}
