# to do (j): document this function
get_start_dates_df <- function(incoming_flagged_data_dfs) {
    find_start_dates <- map(incoming_flagged_data_dfs, function(df) {tail(df$DT_round, n = 1)})
    start_dates_df <- tibble(site = names(incoming_flagged_data_dfs), last_DT_round = unlist(find_start_dates)) %>%
                      mutate(
                        # pull the site name from the index
                        site = sub("-.*", "", site),
                        # convert the Unix timestamp to a POSIXct object
                        last_DT_round = as.POSIXct(.$last_DT_round, origin = "1970-01-01")
                        ) %>%
                      # summarize and find the earliest date for each site
                      group_by(site) %>%
                      summarize(last_DT_round = as.character(min(last_DT_round)))
    return(start_dates_df)
}