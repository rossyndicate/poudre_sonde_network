# to do (j): document this function
get_start_dates_df <- function(incoming_flagged_data_dfs) {

  DT_finder <- function(df){

    df %>%
      select(DT_round, site, parameter) %>%
      filter(DT_round == max(DT_round))

  }

  start_dates_df <- map_dfr(incoming_flagged_data_dfs, DT_finder) %>%
    group_by(site) %>%
    # use something that is always monitored:
    filter(parameter == "Temperature") %>%
    filter(DT_round == min(DT_round)) %>%
    distinct(site, .keep_all=TRUE) %>%
    select(site,
           start_DT_round = DT_round)

  return(start_dates_df)

}
