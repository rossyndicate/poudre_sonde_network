#' @title Good-ish data preparing method
#'
#' @description
#' This is basically the method that we use to prepare the data that has not
#' been manually verified.
#'

prep_data_for_thresholds <- function(df, site_vector, param_vector) {

  prepped_df <- df %>%
    fcw.qaqc::fix_site_names() %>%
    # All flags were added to these data, so we need to remove the generated threshold flag for this step
    mutate(auto_flag = str_remove_all(auto_flag, "outside of seasonal range|slope violation")) %>%
    # Clean up the flag column
    fcw.qaqc::tidy_flag_column() %>%
    # Filtering the data for what we care about for the thresholds
    mutate(auto_flag = ifelse(auto_flag == "", NA, auto_flag),
           mal_flag = ifelse(mal_flag == "", NA, mal_flag),
           mean = case_when(
             !is.na(auto_flag) ~ NA,
             sonde_employed == 1 ~ NA,
             !is.na(sonde_moved) ~ NA,
             !is.na(mal_flag) ~ NA,
             TRUE ~ mean
           ),
           DT_join = as.character(format(as.POSIXct(DT_join), "%Y-%m-%d %H:%M:%S"))
    ) %>%
    select(DT_round, DT_join, site, parameter, mean, flag = auto_flag)

  return(prepped_df)
}
