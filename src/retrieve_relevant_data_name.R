retrieve_relevant_data_name <- function(df_name_arg, year_week_arg = NULL, interval_arg = NULL) {
  if(is.null(interval_arg)){
    if (df_name_arg %in% names(verified_data) & any(year_week_arg %in% verified_data[[df_name_arg]]$y_w)) {
      return("verified_data")
    }
    if (df_name_arg %in% names(intermediary_data) & any(year_week_arg %in% intermediary_data[[df_name_arg]]$y_w)) {
      return("intermediary_data")
    }
    if (df_name_arg %in% names(all_data) & any(year_week_arg %in% all_data[[df_name_arg]]$y_w)) {
      return("all_data")
    }
  }
  if(is.null(year_week_arg)){
    if (df_name_arg %in% names(verified_data) & any(verified_data[[df_name_arg]]$DT_round %within% interval_arg)) {
      return("verified_data")
    }
    if (df_name_arg %in% names(intermediary_data) & any(intermediary_data[[df_name_arg]]$DT_round %within% interval_arg)) {
      return("intermediary_data")
    }
    if (df_name_arg %in% names(all_data) & any(all_data[[df_name_arg]]$DT_round %within% interval_arg)) {
      return("all_data")
    }
  }
}
