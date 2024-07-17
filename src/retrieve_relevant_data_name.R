retrieve_relevant_data_name <- function(df_name_arg, year_week_arg) {

  if (df_name_arg %in% names(verified_data) & year_week_arg %in% verified_data[[df_name_arg]]$y_w) {
    return("verified_data")
  }

  if (df_name_arg %in%  names(intermediary_data) & year_week_arg %in% intermediary_data[[df_name_arg]]$y_w) {
    return("intermediary_data")
  }

  if (df_name_arg %in% names(all_data) & year_week_arg %in% all_data[[df_name_arg]]$y_w) {
      return("all_data")
  }

}
