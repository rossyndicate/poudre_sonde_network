save_intermediary_dir <- function(updated_df, df_name) {
  # if updated_site_param_df is from all_data, but there is already intermediary data for it, refuse to move on
  if (all(!updated_df$is_verified) & df_name %in% list.files(intermediary_path)) {
    while (TRUE) {
      cat("!!!YOU ARE ABOUT TO OVERWRITE INTERMEDIARY DATA WITH PRE-VERIFICATION DATA!!!")
      user_input <- readline(prompt = "Are you sure you want to continue with this action? (yes/no):")
      user_input <- tolower(user_input)

      if (user_input == "yes") {
        saveRDS(updated_df, paste0(intermediary_path, df_name))
        break
      }

      if (user_input == "no") {
        working_data <<- set_names(map(list.files(intermediary_path, full.names = TRUE), readRDS), list.files(intermediary_path))
        updated_site_param_df <<- working_data[[site_param_name]] # working dir df
        saveRDS(updated_site_param_df, paste0(intermediary_path, df_name))
        break
      }

      cat("Invalid input. Please enter one of the options from the following:\n\n")
      cat("yes\n")
      cat("no\n")
    }
  }

  # if updated_site_param_df is from verified data, refuse to move on
  if (df_name %in% list.files(verified_path)) { # test to see if this works
    while (TRUE) {
      cat("!!!YOU ARE ABOUT TO WORK ON DATA THAT HAS BEEN FINALIZED!!!")
      user_input <- readline(prompt = "Are you sure you want to continue with this action? (yes/no):")
      user_input <- tolower(user_input)

      if (user_input == "yes") {
        saveRDS(updated_df, paste0(intermediary_path, df_name))
        break
      }

      if (user_input == "no") {
        cat("Please update your `site` and `parameter` objects to data that has not been finalized.")
        break
      }

      cat("Invalid input. Please enter one of the options from the following:\n\n")
      cat("yes\n")
      cat("no\n")
    }
  }

  saveRDS(updated_df, paste0(intermediary_path, df_name))
}
