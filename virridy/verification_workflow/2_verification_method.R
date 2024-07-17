for (i in weekly_plot_objects) {

  if(QUIT){
    break()
  }

  update <- verify_flag_data(
    df_list_arg = all_data,
    site_arg = site,
    parameter_arg = parameter,
    flag_arg = NULL,
    weekly_plot_object = i
  )

  if (!is.null(update)){

    if(is_tibble(update)) {
      update <- bind_rows(update)
    } else {
      if (length(update) != 51) { # 46 is because sometimes dfs decompose into lists... idk why -jd
        update <-  update %>%
          # remove NULL values from the list
          keep(~ !is.null(.)) %>%
          # remove empty dfs from the list
          keep(~ nrow(.)>0) %>%
          bind_rows(.)
      } else {
        update <- bind_rows(update)
      }
    }

    # update site param df
    updated_site_param_df <- updated_site_param_df %>%
      mutate(
        mean_verified = as.double(mean_verified),
        verification_status = as.character(verification_status)) %>%
      rows_update(update, by = "DT_join")

    # update the saved data in the intermediary dir
    saveRDS(object = updated_site_param_df, file = paste0(intermediary_path, site_param_name))

    graphics.off()
    gc()
  } else {
    next
  }
}

# Quit toggle
QUIT <- FALSE

# check data
updated_df <- readRDS(paste0(intermediary_path, site_param_name))

# clean the directories
clean_directories()
