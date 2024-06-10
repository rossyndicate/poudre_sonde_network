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
    update <- bind_rows(update)

    # update site param df
    matching_rows <- updated_site_param_df$DT_join %in% update$DT_join
    updated_site_param_df[matching_rows, ] <- update[update$DT_join %in% updated_site_param_df$DT_join, ]

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
