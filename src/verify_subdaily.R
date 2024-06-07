verify_subdaily <- function(daily_verification_decision_arg,
                            daily_plot_data_arg,
                            daily_plot_object_arg,
                            weekly_plot_object_arg){

  # set layer
  layer <- "subdaily"

  # inspect all
  if (daily_verification_decision_arg == "INSPECT ALL") {

    for (j in 1:nrow(daily_plot_data_arg)) {

      # create a data frame with a single row containing the desired DT_round value
      # and add it to the week plot and day plot
      flag_demarcation_data <- tibble(DT_round = daily_plot_data_arg$DT_round[j])

      # ignore flags that you cannot alter
      daily_plot_object_new <- daily_plot_object_arg +
        geom_vline(data = flag_demarcation_data,
                   aes(xintercept = DT_round,
                       color = "data point of interest",
                       # size = 2, # fix this later
                       alpha = 0.05))

      weekly_plot_object_new <- weekly_plot_object_arg +
        geom_vline(data = flag_demarcation_data,
                   aes(xintercept = DT_round,
                       color = "data point of interest",
                       # size = 2, # fix this later
                       alpha = 0.05))

      # print ggarrange daily and weekly plots
      print(ggarrange(daily_plot_object_new, weekly_plot_object_new, nrow = 2, ncol = 1))

      # generate the prompt for the user to answer
      # the user should not be able to inspect again
      day_dt <- daily_plot_data_arg$DT_join[j] # reformat so that this reads as a date
      dt_choice_prompt <- paste("Would you like to (pass/fail/skip) ", day_dt, "?\n(layer~", layer, ")(px/fx/sx)")
      dt_verification_decision <- get_verification_decision(dt_choice_prompt)

      # Handle the case when the user selects an "INSPECT" or "QUIT" option inappropriately
      while (layer == "subdaily" & dt_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME", "QUIT")) {
        if (dt_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME")){
          cat("You cannot inspect at this level. Please choose a different option.\n")
          dt_verification_decision <- get_verification_decision(dt_choice_prompt)
        }
        if (dt_verification_decision == "QUIT"){
          cat("You cannot quit during this step. Please choose a different option.\n")
          dt_verification_decision <- get_verification_decision(dt_choice_prompt)
        }
      }

      # update the specific row in df_daily_data based on the index
      row_index <- which(daily_plot_data_arg$DT_join == day_dt)
      daily_plot_data_arg[row_index, ] <- alter_verification_column(dt_verification_decision, daily_plot_data_arg[row_index, ])
    }
    # inform the user that they are done verifying which ever df
    cat("Finished verifying: ", as.character(min(daily_plot_data_arg$DT_round)), " data.\n")

  }

  # inspect valid
  if (daily_verification_decision_arg == "INSPECT VALID") {

    # get the dt of the valid data to inspect to get indexes of that data
    valid_daily_plot_data_dt_list <- daily_plot_data_arg %>%
      filter(is.na(flag)) %>%
      pull(DT_join)

    # get the dt of the flagged data to get the non-inspect decision
    new_dt_list <- pull(daily_plot_data_arg, DT_join)
    non_dts_to_inspect <- new_dt_list[!new_dt_list %in% valid_daily_plot_data_dt_list]

    # decide what to do with the flagged data ----
    # for each non-inspect dt decision:
    if (length(non_dts_to_inspect) > 0) {

      # get non-inspect day decision:
      noninspect_prompt_text = paste("Would you like to (pass/fail/skip) non-inspect dts?\n(layer~", layer, ")(px/fx/sx): ")
      non_inspect_verification_decision <- get_verification_decision(noninspect_prompt_text)

      # Handle the case when the user selects an "INSPECT" or "QUIT" option inappropriately
      while (non_inspect_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME", "QUIT")) {
        if (non_inspect_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME")){
          cat("You cannot inspect non-inspect dts. Please choose a different option.\n")
          non_inspect_verification_decision <- get_verification_decision(noninspect_prompt_text)
        }
        if (non_inspect_verification_decision == "QUIT"){
          cat("You cannot quit during this step. Please choose a different option.\n")
          non_inspect_verification_decision <- get_verification_decision(noninspect_prompt_text)
        }
      }

      for (j in non_dts_to_inspect) {
        # j <- format(as.POSIXct(j, tz = "MST"), "%Y-%m-%d %H:%M:%S")

        j_index <- which((daily_plot_data_arg$DT_join) == j)

        daily_plot_data_arg[j_index, ] <- alter_verification_column(non_inspect_verification_decision, daily_plot_data_arg[j_index, ])

        # some sort of print statement to show what happened to the non-target dts
      }
    }

    # update those flagged data in the df

    for (j in valid_daily_plot_data_dt_list) {

      # j <- format(as.POSIXct(j, tz = "MST"), "%Y-%m-%d %H:%M:%S")

      j_index <- which((daily_plot_data_arg$DT_join) == j)

      # create a data frame with a single row containing the desired DT_round value
      # and add it to the week plot and day plot
      flag_demarcation_data <- tibble(DT_round = daily_plot_data_arg$DT_round[j_index])

      daily_plot_object_new <- daily_plot_object_arg +
        geom_vline(data = flag_demarcation_data,
                   aes(xintercept = DT_round,
                       color = "data point of interest",
                       # size = 2, # fix this later
                       alpha = 0.05))

      weekly_plot_object_new <- weekly_plot_object_arg +
        geom_vline(data = flag_demarcation_data,
                   aes(xintercept = DT_round,
                       color = "data point of interest",
                       # size = 2, # fix this later
                       alpha = 0.05))

      # print ggarrange daily and weekly plots
      print(ggarrange(daily_plot_object_new, weekly_plot_object_new, nrow = 2, ncol = 1))

      # generate the prompt for the user to answer
      # the user should not be able to inspect again
      day_dt <- as.character(daily_plot_data_arg$DT_round[j_index]) # reformat so that this reads as a date
      dt_choice_prompt <- paste("Would you like to (pass/fail/skip) ", day_dt, "?\n(layer~", layer, ")(px/fx/sx)")
      dt_verification_decision <- get_verification_decision(dt_choice_prompt)

      # Handle the case when the user selects an "INSPECT" or "QUIT" option inappropriately
      while (layer == "subdaily" & dt_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME", "QUIT")) {
        if (dt_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME")){
          cat("You cannot inspect at this level. Please choose a different option.\n")
          dt_verification_decision <- get_verification_decision(dt_choice_prompt)
        }
        if (dt_verification_decision == "QUIT"){
          cat("You cannot quit during this step. Please choose a different option.\n")
          dt_verification_decision <- get_verification_decision(dt_choice_prompt)
        }
      }

      # update the specific row in df_daily_data based on the index
      # row_index <- which(as.character(daily_plot_data_arg$DT_round) == day_dt)
      daily_plot_data_arg[j_index, ] <- alter_verification_column(dt_verification_decision, daily_plot_data_arg[j_index, ])
    }
    # inform the user that they are done verifying which ever df
    cat("Finished verifying: ", as.character(min(daily_plot_data_arg$DT_round)), " data.\n")
    # return the altered df
    # altered_df_list_arg[[i]] <- daily_plot_data_arg # make sure that this is the updated data frame

  }
  # inspect flagged
  if (daily_verification_decision_arg == "INSPECT FLAGGED") {

    # get the dt of the flagged data to inspect to get indexes of that data
    flagged_daily_plot_data_dt_list <- daily_plot_data_arg %>%
      filter(!is.na(flag)) %>%
      pull(DT_join)

    # get the dt of the flagged data to get the non-inspect decision
    new_dt_list <- pull(daily_plot_data_arg, DT_join)
    non_dts_to_inspect <- new_dt_list[!new_dt_list %in% flagged_daily_plot_data_dt_list]

    # decide what to do with the flagged data ----
    # for each non-inspect dt decision:
    if (length(non_dts_to_inspect) > 0) {

      # get non-inspect day decision:
      noninspect_prompt_text = paste("Would you like to (pass/fail/skip) non-inspect dts?\n(layer~", layer, ")(px/fx/sx): ")
      non_inspect_verification_decision <- get_verification_decision(noninspect_prompt_text)

      # Handle the case when the user selects an "INSPECT" or "QUIT" option inappropriately
      while (non_inspect_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME", "QUIT")) {
        if (non_inspect_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME")){
          cat("You cannot inspect non-inspect dts. Please choose a different option.\n")
          non_inspect_verification_decision <- get_verification_decision(noninspect_prompt_text)
        }
        if (non_inspect_verification_decision == "QUIT"){
          cat("You cannot quit during this step. Please choose a different option.\n")
          non_inspect_verification_decision <- get_verification_decision(noninspect_prompt_text)
        }
      }

      for (j in non_dts_to_inspect) {
        # j <- format(as.POSIXct(j, tz = "MST"), "%Y-%m-%d %H:%M:%S") # time zone

        j_index <- which((daily_plot_data_arg$DT_join) == j)

        daily_plot_data_arg[j_index, ] <- alter_verification_column(non_inspect_verification_decision, daily_plot_data_arg[j_index, ])

        # some sort of print statement to show what happened to the non-target dts
      }
    }

    # update those flagged data in the df

    for (j in flagged_daily_plot_data_dt_list) {

      # j <- format(as.POSIXct(j, tz = "MST"), "%Y-%m-%d %H:%M:%S")

      j_index <- which((daily_plot_data_arg$DT_join) == j)

      # create a data frame with a single row containing the desired DT_round value
      # and add it to the week plot and day plot
      flag_demarcation_data <- tibble(DT_round = daily_plot_data_arg$DT_round[j_index])

      daily_plot_object_new <- daily_plot_object_arg +
        geom_vline(data = flag_demarcation_data,
                   aes(xintercept = DT_round,
                       color = "data point of interest",
                       # size = 2, # fix this later
                       alpha = 0.05))

      weekly_plot_object_new <- weekly_plot_object_arg +
        geom_vline(data = flag_demarcation_data,
                   aes(xintercept = DT_round,
                       color = "data point of interest",
                       # size = 2, # fix this later
                       alpha = 0.05))


      # print ggarrange daily and weekly plots
      print(ggarrange(daily_plot_object_new, weekly_plot_object_new, nrow = 2, ncol = 1))

      # generate the prompt for the user to answer
      # the user should not be able to inspect again
      day_dt <- as.character(daily_plot_data_arg$DT_round[j_index]) # reformat so that this reads as a date
      dt_choice_prompt <- paste("Would you like to (pass/fail/skip) ", day_dt, "?\n(layer~", layer, ")(px/fx/sx)")
      dt_verification_decision <- get_verification_decision(dt_choice_prompt)

      # Handle the case when the user selects an "INSPECT" or "QUIT" option inappropriately
      while (layer == "subdaily" & dt_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME", "QUIT")) {
        if (dt_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME")){
          cat("You cannot inspect at this level. Please choose a different option.\n")
          dt_verification_decision <- get_verification_decision(dt_choice_prompt)
        }
        if (dt_verification_decision == "QUIT"){
          cat("You cannot quit during this step. Please choose a different option.\n")
          dt_verification_decision <- get_verification_decision(dt_choice_prompt)
        }
      }

      # update the specific row in df_daily_data based on the index
      # row_index <- which(as.character(daily_plot_data_arg$DT_round) == day_dt)
      daily_plot_data_arg[j_index, ] <- alter_verification_column(dt_verification_decision, daily_plot_data_arg[j_index, ])
    }
    # inform the user that they are done verifying which ever df
    cat("Finished verifying: ", as.character(min(daily_plot_data_arg$DT_round)), " data.\n")

  }
  # inspect some
  if (daily_verification_decision_arg == "INSPECT SOME") { # asdf

    # get which dts to inspect
    dt_intervals_to_inspect <- get_dt_inspection_decisions(daily_plot_data = daily_plot_data_arg)

    # decided what to do with the non-inspect data
    new_dt_list <- pull(daily_plot_data_arg, DT_round)
    non_dts_to_inspect <- new_dt_list[!new_dt_list %within% dt_intervals_to_inspect]

    # decide what to do with the flagged data ----
    # for each non-inspect dt decision:
    if (length(non_dts_to_inspect) > 0) {

      # get non-inspect day decision:
      noninspect_prompt_text = paste("Would you like to (pass/fail/skip) non-inspect dts?\n(layer~", layer, ")(px/fx/sx): ")
      non_inspect_verification_decision <- get_verification_decision(noninspect_prompt_text)

      # Handle the case when the user selects an "INSPECT" or "QUIT" option inappropriately
      while (non_inspect_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME", "QUIT")) {
        if (non_inspect_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME")){
          cat("You cannot inspect non-inspect dts. Please choose a different option.\n")
          non_inspect_verification_decision <- get_verification_decision(noninspect_prompt_text)
        }
        if (non_inspect_verification_decision == "QUIT"){
          cat("You cannot quit during this step. Please choose a different option.\n")
          non_inspect_verification_decision <- get_verification_decision(noninspect_prompt_text)
        }
      }

      for (j in non_dts_to_inspect) {
        j <- format(as.POSIXct(j, tz = "MST"), "%Y-%m-%d %H:%M:%S")

        j_index <- which((daily_plot_data_arg$DT_round) == j)

        daily_plot_data_arg[j_index, ] <- alter_verification_column(non_inspect_verification_decision, daily_plot_data_arg[j_index, ])

        # some sort of print statement to show what happened to the non-target dts
      }
    }

    # update those flagged data in the df

    for (j in dt_intervals_to_inspect) { # make sure that j can not be 0

      interval_df <- daily_plot_data_arg %>%
        filter(DT_round %within% j)

      for (k in 1:nrow(interval_df)) {

        k_index <- which((daily_plot_data_arg$DT_round) == interval_df$DT_round[k])

        # create a data frame with a single row containing the desired DT_round value
        # and add it to the week plot and day plot
        flag_demarcation_data <- tibble(DT_round = daily_plot_data_arg$DT_round[k_index])

        daily_plot_object_new <- daily_plot_object_arg +
          geom_vline(data = flag_demarcation_data,
                     aes(xintercept = DT_round,
                         color = "data point of interest",
                         # size = 2, # fix this later
                         alpha = 0.05))

        weekly_plot_object_new <- weekly_plot_object_arg +
          geom_vline(data = flag_demarcation_data,
                     aes(xintercept = DT_round,
                         color = "data point of interest",
                         # size = 2, # fix this later
                         alpha = 0.05))


        # print ggarrange daily and weekly plots
        print(ggarrange(daily_plot_object_new, weekly_plot_object_new, nrow = 2, ncol = 1))

        # generate the prompt for the user to answer
        # the user should not be able to inspect again
        day_dt <- as.character(daily_plot_data_arg$DT_round[k_index]) # reformat so that this reads as a date
        dt_choice_prompt <- paste("Would you like to (pass/fail/skip) ", day_dt, "?\n(layer~", layer, ")(px/fx/sx)")
        dt_verification_decision <- get_verification_decision(dt_choice_prompt)

        # Handle the case when the user selects an "INSPECT" or "QUIT" option inappropriately
        while (layer == "subdaily" & dt_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME", "QUIT")) {
          if (dt_verification_decision %in% c("INSPECT ALL", "INSPECT FLAGGED", "INSPECT VALID", "INSPECT SOME")){
            cat("You cannot inspect at this level. Please choose a different option.\n")
            dt_verification_decision <- get_verification_decision(dt_choice_prompt)
          }
          if (dt_verification_decision == "QUIT"){
            cat("You cannot quit during this step. Please choose a different option.\n")
            dt_verification_decision <- get_verification_decision(dt_choice_prompt)
          }
        }

        # update the specific row in df_daily_data based on the index
        # row_index <- which(as.character(daily_plot_data_arg$DT_round) == day_dt)
        daily_plot_data_arg[k_index, ] <- alter_verification_column(dt_verification_decision, daily_plot_data_arg[k_index, ])
      }
    }
    # inform the user that they are done verifying which ever df
    cat("Finished verifying: ", as.character(min(daily_plot_data_arg$DT_round)), " data.\n")
  }

  return(daily_plot_data_arg)

}
