# Verify flag data
## This uses the data that is generated from the function generate_daily_flag_plots()
## to ask the user whether or not a flagged data point should pass or fail.

# get day decision function ----
# returns "pass", "fail", or "inspect depending on input from the user
get_day_decision <- function(prompt_text) {
  while (TRUE) {
    user_input <- readline(prompt = paste(prompt_text))
    user_input <- tolower(user_input)

    if (user_input %in% c("pass", "p")) {
      return("pass")
    } else if (user_input %in% c("fail", "f")) {
      return("fail")
    } else if (user_input %in% c("inspect", "i")){
      return("inspect")
    } else {
      cat("Invalid input. Please enter 'yes', 'no', or 'inspect'.\n")
    }
  }
}

# get flag decision function ----
# returns a binary TRUE or FALSE depending on the input from the user
get_flag_decision <- function(prompt_text) {
  while (TRUE) {
    user_input <- readline(prompt = paste(prompt_text))
    user_input <- tolower(user_input)

    if (user_input %in% c("pass", "p")) {
      return(TRUE)
    } else if (user_input %in% c("fail", "f")) {
      return(FALSE)
    } else {
      cat("Invalid input. Please enter 'pass' or 'fail'.\n")
    }
  }
}

# Verify flag data function and testing ----
## Right now this function takes in a ggplot object, extracts its data and then
## makes a plot for each point of interest to ask the user if it is valid point
## or not. As it is how the user answers has no effect on any generated data.
## Instead, new dfs will be generated with the users decisions.

# there will need to be some sort of imap so that we can verify that the weekly data and the daily data are the same
verify_flag_data <- function(weekly_plot_object, daily_plot_object) {

  # get data from daily plot
  df_data <- ggplot_build(daily_plot_object)$plot$data
  df_name <- ggplot_build(daily_plot_object)$plot$labels$title

  # which df are you working on:
  cat("Currently verifying: ", df_name, " data.\n")

  print(ggarrange(weekly_plot_object, daily_plot_object, nrow = 2, ncol = 1))
  # Generate the prompt for the user to answer
  day_choice_prompt <- paste("Would you like to (pass/fail/inspect) all data points for: ", df_name, "? (p/f/i): ")
  day_choice <- get_day_decision(day_choice_prompt)

  # alter the df based on the response from the user
  if (day_choice == "pass") { # if choice is true
    # add pass to verification column for all rows
    altered_df <- df_data %>%
      mutate(verification = ifelse(is.na(verification), "pass", verification),
             mean_verified = ifelse(verification == "pass", mean, NA)) %>%
      relocate(mean_verified, .after = "mean_public")
    cat("All points for ", df_name, "have PASSED.\n")
    return(altered_df)
  } else if (day_choice == "fail") { # if choice is false
    # add fail to verification column for all rows
    altered_df <- df_data %>%
      # do we want any points to automatically pass? we already have some points that automatically fail
      # ex: missing data
      mutate(verification = ifelse(is.na(verification), "fail", verification),
             mean_verified = ifelse(verification == "pass", mean, NA)) %>%
      relocate(mean_verified, .after = "mean_public")
    cat("All points for ", df_name, "have FAILED.\n")
    return(altered_df)
  } else if (day_choice == "inspect") {

    # pull the flagged data and iterate through each row
    flag_data <- df_data %>%
      filter(is.na(verification))

    for (i in 1:nrow(flag_data)){
      # Create a data frame with a single row containing the desired DT_round value
      # and add it to the week plot and day plot
      flag_demarcation_data <- data.frame(DT_round = flag_data$DT_round[i])

      week_plot <- weekly_plot_object +
        geom_vline(data = flag_demarcation_data,
                   aes(xintercept = DT_round,
                       color = "data point of interest",
                       # size = 2, # fix this later
                       alpha = 0.05))

      day_plot <- daily_plot_object +
        geom_vline(data = flag_demarcation_data,
                   aes(xintercept = DT_round,
                       color = "data point of interest",
                       # size = 2, # fix this later
                       alpha = 0.05))

      # Print the plot so the user can see the data
      print(ggarrange(week_plot, day_plot, nrow = 2, ncol = 1))

      # Generate the prompt for the user to answer
      flag_dt <- as.character(flag_data$DT_round[i])
      flag_choice_prompt <- paste(flag_dt, " (pass/fail): ")
      flag_choice <- get_flag_decision(flag_choice_prompt)

      df_data <- df_data %>%
        mutate(verification = ifelse(as.character(DT_round) == flag_dt, ifelse(flag_choice, "pass", "fail"), verification),
               mean_verified = ifelse(verification == "pass", mean, NA)) %>%
        relocate(mean_verified, .after = "mean_public")
    }
    # Inform the user that they are done verifying which ever df
    cat("Finished verifying: ", df_name, " data.\n")

    # return the altered df
    altered_df <- df_data

    # Return the df that has verification column altered
    return(altered_df)
  }
}
