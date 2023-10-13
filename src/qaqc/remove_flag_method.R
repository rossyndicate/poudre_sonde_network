# Remove flag methods
## This uses the data that is generated from the function generate_daily_flag_plots()
## to ask the user whether or not a flagged data point should pass or fail.

# get flag decision function ----
# returns a binary TRUE or FALSE depending on the input from the user
get_flag_decision <- function(prompt_text) {
  while (TRUE) {
    user_input <- readline(prompt = paste(prompt_text, " (pass/fail): "))
    user_input <- tolower(user_input)

    if (user_input %in% c("pass", "p")) {
      return(TRUE)
    } else if (user_input %in% c("fail", "f")) {
      return(FALSE)
    } else {
      cat("Invalid input. Please enter 'pass' or 'fail'.\n")
    }
    # need to add skip option later
  }
}

# alter verification column ----
# this does not work yet
## This is the function that we want to pass into the next function to alter
## the verification column after the user inputs pass or fail
alter_verification_column <- function(df, dt, choice_arg = choice) {

  row_condition <- as.character(df$DT_round) == dt

  if (choice_arg) {
    df <- df %>%
      mutate(verification = ifelse(row_condition, "pass", verification))
  } else {
    df <- df %>%
      mutate(verification = ifelse(row_condition, "fail", verification))
  }

  return(df)

}

# Verify flag data function and testing ----
## Right now this function takes in a ggplot object, extracts its data and then
## makes a plot for each point of interest to ask the user if it is valid point
## or not. As it is how the user answers has not effect on any data.

# there will need to be some sort of imap so that we can verify that the weekly data and the daily data are the same
verify_flag_data <- function(weekly_plot_object, daily_plot_object) {

  # filter the data from the plot object to get any points where the
  # verification column is NA
  df_data <- ggplot_build(daily_plot_object)$plot$data
  flag_data <- df_data %>%
    filter(is.na(verification))

  # pull the site-param combo and inform the user what they are working on
  flag_site <- flag_data$site[1]
  flag_parameter <- flag_data$parameter[1]

  # which df are you working on:
  cat("Currently verifying: ", flag_site, flag_parameter, " data.\n")

  # initialize the df_join as an empty df to join to the df_data later
  df_join <- data.frame()

  for (i in 1:nrow(flag_data)){

    # Create a data frame with a single row containing the desired DT_round value
    flag_demarcation_data <- data.frame(DT_round = flag_data$DT_round[i])

    day_plot <- daily_plot_object +
      geom_vline(data = flag_demarcation_data,
                 aes(xintercept = DT_round,
                     color = "data point of interest",
                     # size = 2, # fix this later
                     alpha = 0.05))

    week_plot <- weekly_plot_object +
      geom_vline(data = flag_demarcation_data,
                 aes(xintercept = DT_round,
                     color = "data point of interest",
                     # size = 2, # fix this later
                     alpha = 0.05))

    ### add the weekly data for verification as well
    stacked_plot <- ggarrange(week_plot, day_plot, nrow = 2, ncol = 1)

    # Print the plot so the user can see the data
    print(stacked_plot)

    # Generate the prompt for the user to answer
    flag_dt <- as.character(flag_data$DT_round[i])

    choice_prompt <- paste(flag_site, flag_parameter, "at", flag_dt)

    choice <- get_flag_decision(choice_prompt)

    # alter the df based on the response from the user
    if (choice) { # if choice is true
      cat(flag_site, flag_parameter, " at ", flag_dt, " has passed.\n")
      # add pass to verification column
      choice_df <- alter_verification_column(df = flag_demarcation_data, dt = flag_dt, choice_arg = TRUE)
    } else { # if choice is false
      cat(flag_site, flag_parameter, " at ", flag_dt, " has failed.\n")
      # add fail
      choice_df <- alter_verification_column(df = flag_demarcation_data, dt = flag_dt, choice_arg = FALSE)
    }

    # append choice_df to df_join
    df_join <- bind_rows(df_join, choice_df)
  }

  # Inform the user that they are done verifying which ever df
  cat("Finished verifying: ", flag_site, flag_parameter, " data.\n")

  # Join the original data with the decisions that the user made
  altered_df <- df_data %>%
    left_join(df_join, by = "DT_round") %>%
    mutate(verification = coalesce(verification.x, verification.y)) %>%
    select(!c(verification.x, verification.y))

  # Return the df that has verification column altered
  return(altered_df)

}

# Examples ----

# Right now this takes in a list of plots, I don't love this but for now its the
# easiest way to quickly get to the data that has flagged data.

# Generating the list of plots (this example only has one plot that has a repeated value flag)
# we are doing it this way so that we can get the data from the daily ggplot
test_weekly <- generate_weekly_flag_plots("archery", "Temperature", "slope violation")
test_daily <- generate_daily_flag_plots("archery", "Temperature", "slope violation")

# Using the list of plots that we generated to verify just one of the dfs in the list

# maybe we will use map2() so that it can take the input from both and then combine them?
verify_flag_data(test_weekly[[2]], test_daily[[2]])

# Using the function that was made to map over the dfs in the list
# make sure to put in the weekly information first
test_map <- map2(head(test_weekly), head(test_daily), verify_flag_data)

# For now only the daily plots are generated, but this can change in the future.
