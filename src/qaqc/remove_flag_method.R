# Remove flag methods
# for every flag in a df:
#   - save the df as a separate object (flags altered object)
#   - display the stacked ggplot with the point of interest highlighted
#   - Give an option in the console to remove it with the DT of the point
#   - make a column that holds pass/fail information

# get flag decision function ----
get_flag_decision <- function(prompt_text) {
  while (TRUE) {
    user_input <- readline(prompt = paste(prompt_text, " (yes/no): "))
    user_input <- tolower(user_input)

    if (user_input %in% c("yes", "y")) {
      return(TRUE)
    } else if (user_input %in% c("no", "n")) {
      return(FALSE)
    } else {
      cat("Invalid input. Please enter 'yes' or 'no'.\n")
    }
    # need to add skip option later
  }
}

# testing ----
test_daily <- generate_daily_flag_plots("archery", "Temperature", "slope flag suspect")

display_plots <- function(plot_object) {

  # for each plot object in a list print the plot for each item !is.na(flag)
  flag_data <- ggplot_build(plot_object)$plot$data %>%
    filter(is.na(verification))

  # which df are you working on:
  print("you are working on x df")

  for (i in 1:nrow(flag_data)){

    # Create a data frame with a single row containing the desired DT_round value
    flag_demarcation_data <- data.frame(DT_round = flag_data$DT_round[i])

    new_plot <- plot_object +
      geom_vline(data = flag_demarcation_data,
                 aes(xintercept = DT_round,
                     color = "data point of interest",
                     # size = 2, # fix this later
                     alpha = 0.05))

    print(new_plot)

    flag_site <- toupper(flag_data$site[i])
    flag_parameter <- toupper(flag_data$parameter[i])
    flag_dt <- as.character(flag_data$DT_round[i])

    choice_prompt <- paste("Remove flag for", flag_site, flag_parameter, "at", flag_dt, "?")

    choice <- get_flag_decision(choice_prompt)

    if (choice) { # if choice is true
      cat(flag_site, flag_parameter, " at ", flag_dt, " has passed.\n")
      # add pass for
    } else { # if choice is false
      cat(flag_site, flag_parameter, " at ", flag_dt, " has failed.\n")
      # add fail
    }

  }

  # which df are you working on:
  print("you are finished working on x df")

}

display_plots(test_daily[[10]])
