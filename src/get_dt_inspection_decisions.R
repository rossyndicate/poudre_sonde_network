get_dt_inspection_decisions <- function(daily_plot_data) {
  # Create a sequence of date times from the daily plot data
  dt_list <- sort(unique(pull(daily_plot_data, DT_round)))

  # Get the date
  plot_date <- as_date(daily_plot_data$DT_round[1])

  # Prompt for the number of intervals
  prompt_text <- paste("How many intervals would you like to inspect? (Must be less than", length(dt_list), ")\n")

  while (TRUE) {
    user_input <- readline(prompt = prompt_text)

    # The user input needs to check for:
    # Only numbers between 1 and length(dt_list)
    num_intervals <- suppressWarnings(as.numeric(user_input))

    if (!is.na(num_intervals) && num_intervals > 0 && num_intervals < length(dt_list)) {
      break
    }

    cat("Invalid input. Please enter a valid number of intervals.\n")
  }

  selected_intervals <- list()

  for (i in 1:num_intervals) {
    prompt_text <- paste("Enter the time range for interval", i, "in the format 'HH:MM:SS-HH:MM:SS':\n")

    while (TRUE) {
      user_input <- readline(prompt = prompt_text) # these need to be 15 minute intervals ***
      interval <- unlist(strsplit(user_input, "-"))

      interval_set <- paste(plot_date, interval)
      interval_set <- as_datetime(interval_set, tz = "MST")

      if (length(interval_set) == 2 && all(interval_set %in% dt_list)) {
        start_time <- interval_set[1]
        end_time <- interval_set[2]

        # Check if selected_intervals is empty
        if (start_time <= end_time & length(selected_intervals) == 0) {
          selected_intervals[[i]] <- interval(start_time, end_time)
          break
        }

        if (start_time <= end_time & !suppressWarnings(any(map(selected_intervals, ~int_overlaps(interval(start_time, end_time), .x))))) {
          selected_intervals[[i]] <- interval(start_time, end_time)
          break
        }
      }

      cat("Invalid input. Please enter a valid time range that doesn't overlap with previous intervals.\n")
    }
  }

  return(selected_intervals)
}
