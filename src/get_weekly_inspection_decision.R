get_weekly_inspection_decision <- function(weekly_plot_data) {

  prompt_text <- "Which days do you want to verify (1/2/3/4/5/6/7)? \nCan be any combination of weekdays, with no other characters.\nex: 456\n"
  day_min <- min(weekly_plot_data$weekday)
  day_max <- max(weekly_plot_data$weekday)

  while (TRUE) {

    user_input_og <- readline(prompt = paste(prompt_text))

    user_input <- suppressWarnings(
      sort(na.omit(as.numeric(unique(unlist(strsplit(user_input_og, ""))))) [as.numeric(unique(unlist(strsplit(user_input_og, "")))) >= day_min & as.numeric(unique(unlist(strsplit(user_input_og, "")))) <= day_max])
      ) # 1 and 7 will be set to data min and max

    if (length(user_input) != 0) {
      return(user_input)
    }

    cat(user_input_og, " is not a valid input")
  }
}
