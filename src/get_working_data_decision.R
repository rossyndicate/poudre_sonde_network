#' @title Get working data decision
#'
#' @description
#' Get working directory and data decisions from user input
#'
#' @param prompt_text The prompt that the user is replying to
#'
#' @examples
#' # get_day_decision(prompt_text = "Would you like to (pass/fail/inspect) all data points for: ")

get_working_data_decision <- function() { # should add a layer level to this to prevent user from inspect sub-daily data

  prompt_text <- "Which directory are you working from? (pre/int): "

  while (TRUE) {
    user_input <- readline(prompt = paste(prompt_text))
    user_input <- tolower(user_input)

    if (user_input == "pre") {
      working_data <<- set_names(map(list.files(pre_verification_path, full.names = TRUE), readRDS), list.files(pre_verification_path))
      break()
    }

    if (user_input == "int") {
      working_data <<- set_names(map(list.files(intermediary_path, full.names = TRUE), readRDS), list.files(intermediary_path))
      break()
    }

    cat("Invalid input. Please enter one of the options from the following:\n\n")
    cat("pre = pre_verification_dir\n")
    cat("int = intermediary_dir\n")
  }

}
