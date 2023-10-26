# Get flag decision.
# @param prompt_text A string for the prompt text.
# @return A boolean of TRUE or FALSE depending on input from the user.
# @examples
# get_flag_decision(prompt_text = "Would you like to (pass/fail) this data point: ")

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
