# Get day decision.
# @param prompt_text A string for the prompt text.
# @return A string of "pass", "fail", or "inspect" depending on input from the user.
# @examples
# get_day_decision(prompt_text = "Would you like to (pass/fail/inspect) all data points for: ") 

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