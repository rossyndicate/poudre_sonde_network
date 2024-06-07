#' @title Get verification decision
#'
#' @description
#' Get decisions from user input
#'
#' @param prompt_text The prompt that the user is replying to
#'
#' @examples
#' # get_day_decision(prompt_text = "Would you like to (pass/fail/inspect) all data points for: ")

get_verification_decision <- function(prompt_text) { # should add a layer level to this to prevent user from inspect sub-daily data

  while (TRUE) {
    user_input <- readline(prompt = paste(prompt_text))
    user_input <- tolower(user_input)

    # pass statements
    if (user_input %in% c("pass all", "pass", "pa", "p")) {
      return("PASS ALL")
    }
    if (user_input %in% c("pass valid", "pv")) {
      return("PASS VALID")
    }
    if (user_input %in% c("pass flagged", "pf")) {
      return("PASS FLAGGED")
    }
    if (user_input %in% c("pass none", "pn")) {
      return("PASS NONE")
    }

    # fail statements
    if (user_input %in% c("fail all", "fail", "fa", "f")) {
      return("FAIL ALL")
    }
    if (user_input %in% c("fail valid", "fv")) {
      return("FAIL VALID")
    }
    if (user_input %in% c("fail flagged", "ff")) {
      return("FAIL FLAGGED")
    }
    if (user_input %in% c("fail none", "fn")) {
      return("FAIL NONE")
    }

    # skip statements
    if (user_input %in% c("skip", "skip all", "s", "sa")) {
      return("SKIP")
    }

    # inspect statements ***
    if (user_input %in% c("inspect all", "inspect", "ia", "i")){
      return("INSPECT ALL")
    }

    if (user_input %in% c("inspect valid", "iv")){
      return("INSPECT VALID")
    }

    if (user_input %in% c("inspect flagged", "if")){
      return("INSPECT FLAGGED")
    }

    if(user_input %in% c("inspect some", "is")){ # if we are at daily level we can't go further
      return("INSPECT SOME")
    }

    # quit statement
    if (user_input %in% c("quit", "q")) {
      return("QUIT")
    }

    cat("Invalid input. Please enter one of the options from the following decision matrix:\n\n")
    cat("          | PASS | FAIL | SKIP | INSPECT  |  | QUIT |\n")
    cat("----------+------+------+------+----------|  |------|\n")
    cat(" ALL      | pa   | fa   | sa   | ia       |  | q    |\n")
    cat(" VALID    | pv   | fv   |      | iv       |  +------+\n")
    cat(" FLAGGED  | pf   | ff   |      | if       |\n")
    cat(" NONE     | pn   | fn   |      |          |\n")
    cat(" SOME     |      |      |      | is       |\n")
    cat("----------+------+------+------+----------|\n")
  }

}
