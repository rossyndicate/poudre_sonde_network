#' @title Remove trailing semicolons
#' 
#' @description
#' Function to remove ";" or "; " at the end of a string
#' 
#' @param text A character string.
#' 
#' @return A character string with the trailing semicolons removed.
#' 
#' @examples
#' remove_trailing_semicolons("This is a sentence;")

remove_trailing_semicolons <- function(text) {
  text %>%
    str_replace_all(";\\s*$", "") %>%  # Remove "; " at the end of the string
    str_replace_all(";$", "")          # Remove ";" at the end of the string
}
