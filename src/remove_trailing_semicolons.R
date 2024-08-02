# Function to remove ";" or "; " at the end of a string
remove_trailing_semicolons <- function(text) {
  text %>%
    str_replace_all(";\\s*$", "") %>%  # Remove "; " at the end of the string
    str_replace_all(";$", "")          # Remove ";" at the end of the string
}
