#' Strip Drift Tokens Located Outside Active Window Areas
#'
#' @description
#' Cleans up quality flag text strings by removing specific "drift" tokens while preserving other existing flags.
#'
#' @param flag_str Character. A string containing semi-colon separated quality flags (e.g., \code{"drift; low_battery"}), or \code{NA}.
#'
#' @details
#' \itemize{
#'   \item Splits the text string by semi-colons and cleans up newline breaks or whitespace padding.
#'   \item Drops any individual string elements matching \code{"drift"} or empty spaces.
#'   \item Re-assembles the remaining text elements back into a semi-colon delimited format.
#' }
#'
#' @return A cleaned character string containing only the surviving tokens, or \code{NA_character_} if no other flags remain.
remove_drift_token <- function(flag_str) {
  if (is.na(flag_str) || flag_str == "") return(NA_character_)
  tokens <- str_split(flag_str, ";")[[1]] %>% str_replace_all("\\n", " ") %>% str_trim()
  cleaned <- tokens[tokens != "drift" & tokens != ""]
  if (length(cleaned) == 0) return(NA_character_)
  return(paste(cleaned, collapse = "; "))
}
