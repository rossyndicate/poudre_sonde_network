#' @title Apply all flags to a data frame
#' 
#' @description
#' A function that applies all flags to a data frame. This function is used to
#' apply all flags to a data frame in one step.
#' 
#' @param data A data frame with a `flag` column.
#' 
#' @param require An upstream dependency (i.e., alternative to `{necessary object}`).
#' 
#' @return A data frame with a `flag` column that has been updated with all flags.
#' 
#' @examples
#' flag_all_data(data = all_data_flagged$`archery-Actual Conductivity`)
#' flag_all_data(data = all_data_flagged$`boxelder-Temperature`)
#' 
#' @seealso [add_field_flag()]
#' @seealso [add_spec_flag()]
#' @seealso [add_seasonal_flag()]
#' @seealso [add_na_flag()]
#' @seealso [add_repeat_flag()]
#' @seealso [add_suspect_flag()]
#' @seealso [add_malfunction_flag()]

flag_all_data <- function(data, require = NULL) {
  flagged_data <- data %>%
    add_field_flag() %>%
    add_spec_flag() %>%
    add_seasonal_flag() %>%
    add_na_flag() %>%
    add_repeat_flag() %>%
    add_suspect_flag() %>%
    add_malfunction_flag()
    # mutate(mean_public = ifelse(is.na(flag), mean, NA))
  return(flagged_data)
}
