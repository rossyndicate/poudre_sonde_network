#' @title Find DO interference
#' 
#' @description
#' A function that checks if the DO sensor is experiencing interference
#' by comparing the difference between the front and back sensors to the mean.
#' If the difference is greater than 0.5, the function will add a "DO interference".
#' 
#' @param df A data frame with a `flag` column.
#' 
#' @return A data frame with a `flag` column that has been updated with the
#' relevant DO interference flags.
#' 
#' @examples
#' find_do_noise(df = all_data_flagged$`archery-DO`)

find_do_noise <- function(df){

if("DO" %in% df$parameter){

  df <- df %>%
    add_flag(back1 - mean >= 0.5 & front1 - mean >= 0.5, "DO interference")

  return(df)

} else {

  return(df)

}

  }
