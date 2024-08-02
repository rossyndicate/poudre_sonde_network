find_do_noise <- function(df){

if("DO" %in% df$parameter){

  df <- df %>%
    add_flag(back1 - mean >= 0.5 & front1 - mean >= 0.5, "DO interference")

  return(df)

} else {

  return(df)

}

  }
