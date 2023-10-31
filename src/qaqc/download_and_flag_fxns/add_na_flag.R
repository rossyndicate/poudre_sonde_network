add_na_flag <- function(df){
  df <- df %>%
    add_flag(is.na(mean), "missing data")
}
