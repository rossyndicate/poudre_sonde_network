add_na_flag <- function(df){
  df <- df %>%
    add_flag(is.na(mean) & !grepl("missing data", flag), "missing data")
}
