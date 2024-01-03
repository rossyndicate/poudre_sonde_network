add_repeat_flag <- function(df){
  df <- df %>%
    add_flag((mean == front1 | mean == back1) & !grepl("repeated value", flag), "repeated value")
}
