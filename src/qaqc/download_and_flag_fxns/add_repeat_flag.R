# Add a flag for when data repeats

add_repeat_flag <- function(df){

  df %>%
    add_flag((mean == front1 | mean == back1), "repeated value")

}
