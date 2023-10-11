# Flag alter function
# This function can be used to alter flags that have been established
alter_flag <- function(df, condition_arg, old_description_arg, new_description_arg) {
  df <- df %>% mutate(flag = case_when(
    {{condition_arg}} & str_detect(flag, old_description_arg) ~ str_replace(flag, old_description_arg, new_description_arg),
    TRUE ~ flag))
  return(df)
}
