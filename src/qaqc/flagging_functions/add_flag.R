# flag addition function
add_flag <- function(df, condition_arg, description_arg) {
  df <- df %>% mutate(flag = case_when(
    {{condition_arg}} ~ if_else(is.na(flag), paste(description_arg), paste(flag, description_arg, sep = ";\n")),
    TRUE ~ flag))
  return(df)
}
