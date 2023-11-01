# Add a flag to the `flag` column of a dataframe based on a condition and a description of the flag.
# @param df A dataframe with a `flag` column.
# @param condition_arg A logical statement that is evaluated in the context of the dataframe.
# @param description_arg A string that is added to the flag column if the condition is true.
# @return A dataframe with a `flag` column that has been updated with the flag description provided.
# @examples
# add_flag(df = all_data_flagged$`archery-Actual Conductivity`, condition_arg = mean >= 100, description_arg = "exceeds 100")
# add_flag(df = all_data_flagged$`archery-Actual Conductivity`, condition_arg = mean <= 10, description_arg = "below 10")

add_flag <- function(df, condition_arg, description_arg) {
  df <- df %>% mutate(flag = case_when(
    {{condition_arg}} ~ if_else(is.na(flag), paste(description_arg), paste(flag, description_arg, sep = ";\n")),
    TRUE ~ flag))
  return(df)
}
