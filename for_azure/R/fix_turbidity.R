#' @title Fix turbidity values that are too high.
#' 
#' @description
#' A function that checks if the turbidity values are too high and replaces them
#' with a value of 1000.
#' 
#' @param df A turbidity data frame with a `mean` column.
#' 
#' @return A data frame with a `mean` column that has been updated with the
#' relevant turbidity values.

fix_turbidity <- function(df){

# Filter records for relevant site-param information
df_site <- unique(df$site)
df_parameter <- unique(df$parameter)

# Function to add a column if it doesn't already exist
add_column_if_not_exists <- function(df, column_name, default_value = NA) {
  if (!column_name %in% colnames(df)) {
    df <- df %>% dplyr::mutate(!!sym(column_name) := default_value)
  }
  return(df)
}

df <- df %>%
  add_column_if_not_exists(column_name = "raw") %>%
  mutate(raw = ifelse(is.na(raw), mean, raw))

if(df_parameter == "Turbidity"){

  df <- df %>%
    mutate(mean = ifelse(mean >= 1000, 1000, mean))

  }

return(df)

}
