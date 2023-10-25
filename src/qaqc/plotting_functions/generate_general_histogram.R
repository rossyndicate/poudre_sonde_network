# Generate histograms of a site parameter data frame based on the mean column.
# @param df A dataframe with a `mean` column.
# @param df_index The index of the dataframe.
# @return A histogram of the mean column.
# @examples
# generate_general_histogram(df = all_data_flagged$`archery-Actual Conductivity`, df_index = "archery-Actual Conductivity")
# generate_general_histogram(df = all_data_flagged$`boxelder-Temperature`, df_index = "boxelder-Temperature")
 
generate_general_histogram <- function(df, df_index) {

  site_param <- toupper(sub("-", " ", df_index, fixed = TRUE))
  n <- as.character(sum(!is.na(df$mean)))
  title <- paste0("Histogram of ", site_param, " (n = ", n, ")")

  # there should be checks for data here
  minimum <- floor(min(df$mean, na.rm = TRUE))
  maximum <- ceiling(max(df$mean, na.rm = TRUE))

  histogram <- ggplot(data = df, aes(x = mean)) +
    geom_histogram(
      breaks = seq(minimum, maximum, by = 1)
    ) +
    labs(title = title)

  return(histogram)
}
