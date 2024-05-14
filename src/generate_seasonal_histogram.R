# Generate seasonal histograms for a given site parameter data frame based on the mean column.
# @param df A dataframe with a `mean` column.
# @param df_index The index of the dataframe.
# @return A plot with a histogram of the mean column for each season.
# @examples
# generate_seasonal_histogram(df = all_data_flagged$`archery-Actual Conductivity`, df_index = "archery-Actual Conductivity")
# generate_seasonal_histogram(df = all_data_flagged$`boxelder-Temperature`, df_index = "boxelder-Temperature")

generate_seasonal_histogram <- function(df, df_index) {

  winter_baseflow <- c(12,1,2,3,4)
  snowmelt <- c(5,6,NA,NA,NA)
  monsoon <- c(7,8,9,NA,NA)
  fall_baseflow <- c(10,11,NA,NA,NA)

  # do water years

  seasons <- data.frame(winter_baseflow, snowmelt, monsoon, fall_baseflow)

  site_param <- toupper(sub("-", " ", df_index, fixed = TRUE))

  param <- unique(na.omit(df$parameter))

  hist_list <- list()
  for (i in colnames(seasons)){

    filtered_df <- df %>%
      filter(month %in% seasons[[i]],
             !str_detect(flag, "sensor specification range"))

    n <- as.character(sum(!is.na(filtered_df$mean)))

    title <- paste0(i," (n = ",n ,")")

    histogram <- ggplot() +
      geom_histogram() +
      labs(title = title)

    tryCatch({
      minimum <- floor(min(filtered_df$mean, na.rm = TRUE))
      maximum <- ceiling(max(filtered_df$mean, na.rm = TRUE))

      if (param %in% c("Specific Conductivity", "Actual Conductivity", "Turbidity")) {
        bins <- seq(minimum, maximum, by = 10)
      } else if (param %in% c("ORP", "pH", "Depth")) {
        bins <- seq(minimum, maximum, by = 0.05)
      } else {
        bins <- seq(minimum, maximum)
      }

      x_min <- filtered_df$m_mean05[1]
      x_max <- filtered_df$m_mean99[1]

      histogram <- ggplot(data = filtered_df, aes(x = mean)) +
        geom_histogram(breaks = bins) +
        geom_vline(xintercept = x_min, color = "red", linetype = "dashed") +
        geom_vline(xintercept = x_max, color = "red", linetype = "dashed") +
        facet_wrap(~ year, nrow = 1) +
        labs(title = title)
    },
    error = function(err) {
      cat("No finite values for", site_param, i, "\n")})

    hist_list[[i]] <- histogram

  }

  collated_hist <- ggarrange(plotlist = hist_list, nrow=2, ncol=2) %>%
    annotate_figure(top = site_param)

  return(collated_hist)

}
