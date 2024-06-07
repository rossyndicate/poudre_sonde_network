generate_daily_plot <- function(plot_data_arg, df_list_arg, site_arg, parameter_arg) {

  # site_param_df <- df_list_arg[[paste0(site_arg, "-", parameter_arg)]]

  start_date <- min(plot_data_arg$DT_round)
  end_date <- max(plot_data_arg$DT_round)

  site_vector <- c("tamasag",
                   "legacy",
                   "lincoln",
                   "timberline",
                   "prospect",
                   "boxelder",
                   "archery",
                   "river bluffs")

  # determining the index for the site of interest.
  site_index <- which(site_vector == site_arg)

  # TryCatch used here to avoid erroring out on the first and last values of
  # site_vector object (there is no prior/next record after the first/last record).
  # Return df as NULL in case of an error
  prev_site_df <- NULL
  next_site_df <- NULL

  tryCatch({
    previous_site <- paste0(site_vector[site_index-1],"-",unique(parameter_arg))
    prev_site_df <- df_list_arg[[previous_site]] %>%
      filter(DT_round %within% interval(start_date, end_date))},
    error = function(err) {
      cat("No previous site.\n")})

  tryCatch({
    next_site <- paste0(site_vector[site_index+1],"-",unique(parameter_arg))
    next_site_df <- df_list_arg[[next_site]] %>%
      filter(DT_round %within% interval(start_date, end_date))},
    error = function(err) {
      cat("No next site.\n")})

  # Bind all three dfs
  daily_plot_data <- list(plot_data_arg, prev_site_df, next_site_df) %>%
    # remove NULL values from the list
    keep(~ !is.null(.)) %>%
    bind_rows()

  # use the daily flag data day as flag_day
  flag_day <- min(plot_data_arg$DT_round)

  plot <- ggplot(data = daily_plot_data) +
    geom_point(data = filter(daily_plot_data, (site == site_arg)),
               aes(x=DT_round, y=mean, color=flag)) +
    geom_line(data = filter(daily_plot_data, (site != site_arg)),
              aes(x=DT_round, y=mean, color=site)) +
    ggtitle(paste0(str_to_title(site_arg), " ", str_to_title(parameter_arg), " (", format(flag_day, "%B %d, %Y"), ")")) +
    labs(x = "Time",
         y = "Mean")

  plot <- add_threshold_lines(plot = plot,
                              plot_data = plot_data_arg,
                              site_arg = site_arg,
                              parameter_arg = parameter_arg)

  plot <- plot +
    geom_vline(xintercept = seq(min(plot_data_arg$DT_round), max(plot_data_arg$DT_round), by = "hour"),
               color = "lightgrey", size = 0.5) +
    scale_x_datetime(breaks = seq(min(plot_data_arg$DT_round), max(plot_data_arg$DT_round), by = "hour"),
                     labels = format(seq(min(plot_data_arg$DT_round), max(plot_data_arg$DT_round), by = "hour"), "%H")) +
    theme_bw() +
    theme(legend.position = 'right',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1)) + # make the background white
    guides(color = guide_legend(nrow = 10, byrow = TRUE))

  return(plot)

}
