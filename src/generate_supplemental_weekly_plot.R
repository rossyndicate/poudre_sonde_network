generate_supplemental_weekly_plot <- function(daily_plot_data_arg, df_list_arg, site_arg, parameter_arg) {

  site_param_df <- df_list_arg[[paste0(site_arg, "-", parameter_arg)]]

  start_date <- min(daily_plot_data_arg$DT_round) - days(3)
  end_date <- max(daily_plot_data_arg$DT_round) + days(3)

  week_plot_data <- site_param_df %>%
    filter(DT_round %within% interval(start_date, end_date))

  #default is lower network
  site_vector <- c("tamasag", # rist
                   "legacy",
                   "lincoln",
                   "timberline",
                   "prospect",
                   "boxelder", # elc
                   "archery",
                   "river bluffs")

  if(network == "virridy"){
    site_vector <-  c("joei",
                      "cbri",
                      "chd",
                      "pfal",
                      "pbd",
                      "sfm",
                      "lbea",
                      "penn",
                      NA,
                      "timberline",
                      "timberline virridy",
                      "springcreek",
                      "prospect",
                      "prospect virridy",
                      "archery",
                      "archery virridy",
                      "boxcreek")
  }

  # determining the index for the site of interest.
  site_index <- which(site_vector == site_arg)

  # TryCatch used here to avoid erroring out on the first and last values of
  # site_vector object (there is no prior/next record after the first/last record).
  # Return df as NULL in case of an error
  prev_site_df <- NULL
  next_site_df <- NULL

  tryCatch({
    previous_site <- paste0(site_vector[site_index-1],"-",unique(daily_plot_data_arg$parameter))
    prev_site_df <- df_list_arg[[previous_site]] %>%
      filter(DT_round %within% interval(start_date, end_date))},
    error = function(err) {
      cat("No previous site.\n")})

  tryCatch({
    next_site <- paste0(site_vector[site_index+1],"-",unique(daily_plot_data_arg$parameter))
    next_site_df <- df_list_arg[[next_site]] %>%
      filter(DT_round %within% interval(start_date, end_date))},
    error = function(err) {
      cat("No next site.\n")})

  # Bind all three dfs
  week_plot_data <- list(week_plot_data, prev_site_df, next_site_df) %>%
    # remove NULL values from the list
    keep(~ !is.null(.)) %>%
    bind_rows()

  # Create a sequence of dates for the vertical lines
  start_date <- floor_date(min(week_plot_data$DT_round), "day")
  end_date <- ceiling_date(max(week_plot_data$DT_round), "day")
  vline_dates <- seq(start_date, end_date, by = "day")

  date_seq <- seq(min(vline_dates), max(vline_dates) - days(1), by = "1 day") + hours(12)

  # use the daily flag data day as flag_day
  flag_day <- unique(daily_plot_data_arg$DT_round)

  week_plot <- ggplot(data = week_plot_data) +
    geom_point(data = filter(week_plot_data, (site == unique(daily_plot_data_arg$site))),
               aes(x=DT_round, y=mean, color=flag)) +
    geom_line(data = filter(week_plot_data, (site != unique(daily_plot_data_arg$site))),
              aes(x=DT_round, y=mean, color=site)) +
    geom_rect(data = daily_plot_data_arg, aes(xmin = min(DT_round), xmax = max(DT_round),
                                              ymin = -Inf, ymax = Inf),
              fill = "grey",
              alpha = 0.01,
              color = NA) +
    geom_vline(xintercept = vline_dates, color = "black") +
    ggtitle(paste0(str_to_title(unique(daily_plot_data_arg$site)), " ", unique(daily_plot_data_arg$parameter), " (", format(flag_day, "%B %d, %Y"), ")")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Day",
         y = "Mean")

  week_plot <- add_threshold_lines(plot = week_plot,
                                   plot_data = week_plot_data,
                                   site_arg = unique(daily_plot_data_arg$site),
                                   parameter_arg = unique(daily_plot_data_arg$parameter))

  week_plot <- week_plot +
    theme_bw() +
    scale_x_datetime(date_breaks = "1 day",
                     date_labels = "%b %d",
                     minor_breaks = date_seq) +
    theme(legend.position = 'right',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + # make the background white
    guides(color = guide_legend(nrow = 10, byrow = TRUE))

  return(week_plot)
}
