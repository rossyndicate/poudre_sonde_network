generate_supplemental_weekly_plot <- function(daily_plot_data_arg, df_list_arg, site_arg, parameter_arg) {

  site_param_df <- df_list_arg[[paste0(site_arg, "-", parameter_arg)]]

  start_date <- min(daily_plot_data_arg$DT_round) - days(3) # TODO: replace all daily_plot_data_arg instances with something more efficient
  end_date <- max(daily_plot_data_arg$DT_round) + days(3)

  week_plot_data <- site_param_df %>%
    filter(DT_round %within% interval(start_date, end_date))

  # establish which sites are relevant to the site of interest
  # the order of the sites are hardcoded in relevant_sonde_selector function
  plot_filter <- relevant_sonde_selector(site_arg = site_arg)

  # get the relevant sonde data source
  relevant_sonde_source <- map(plot_filter, ~ {
    sonde_name <- paste0(.x, "-", parameter_arg)
    # Determine which directory to pull data from
    tryCatch({
      retrieve_relevant_data_name(sonde_name, interval_arg = interval(start_date, end_date))
    }, error = function(err) {
      return("all_data")
    })
  })

  # Get the relevant data
  relevant_sondes <- map2(plot_filter,
                          relevant_sonde_source,
    function(name, source) {
    sonde_name <- paste0(name, "-", parameter_arg)
    # try to pull in the data
    tryCatch({
      get(source)[[sonde_name]] %>%
        filter(DT_round %within% interval(start_date, end_date))
    }, error = function(err) {
      return(NULL)
    })
  })

  # combine the lists
  sonde_info <- map2(relevant_sondes, relevant_sonde_source, list)

  # Remove any NULL results from the list
  sonde_info <- keep(sonde_info, ~!is.null(.x[[1]]))

  # append site_df to relevant sonde list, clean list, and bind dfs
  # to find plot info
  relevant_dfs <- map(sonde_info, ~.x[[1]])

  week_plot_data <- append(relevant_dfs, list(week_plot_data)) %>%
    keep(~ !is.null(.)) %>%
    keep(~ nrow(.)>0) %>%
    bind_rows() %>%
    arrange(day)

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
    map(sonde_info, function(sonde_data) {

      data <- sonde_data[[1]]
      data_source <- sonde_data[[2]]

      y_column <- if (data_source == "all_data") "mean" else "mean_verified"

      geom_line(data = data, aes(x = DT_round, y = .data[[y_column]], color = site))
    }) +
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
