generate_daily_plot <- function(plot_data_arg, df_list_arg, site_arg, parameter_arg) {

  # site_param_df <- df_list_arg[[paste0(site_arg, "-", parameter_arg)]]

  start_date <- min(plot_data_arg$DT_round)
  end_date <- max(plot_data_arg$DT_round)

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

  # append plot_data_arg to relevant sonde list, clean list, and bind dfs
  daily_plot_data <- append(relevant_dfs, list(plot_data_arg)) %>%
    keep(~ !is.null(.)) %>%
    keep(~ nrow(.)>0) %>%
    bind_rows()

  # use the daily flag data day as flag_day
  flag_day <- min(plot_data_arg$DT_round)

  plot <- ggplot(data = daily_plot_data) +
    geom_point(data = filter(daily_plot_data, (site == site_arg)),
               aes(x=DT_round, y=mean, color=flag)) +
    # geom_line(data = filter(daily_plot_data, (site != site_arg)),
    #           aes(x=DT_round, y=mean, color=site)) +
    map(sonde_info, function(sonde_data) {
      data <- sonde_data[[1]]
      data_source <- sonde_data[[2]]

      y_column <- if (data_source == "all_data") "mean" else "mean_verified"

      geom_line(data = data, aes(x = DT_round, y = .data[[y_column]], color = site))
    }) +
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
