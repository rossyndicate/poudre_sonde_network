# Visualizing weekly data ----
sites_order <- c("tamasag", # rist
                 "legacy",
                 "lincoln",
                 "timberline",
                 "prospect",
                 "boxelder", # elc
                 "archery",
                 "river bluffs")

generate_weekly_flag_plots <- function(site_arg, parameter_arg, flag_arg) {

  site_index <- which(sites_order == site_arg)

  site_window <- sites_order[c(site_index,
                               site_index-1,
                               site_index+1)]

  site_param <- paste0(site_window[1],"-",parameter_arg)
  site_param_m_1 <- paste0(site_window[2],"-",parameter_arg)
  site_param_p_1 <- paste0(site_window[3],"-",parameter_arg)

  site_flag_dates <- all_data_flagged[[site_param]] %>%
    filter(str_detect(flag, as.character(flag_arg))) %>%
    group_by(day(DT_join)) %>%
    slice(1)

  plot_list <- list()

  for(i in 1:nrow(site_flag_dates)) {

    flag_year <- site_flag_dates$year[i]
    flag_month <- site_flag_dates$month[i]
    flag_day <- site_flag_dates$DT_round[i]
    # Getting the previous and next 3 days in relation to the flag day
    start_day <- flag_day - days(3)
    end_day <- flag_day + days(3)

    site_df <- all_data_flagged[[site_param]] %>%
      filter(year == flag_year,
             month == flag_month,
             DT_round >= start_day & DT_round <= end_day)

    prev_site_df <- all_data_flagged[[site_param_m_1]] %>%
      filter(year == flag_year,
             month == flag_month,
             DT_round >= start_day & DT_round <= end_day)

    next_site_df <- all_data_flagged[[site_param_p_1]] %>%
      filter(year == flag_year,
             month == flag_month,
             DT_round >= start_day & DT_round <= end_day)

    week_plot_data <- bind_rows(site_df, prev_site_df, next_site_df)

    site_day_data <- all_data_flagged[[site_param]] %>%
      filter(year == flag_year,
             month == flag_month,
             day(DT_round) == day(flag_day))

    week_plot <- ggplot(data = week_plot_data, aes(x=DT_round, y=mean, color=site)) +
      geom_rect(data = site_day_data, aes(xmin = min(DT_round), xmax = max(DT_round),
                                          ymin = -Inf, ymax = Inf),
                fill = "grey",
                alpha = 0.01,
                color = NA)+
      geom_line() +
      # geom_line(aes(x = DT_round, y = rollavg, color = "mean"), show.legend = TRUE) +
      # geom_vline(data = (plot_data %>% filter(is.na(mean))), aes(xintercept = DT_round, color = flag)) +
      # geom_smooth(method='loess', formula= y~x, color = "blue", level = .99, na.rm = TRUE) +
      theme_bw() +
      theme(legend.position = 'bottom') +
      ggtitle(paste("Site Visit on", as.character(flag_day)))

    day_plot <- ggplot(data = site_day_data, aes(x=DT_round, y = mean, color = flag)) +
      geom_point() +
      # exceeding sd visualized
      geom_ribbon(aes(ymin = rollavg - ym_sd, ymax = rollavg + ym_sd), alpha = 0.1, color = NA) +
      geom_ribbon(aes(ymin = rollavg - (ym_sd*2), ymax = rollavg + (ym_sd*2)), alpha = 0.1, color = NA) +
      geom_ribbon(aes(ymin = rollavg - (ym_sd*3), ymax = rollavg + (ym_sd*3)), alpha = 0.1, color = NA) +
      geom_line(aes(x = DT_round, y = rollavg, color = "mean"), show.legend = TRUE) +
      # exceeding slope visualized
      geom_vline(data = (day_data %>% filter(is.na(mean))), aes(xintercept = DT_round, color = flag)) +
      # geom_smooth(method='loess', formula= y~x, color = "blue", level = .99, na.rm = TRUE) +
      theme_bw() +
      theme(legend.position = 'bottom') +
      ggtitle(paste("Site Visit on", as.character(flag_day)))

    combined_plot <- ggarrange(week_plot, day_plot, nrow = 2)

    plot_list[[paste(site_arg, parameter_arg, as.character(flag_day))]] <- combined_plot
  }

  return(plot_list)
}

generate_weekly_flag_plots("archery", "Temperature", "slope flag suspect")
