generate_initial_weekly_plots <- function(all_df_list, pending_df_list, site_arg, parameter_arg, flag_arg = NULL) {

  site_param <- paste0(site_arg, "-", parameter_arg)

  site_flag_dates <- pending_df_list[[site_param]] %>%
    group_by(y_w) %>%
    filter(any(verification_status == "SKIP") | any(!is_verified)) %>%
    ungroup()

  if (!is.null(site_flag_dates)){
    # vector of sites in the order that they are in spatially ----
    # some sites have some funkiness going on (not all of the sites are present in the final plot)
    #default is lower network
    sites_order <- c("tamasag", # rist
                     "legacy",
                     "lincoln",
                     "timberline",
                     "prospect",
                     "boxelder", # elc
                     "archery",
                     "river bluffs")

    if(network == "virridy"){
      sites_order <-  c("joei",
                        "cbri",
                        "chd",
                        "pfal",
                        "pbd",
                        "sfm",
                        "lbea",
                        "penn",
                        NA,
                        "lincoln",
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
    site_index <- which(sites_order == site_arg)

    # site_flag_weeks <- site_flag_dates %>%
    #   filter(if (!is.null(flag_arg)) str_detect(flag, flag_arg) else TRUE) %>%
    #   group_by(week, year) %>%
    #   slice(1)

    if (nrow(site_flag_dates >0)) {

      if (is.null(flag_arg)) {
        # This for loop generates an overlayed plot of weekly data for the site of
        # interest sandwiched by the site above and below it for each day that was
        # tagged by a flag of interest
        plot_list <- list()

        grouped_data <- site_flag_dates %>%
          group_by(week, year) %>% # group_by(week, month, year) %>%
          group_split()

        for(i in 1:length(grouped_data)) {

          group_data <- grouped_data[[i]]

          # flag_title <- site_flag_dates$flag[i] # no flag title ***
          flag_year <- unique(group_data$year)
          # flag_month <- unique(group_data$month)
          flag_week <- unique(group_data$week)

          # filtering dfs of interest for the weeks where a flag was detected
          site_df <- site_flag_dates %>%
            filter(y_w == group_data$y_w)

          # TryCatch used here to avoid erroring out on the first and last values of
          # sites_order object (there is no prior/next record after the first/last record).
          # Return df as NULL in case of an error
          prev_site_df <- NULL
          next_site_df <- NULL

          tryCatch({
            previous_site <- paste0(sites_order[site_index-1],"-",parameter_arg)
            prev_site_df <- all_df_list[[previous_site]] %>%
              filter(y_w == group_data$y_w)},
            error = function(err) {
              cat("No previous site.\n")})

          tryCatch({
            next_site <- paste0(sites_order[site_index+1],"-",parameter_arg)
            next_site_df <- all_df_list[[next_site]] %>%
              filter(y_w == group_data$y_w)},
            error = function(err) {
              cat("No next site.\n")})

          # Bind all three dfs
          week_plot_data <- list(site_df, prev_site_df, next_site_df) %>%
            # remove NULL values from the list
            keep(~ !is.null(.)) %>%
            # remove empty dfs from the list
            keep(~ nrow(.)>0) %>%
            bind_rows() %>%
            arrange(weekday)

          # Create a sequence of dates for the vertical lines
          start_date <- floor_date(min(week_plot_data$DT_round), "day")
          end_date <- ceiling_date(max(week_plot_data$DT_round), "day")
          vline_dates <- seq(start_date, end_date, by = "day")

          date_seq <- seq(min(vline_dates), max(vline_dates) - days(1), by = "1 day") + hours(12)

          # Use the first day of the group as flag_day
          flag_day <- min(group_data$DT_round)

          week_plot <- ggplot(data = week_plot_data) +
            geom_point(data = filter(week_plot_data, (site == site_arg)),
                       aes(x=DT_round, y=mean, color=flag)) +
            geom_line(data = filter(week_plot_data, (site != site_arg)),
                      aes(x=DT_round, y=mean, color=site)) +
            geom_vline(xintercept = vline_dates, color = "black") +

            ggtitle(paste0(str_to_title(site_arg), " ", parameter_arg, " (", format(flag_day, "%B %d, %Y"), ")")) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "Day",
                 y = "Mean")

         
          week_plot <- add_threshold_lines(plot = week_plot,
                                           plot_data = week_plot_data,
                                           site_arg = site_arg,
                                           parameter_arg = parameter_arg)
 

          week_plot <- week_plot +
            theme_bw() +
            scale_x_datetime(date_breaks = "1 day",
                             date_labels = "%b %d",
                             minor_breaks = date_seq,
                             sec.axis = sec_axis(~., breaks = date_seq, labels = unique(week_plot_data$weekday))) +
            theme(legend.position = 'bottom',
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            # annotate("text", x = date_seq, y = min(week_plot_data$mean, na.rm = TRUE) - 1, label = unique(week_plot_data$weekday), hjust = 0) +
            guides(color = guide_legend(nrow = 4, byrow = TRUE))

          plot_list[[paste(site_param, as.character(flag_day))]] <- week_plot
          sorted_plot_names <- names(plot_list)[order(names(plot_list))]

          plot_list <- plot_list[sorted_plot_names]
        }
      }
      # ---- not null ----
      if (!is.null(flag_arg)) {
        # This for loop generates an overlayed plot of weekly data for the site of
        # interest sandwiched by the site above and below it for each day that was
        # tagged by a flag of interest
        plot_list <- list()

        site_flag_weeks <- site_flag_dates %>%
          filter(str_detect(flag, flag_arg)) %>%
          group_by(week, year) %>%
          slice(1)

        grouped_data <- site_flag_dates %>%
          filter(y_w %in% site_flag_weeks$y_w) %>%
          group_by(week, year) %>% # group_by(week, month, year) %>%
          group_split()


        for(i in 1:length(grouped_data)) {

          group_data <- grouped_data[[i]]

          # flag_title <- site_flag_dates$flag[i] # no flag title ***

          # filtering dfs of interest for the weeks where a flag was detected
          site_df <- site_flag_dates %>%
            filter(y_w == group_data$y_w)

          # TryCatch used here to avoid erroring out on the first and last values of
          # sites_order object (there is no prior/next record after the first/last record).
          # Return df as NULL in case of an error
          prev_site_df <- NULL
          next_site_df <- NULL

          tryCatch({
            previous_site <- paste0(sites_order[site_index-1],"-",parameter_arg)
            prev_site_df <- all_df_list[[previous_site]] %>%
              filter(y_w == group_data$y_w)},
            error = function(err) {
              cat("No previous site.\n")})

          tryCatch({
            next_site <- paste0(sites_order[site_index+1],"-",parameter_arg)
            next_site_df <- all_df_list[[next_site]] %>%
              filter(y_w == group_data$y_w)},
            error = function(err) {
              cat("No next site.\n")})

          # Bind all three dfs
          week_plot_data <- list(site_df, prev_site_df, next_site_df) %>%
            # remove NULL values from the list
            keep(~ !is.null(.)) %>%
            bind_rows()

          # Create a sequence of dates for the vertical lines
          start_date <- floor_date(min(week_plot_data$DT_round), "day")
          end_date <- ceiling_date(max(week_plot_data$DT_round), "day")
          vline_dates <- seq(start_date, end_date, by = "day")

          date_seq <- seq(min(vline_dates), max(vline_dates) - days(1), by = "1 day") + hours(12) ## here ----

          # Use the first day of the group as flag_day
          flag_day <- min(group_data$DT_round)

          week_plot <- ggplot(data = week_plot_data) +
            geom_point(data = filter(week_plot_data, (site == site_arg)),
                       aes(x=DT_round, y=mean, color=flag)) +
            geom_line(data = filter(week_plot_data, (site != site_arg)),
                      aes(x=DT_round, y=mean, color=site)) +
            geom_vline(xintercept = vline_dates, color = "black") +
            ggtitle(paste0(str_to_title(site_arg), " ", parameter_arg, " (", format(flag_day, "%B %d, %Y"), ")")) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(x = "Day",
                 y = "Mean")

          week_plot <- add_threshold_lines(plot = week_plot,
                                           plot_data = week_plot_data,
                                           site_arg = site_arg,
                                           parameter_arg = parameter_arg)

          week_plot <- week_plot +
            theme_bw() +
            scale_x_datetime(date_breaks = "1 day",
                             date_labels = "%b %d",
                             minor_breaks = date_seq) +
            theme(legend.position = 'bottom',
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()) +
            annotate("text", x = date_seq, y = min(week_plot_data$mean, na.rm = TRUE) - 1, label = 1:length(date_seq), hjust = 0) +
            guides(color = guide_legend(nrow = 4, byrow = TRUE))

          plot_list[[paste(site_param, as.character(flag_day))]] <- week_plot
          sorted_plot_names <- names(plot_list)[order(names(plot_list))]

          plot_list <- plot_list[sorted_plot_names]
        }
      }
      return(plot_list)
    } else {
      return(paste(flag_arg, "not detected.\n"))
    }
  } else {
    return(paste(site_arg, parameter_arg, "combination not available.\n"))
  }

}
