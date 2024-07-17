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

      sites_order <-  c("joei","cbri","chd","pfal","sfm","pbd","tamasag",
                        "legacy","lincoln","timberline","prospect","boxelder",
                        "archery","riverbluffs")

      trib_sites_order <- c("boxcreek", "archery", NA, "springcreek", "prospect",
                            NA, "penn", "sfm", "lbea")
    }

    # determining the sites relevant to the site of interest.
    if (site_arg %in% sites_order) {

      plot_filter <- tibble(site = c("joei","cbri","chd","pfal","sfm","pbd",
                                     "tamasag","legacy", "lincoln","timberline",
                                     "timberline virridy","prospect",
                                     "prospect virridy","boxelder","archery",
                                     "archery virridy","riverbluffs"))

      site_index <- which(sites_order == site_arg)
      site_list <- as.vector(na.omit(sites_order[max(1, site_index - 1):min(length(sites_order), site_index + 1)]))

      plot_filter <- plot_filter %>%
        filter(grepl(paste(site_list, collapse = "|"), site, ignore.case = TRUE),
               site != site_arg) %>%
        pull(site)

    } else {

      plot_filter <- tibble(site = c("boxcreek", "archery", "archery virridy",
                                     "springcreek", "prospect", "prospect virridy",
                                     "penn", "sfm", "lbea"))

      site_index <- which(trib_sites_order == site_arg)
      site_list <- as.vector(na.omit(trib_sites_order[max(1, site_index - 1):min(length(trib_sites_order), site_index + 1)]))

      plot_filter <- plot_filter %>%
        filter(grepl(paste(site_list, collapse = "|"), site, ignore.case = TRUE),
               site != site_arg) %>%
        pull(site)

    }

    if (nrow(site_flag_dates >0)) {

      if (is.null(flag_arg)) {
        # This for loop generates an overlayed plot of weekly data for the site of
        # interest sandwiched by the site above and below it for each day that was
        # tagged by a flag of interest
        plot_list <- list()

        grouped_data <- site_flag_dates %>%
          group_by(y_w) %>% #group_by(week, year) %>% # group_by(week, month, year) %>%
          group_split()

        for(i in 1:length(grouped_data)) {

          group_data <- grouped_data[[i]]

          flag_week <- unique(group_data$week)

          # filtering dfs of interest for the week of interest
          site_df <- site_flag_dates %>%
            filter(y_w == group_data$y_w)

          # Get the relevant sondes

          relevant_sondes <- map(plot_filter,
                                 ~ {
                                   sonde_name <- paste0(.x,"-",parameter_arg)
                                   tryCatch({
                                     sonde_df <- all_df_list[[sonde_name]] %>%
                                       filter(y_w == group_data$y_w)}, # *** GO BACK TO THIS
                                       # filter(y_w == "2023 - 35")},
                                     error = function(err) {
                                       cat("Sonde ", sonde_name," not found.\n")})
                                 })

          # prev_site_df <- NULL
          # next_site_df <- NULL
          #
          # tryCatch({
          #   previous_site <- paste0(sites_order[site_index-1],"-",parameter_arg)
          #   prev_site_df <- all_df_list[[previous_site]] %>%
          #     filter(y_w == group_data$y_w)},
          #   error = function(err) {
          #     cat("No previous site.\n")})
          #
          # tryCatch({
          #   next_site <- paste0(sites_order[site_index+1],"-",parameter_arg)
          #   next_site_df <- all_df_list[[next_site]] %>%
          #     filter(y_w == group_data$y_w)},
          #   error = function(err) {
          #     cat("No next site.\n")})

          # Bind all dfs
          week_plot_data <- append(relevant_sondes, list(site_df)) %>%
            # remove NULL values from the list
            keep(~ !is.null(.)) %>%
            # remove empty dfs from the list
            keep(~ nrow(.)>0) %>%
            bind_rows() %>%
            arrange(day)
          # arrange(weekday)

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
            guides(color = guide_legend(nrow = 4, byrow = TRUE))

          plot_list[[paste(site_param, as.character(flag_day))]] <- week_plot
          sorted_plot_names <- names(plot_list)[order(names(plot_list))]

          plot_list <- plot_list[sorted_plot_names]

        }
      }
      # ---- if flag arg is not null ----
      if (!is.null(flag_arg)) {
        print("under construction...")
        # # This for loop generates an overlayed plot of weekly data for the site of
        # # interest sandwiched by the site above and below it for each day that was
        # # tagged by a flag of interest
        # plot_list <- list()
        #
        # site_flag_weeks <- site_flag_dates %>%
        #   filter(str_detect(flag, flag_arg)) %>%
        #   group_by(week, year) %>%
        #   slice(1)
        #
        # grouped_data <- site_flag_dates %>%
        #   filter(y_w %in% site_flag_weeks$y_w) %>%
        #   group_by(week, year) %>% # group_by(week, month, year) %>%
        #   group_split()
        #
        #
        # for(i in 1:length(grouped_data)) {
        #
        #   group_data <- grouped_data[[i]]
        #
        #   # flag_title <- site_flag_dates$flag[i] # no flag title ***
        #
        #   # filtering dfs of interest for the weeks where a flag was detected
        #   site_df <- site_flag_dates %>%
        #     filter(y_w == group_data$y_w)
        #
        #   # TryCatch used here to avoid erroring out on the first and last values of
        #   # sites_order object (there is no prior/next record after the first/last record).
        #   # Return df as NULL in case of an error
        #   prev_site_df <- NULL
        #   next_site_df <- NULL
        #
        #   tryCatch({
        #     previous_site <- paste0(sites_order[site_index-1],"-",parameter_arg)
        #     prev_site_df <- all_df_list[[previous_site]] %>%
        #       filter(y_w == group_data$y_w)},
        #     error = function(err) {
        #       cat("No previous site.\n")})
        #
        #   tryCatch({
        #     next_site <- paste0(sites_order[site_index+1],"-",parameter_arg)
        #     next_site_df <- all_df_list[[next_site]] %>%
        #       filter(y_w == group_data$y_w)},
        #     error = function(err) {
        #       cat("No next site.\n")})
        #
        #   # Bind all three dfs
        #   week_plot_data <- list(site_df, prev_site_df, next_site_df) %>%
        #     # remove NULL values from the list
        #     keep(~ !is.null(.)) %>%
        #     bind_rows()
        #
        #   # Create a sequence of dates for the vertical lines
        #   start_date <- floor_date(min(week_plot_data$DT_round), "day")
        #   end_date <- ceiling_date(max(week_plot_data$DT_round), "day")
        #   vline_dates <- seq(start_date, end_date, by = "day")
        #
        #   date_seq <- seq(min(vline_dates), max(vline_dates) - days(1), by = "1 day") + hours(12) ## here ----
        #
        #   # Use the first day of the group as flag_day
        #   flag_day <- min(group_data$DT_round)
        #
        #   week_plot <- ggplot(data = week_plot_data) +
        #     geom_point(data = filter(week_plot_data, (site == site_arg)),
        #                aes(x=DT_round, y=mean, color=flag)) +
        #     geom_line(data = filter(week_plot_data, (site != site_arg)),
        #               aes(x=DT_round, y=mean, color=site)) +
        #     geom_vline(xintercept = vline_dates, color = "black") +
        #     ggtitle(paste0(str_to_title(site_arg), " ", parameter_arg, " (", format(flag_day, "%B %d, %Y"), ")")) +
        #     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        #     labs(x = "Day",
        #          y = "Mean")
        #
        #   week_plot <- add_threshold_lines(plot = week_plot,
        #                                    plot_data = week_plot_data,
        #                                    site_arg = site_arg,
        #                                    parameter_arg = parameter_arg)
        #
        #   week_plot <- week_plot +
        #     theme_bw() +
        #     scale_x_datetime(date_breaks = "1 day",
        #                      date_labels = "%b %d",
        #                      minor_breaks = date_seq) +
        #     theme(legend.position = 'bottom',
        #           panel.grid.major = element_blank(),
        #           panel.grid.minor = element_blank()) +
        #     annotate("text", x = date_seq, y = min(week_plot_data$mean, na.rm = TRUE) - 1, label = 1:length(date_seq), hjust = 0) +
        #     guides(color = guide_legend(nrow = 4, byrow = TRUE))
        #
        #   plot_list[[paste(site_param, as.character(flag_day))]] <- week_plot
        #   sorted_plot_names <- names(plot_list)[order(names(plot_list))]
        #
        #   plot_list <- plot_list[sorted_plot_names]
        # }
      }
      return(plot_list)
    } else {
      return(paste(flag_arg, "not detected.\n"))
    }
  } else {
    return(paste(site_arg, parameter_arg, "combination not available.\n"))
  }

}
