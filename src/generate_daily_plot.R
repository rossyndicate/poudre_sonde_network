generate_daily_plot <- function(plot_data_arg, df_list_arg, site_arg, parameter_arg) {

  # site_param_df <- df_list_arg[[paste0(site_arg, "-", parameter_arg)]]

  start_date <- min(plot_data_arg$DT_round)
  end_date <- max(plot_data_arg$DT_round)

  #default is lower network
  site_vector <- c("tamasag", # rist
                   "legacy",
                   "lincoln",
                   "timberline",
                   "prospect",
                   "boxelder", # elc
                   "archery",
                   "river bluffs")

  if(network == "virridy"){ # this will be the new default
    # establish order for all the non-tributary sites
    sites_order <-  c("joei","cbri","chd","pfal","sfm","pbd","tamasag",
                      "legacy","lincoln","timberline","prospect","boxelder",
                      "archery","riverbluffs")
    # establish the order for the tributary sites
    trib_sites_order <- c("boxcreek", "archery", NA, "springcreek", "prospect",
                          NA, "penn", "sfm", "lbea")
  }

  # determining the index for the site of interest.
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

  # Get the relevant sonde data
  relevant_sondes <- map(plot_filter,
                         ~ {
                           sonde_name <- paste0(.x,"-",parameter_arg)
                           tryCatch({
                             sonde_df <- df_list_arg[[sonde_name]]  %>%
                               filter(DT_round %within% interval(start_date, end_date))},
                             error = function(err) {
                               cat("Sonde ", sonde_name," not found.\n")})
                         })

  # append plot_data_arg to relevant sonde list, clean list, and bind dfs
  daily_plot_data <- append(relevant_sondes, list(plot_data_arg)) %>%
    keep(~ !is.null(.)) %>%
    keep(~ nrow(.)>0) %>%
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
