plotter <- function(site_name = "boxelder",
                    start_date = "2023-03-04",
                    end_date = "2023-07-07",
                    title = "wack data title",
                    parameter = "all") {

  # Plotting options: all, Specific Conductivity, Turbidity, Chl-a Fluorescence, ORP, Depth, pH, DO

  sites_order <- c("tamasag", # rist
                   "legacy",
                   "lincoln",
                   "timberline",
                   "prospect",
                   "boxelder", # elc
                   "archery",
                   "river bluffs")

  # determining the index for the site of interest.
  site_index <- which(sites_order == site_name)
  previous_site <- sites_order[site_index-1]
  next_site <- sites_order[site_index+1]

  # Pull in WQ data from Manners Bridge (in the canyon):
  contrail <- list.files("data/context_data/contrail", full.names = TRUE) %>%
    map_dfr(~fread(.)) %>%
    mutate(date = as_date(ymd_hms(Reading)),
           Reading = floor_date(ymd_hms(Reading), "hour")) %>%
    filter(date >= start_date & date <= end_date)

  # Pull in a list of precip monitoring sites in each sonde's watershed:
  rain_list <- fread("data/context_data/site_link.csv") %>%
    filter(site == site_name) %>% pull(`Sensor Name`)

  # Pull in raw precip data
  rain_data <- fread("data/context_data/fc_rain.csv") %>%
    filter(`Sensor Name` %in% rain_list) %>%
    mutate(date = as_date(mdy_hms(Timestamp)),
           DT = floor_date(mdy_hms(Timestamp), unit = "hour")) %>%
    filter(date >= start_date & date <= end_date) %>%
    group_by(DT) %>%
    summarize(total_ws = sum(`Incremental Rainfall (in)`, na.rm = TRUE))

  # Grab the largest amount of rain across the sonde's watershed at an
  # hourly timestep:
  rain_stat <- fread("data/context_data/fc_rain.csv") %>%
    filter(`Sensor Name` %in% rain_list) %>%
    mutate(date = as_date(mdy_hms(Timestamp)),
           DT = floor_date(mdy_hms(Timestamp), unit = "hour")) %>%
    group_by(DT) %>%
    summarize(total_ws = sum(`Incremental Rainfall (in)`, na.rm = TRUE)) %>%
    filter(total_ws == max(total_ws))

  # Pull in flagged PSN WQ data:
  df <- fread('data/flagged/all_data_flagged_plotter.csv') %>%
    filter(!grepl("sonde not employed", final_cleaned_flag)) %>%
    mutate(date = as_date(DT_round)) %>%
    # filter to dates of interest:
    dplyr::filter((date) >= as_date(start_date) & (date) <= as_date(end_date))

  # site of interest data:
  site_df <- df %>%
    filter(!is.na(mean)) %>%
    filter(site == site_name)

  # downstream of site of interest (if there is one):
  downstream_df <- site_df
  try(downstream_df <- df %>%
        filter(!grepl("site visit|sv window", final_cleaned_flag)) %>%
        filter(site == next_site))

  # upstream of site of interest (if there is one):
  upstream_df <- site_df
  try(upstream_df <- df %>%
        filter(!grepl("site visit|sv window", final_cleaned_flag)) %>%
        filter(site == previous_site))

# All the plotz
  depth <- ggplot() +
    geom_line(data = filter(contrail, Unit == "ft"), aes(Reading, Value * 0.3048), color = "lightblue") +
    geom_line(data = filter(downstream_df, parameter == "Depth"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "lightgrey") +
    geom_line(data = filter(upstream_df, parameter == "Depth"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "darkgrey") +
    geom_line(data = filter(site_df, parameter == "Depth"), aes(DT_round, mean), color = "black") +
    geom_point(data = filter(site_df, parameter == "Depth" & grepl("site visit", final_cleaned_flag)), aes(DT_round, mean), color = "#E70870", cex = 1.5) +
    geom_point(data = filter(site_df, parameter == "Depth" & grepl("sonde moved", final_cleaned_flag)), aes(DT_round, mean), color = "#FFCA3A", cex = 0.75) +
    theme_bw() +
    ylim(min = min(filter(site_df, parameter == "Depth")$mean), max = max(filter(site_df, parameter == "Depth")$mean)) +
    ylab("Depth m") + xlab("") + ggtitle(title)

  temp <- ggplot() +
    geom_line(data = filter(contrail, Unit == "C"), aes(Reading, Value), color = "lightblue") +
    geom_line(data = filter(downstream_df, parameter == "Temperature"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "lightgrey") +
    geom_line(data = filter(upstream_df, parameter == "Temperature"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "darkgrey") +
    geom_line(data = filter(site_df, parameter == "Temperature"), aes(DT_round, mean), color = "black") +
    geom_point(data = filter(site_df, parameter == "Temperature" & grepl("site visit|sv window", final_cleaned_flag)), aes(DT_round, mean), color = "#E70870") +
    theme_bw() +
    ylim(min = min(filter(site_df, parameter == "Temperature")$mean), max = max(filter(site_df, parameter == "Temperature")$mean)) +
    ylab("Temperature C") + xlab("") + ggtitle("")

  ph <- ggplot() +
    geom_line(data = filter(contrail, Unit == "pH"), aes(Reading, Value), color = "lightblue") +
    geom_line(data = filter(downstream_df, parameter == "pH"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "lightgrey") +
    geom_line(data = filter(upstream_df, parameter == "pH"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "darkgrey") +
    geom_line(data = filter(site_df, parameter == "pH"), aes(DT_round, mean), color = "black") +
    geom_point(data = filter(site_df, parameter == "pH" & grepl("site visit|sv window", final_cleaned_flag)), aes(DT_round, mean), color = "#E70870") +
    theme_bw() +
    ylim(min = min(filter(site_df, parameter == "pH")$mean), max = max(filter(site_df, parameter == "pH")$mean)) +
    xlab("") + ylab("pH") + ggtitle("")

  orp <- ggplot() +
    geom_line(data = filter(downstream_df, parameter == "ORP"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "lightgrey") +
    geom_line(data = filter(upstream_df, parameter == "ORP"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "darkgrey") +
    geom_line(data = filter(site_df, parameter == "ORP"), aes(DT_round, mean), color = "black") +
    geom_point(data = filter(site_df, parameter == "ORP" & grepl("site visit|sv window", final_cleaned_flag)), aes(DT_round, mean), color = "#E70870") +
    theme_bw() +
    ylim(min = min(filter(site_df, parameter == "ORP")$mean), max = max(filter(site_df, parameter == "ORP")$mean)) +
    ylab("ORP") + xlab("") + ggtitle("")

  spc <- ggplot() +
    geom_line(data = filter(contrail, Unit == "uS/cm"), aes(Reading, Value), color = "lightblue") +
    geom_line(data = filter(downstream_df, parameter == "Specific Conductivity"), aes(DT_round, mean), cex = 0.2, alpha = 1, cex = 0.2, alpha = 1, color = "lightgrey") +
    geom_line(data = filter(upstream_df, parameter == "Specific Conductivity"), aes(DT_round, mean), cex = 0.2, alpha = 1, cex = 0.2, alpha = 1, color = "darkgrey") +
    geom_line(data = filter(site_df, parameter == "Specific Conductivity"), aes(DT_round, mean), color = "black", cex = 0.8) +
    geom_point(data = filter(site_df, parameter == "Specific Conductivity" & grepl("site visit|sv window", final_cleaned_flag)), aes(DT_round, mean), color = "#E70870", cex = 1.5) +
    theme_bw() +
    ylim(min = min(filter(site_df, parameter == "Specific Conductivity")$mean) - 1, max = max(filter(site_df, parameter == "Specific Conductivity")$mean) + 1) +
    ylab("SpC uS/cm") + xlab("") + ggtitle("")

  do <- ggplot() +
    geom_line(data = filter(contrail, Unit == "mg/L"), aes(Reading, Value), color = "lightblue") +
    geom_line(data = filter(downstream_df, parameter == "DO"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "lightgrey") +
    geom_line(data = filter(upstream_df, parameter == "DO"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "darkgrey") +
    geom_line(data = filter(site_df, parameter == "DO"), aes(DT_round, mean), color = "black") +
    geom_point(data = filter(site_df, parameter == "DO" & grepl("site visit|sv window", final_cleaned_flag)), aes(DT_round, mean), color = "#E70870") +
    theme_bw() +
    ylim(min = min(filter(site_df, parameter == "DO")$mean) - 1, max = max(filter(site_df, parameter == "DO")$mean) + 1) +
    ylab("DO mg/L") + xlab("") + ggtitle("")

  # Handle plotting for sondes with turbidity vs. chl-a data:

  turb <- ggplot() +
    geom_line(data = filter(contrail, Unit == "ntu"), aes(Reading, Value), color = "lightblue") +
    geom_line(data = filter(downstream_df, parameter == "Turbidity"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "lightgrey") +
    geom_line(data = filter(upstream_df, parameter == "Turbidity"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "darkgrey") +
    geom_line(data = filter(site_df, parameter == "Turbidity"), aes(DT_round, mean), color = "black") +
    geom_point(data = filter(site_df, parameter == "Turbidity" & grepl("site visit|sv window", final_cleaned_flag)), aes(DT_round, mean), color = "#E70870") +
    theme_bw() +
    ylim(min = min(filter(site_df, parameter == "Turbidity")$mean) + 10, max = max(filter(site_df, parameter == "Turbidity")$mean) + 10) +
    ylab("Turbidity NTU") + xlab("") + ggtitle("")

  if(nrow(filter(site_df, parameter == "Chl-a Fluorescence")) > 0){
    chla <- ggplot() +
      geom_line(data = filter(downstream_df, parameter == "Chl-a Fluorescence"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "lightgrey") +
      geom_line(data = filter(upstream_df, parameter == "Chl-a Fluorescence"), aes(DT_round, mean), cex = 0.2, alpha = 1, color = "darkgrey") +
      geom_line(data = filter(site_df, parameter == "Chl-a Fluorescence"), aes(DT_round, mean), color = "black") +
      geom_point(data = filter(site_df, parameter == "Chl-a Fluorescence" & grepl("site visit|sv window", final_cleaned_flag)), aes(DT_round, mean), color = "#E70870") +
      theme_bw() +
      ylab("Chl-a RFU") + xlab("") + ggtitle("")
  }

  if(nrow(filter(site_df, parameter == "Chl-a Fluorescence")) == 0){ chla <- NULL }

  # rain proxy plot. this just plots the sum of at an hourly timestep of
  # rain at all sites in the watershed. not rooted in anything scientific,
  # but just shows in a crude way whether or not there was rain happening
  # at any given time (and at any given point) during the time of interest.
  rain <- ggplot(data = rain_data) +
    geom_col(aes(x = DT,
                 y = total_ws), color = "#002EA3") +
    ylab("Rain (PROXY)") + xlab("") +
    ylim(min = 0, max = rain_stat$total_ws) +
    theme_bw()

  # handling what gets plotted based on user input:

  if(parameter %in% c("All", "all", "all parameters", "all params")){
    if(is.null(chla)) {

      plot <- ggarrange(depth, temp, ph, orp, spc, do, turb,
                         rain, nrow = 4, ncol = 2)
    }

    if(!is.null(chla)){
      plot <- ggarrange(depth, temp, ph, orp, spc, do, chla,
                         rain, nrow = 4, ncol = 2)
    }
  }

  if(parameter %in% c("pH", "ph")){
    plot <- ggarrange(depth, temp, ph, rain,
                      ncol = 1, nrow = 4)
  }

  if(parameter %in% c("orp", "ORP")){
    plot <- ggarrange(depth, temp, orp, rain,
                       ncol = 1, nrow = 4)
  }

  if(parameter %in% c("do","DO")){
    plot <- ggarrange(depth, temp, do, rain,
                      ncol = 1, nrow = 4)
  }

  if(parameter %in% c("turbidity", "turb", "Turb", "Turbidity")){
    plot <- ggarrange(depth, temp, turb, rain,
                      ncol = 1, nrow = 4)
  }

  if(parameter %in% c("Specific Conductivity", "conductivity", "specific conductivity", "sc", "spc", "SC","SpC")){
    plot <- ggarrange(depth, temp, spc, rain,
                      ncol = 1, nrow = 4)
  }

  if(parameter %in% c("chla", "Chla", "chl-a", "chlorophyll a", "Chl-a Fluorescence")){
    plot <- ggarrange(depth, temp, chla, rain,
                      ncol = 1, nrow = 4)
  }

  return(plot)
}
