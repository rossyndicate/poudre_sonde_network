


update_sensor_tracker <-function(){
  library(tidyverse)
  library(purrr)
  library(rvest)
  `%nin%` = Negate(`%in%`)
  #look at calibration report folder in `data`
  file_paths <- list.files("data/calibration_reports", full.names = TRUE)
  #prep metatdata of who owns which SNs
  ownership <- read_csv('data/metadata/sensor_ownership.csv', show_col_types = F)%>%
    mutate(SN = as.character(SN))
  #all sites
  sites_list <- c("joei", "cbri", "chd", "pfal", "sfm", "pbd","lbea", "penn",  "tamasag", "legacy",
                  "lincoln", "timberline", "prospect","boxelder", "archery", "riverbluffs", "boxcreek", "springcreek", "mtncampus")
  #currently running sites
  cur_sites_list <- c("joei", "cbri", "chd", "pfal", "sfm", "pbd","lbea", "penn",  "tamasag", "legacy",
                  "lincoln", "timberline", "prospect","boxelder", "archery", "archery virridy", "riverbluffs", "boxcreek", "springcreek", "mtncampus")

  # Function to extract site and owner
  parse_file_info <- function(file_path) {

    # Extract the site: string after the last '/' and before '_'


    site <- str_extract(file_path, paste(sites_list, collapse = "|"))

    # Extract the owner: string before '.html', unless it's 'mst'
    owner <- str_extract(file_path, "(?<=_)[^_/]+(?=\\.html)")%>%
      #remove spaces
      str_replace_all(" ", "")
    #break the file path into parts between _
    parts <- str_split(file_path, "_")%>%unlist()

    date_time <- paste0(parts[3],"_", parts[4])%>%
      ymd_hm(., tz = "MST")

    # Modify the site if owner is 'CU'
    if (owner == "CU") {
      site <- paste0(site, " virridy")
    }

    if (owner == "mst") {
      owner <- NA
    }

    tibble(path = file_path, site = site, owner = owner, datetime = date_time)
  }

  # Apply function to the file paths using map_df to create a tidy dataframe
  current_files <- map_df(file_paths, parse_file_info)%>%
    #find the most recent one for each site after excluding NA site
    filter(!is.na(site) & site %in% cur_sites_list)%>%
    group_by(site)%>%
    filter(datetime == max(datetime))%>%
    ungroup()

  parse_cal_report <- function(file, site){

    cal <- read_html(file) %>%
      html_elements("div") %>%
      html_text() %>%
      as_tibble() %>% pull()%>% str_replace_all(., " ", "") %>%str_split(., "\n") %>%unlist()%>% str_replace_all(., "\r", "")

    troll <- read_html(file) %>%
      html_element("body")%>%
      html_element("table")%>%
      html_text()%>%
      str_replace_all(., " ", "") %>%str_split(., "\n") %>%unlist()%>% str_replace_all(., "\r", "")

    # make a new tibble with a column for Sensor type, serial number, and calibration date. Sensor type starts on the 4th position and repeats every 9
    # Function to extract sensor type, serial number, and calibration date
    sensor_info <- tibble(site = site,
        sensor_type = cal[4 + 9 * (0:(length(cal)/9 - 1))]%>%tolower(),
        serial_number = cal[6 + 9 * (0:(length(cal)/9 - 1))],
        calibration_date = cal[8 + 9 * (0:(length(cal)/9 - 1))])%>%
      add_row(sensor_type = troll[2], serial_number = troll[4], calibration_date = NA, site = site)%>%
      filter(sensor_type %nin%c( "barometricpressure", "pressure"))

    return(sensor_info)

  }

  cur_sensor_info <- map2_df(current_files$path, current_files$site, parse_cal_report)%>%
    left_join(ownership%>%select(-Equipment), by = c("serial_number" = "SN"))


  cur_sensor_info


}

sensors_only <- update_sensor_tracker()%>%
  filter(sensor_type %nin% c("AquaTROLL500", "AquaTROLL600","AquaTROLL700", "AquaTROLL800" ))


%>% select(-Notes, -archive)%>%
  group_by(site)%>%
  summarize(
    sonde_model = paste(unique(sensor_type[grepl("AquaTROLL", sensor_type)]), collapse = ", "),
    sonde_SN = paste(unique(serial_number[grepl("AquaTROLL", sensor_type)]), collapse = ", "),
    sensors = paste(sort(unique(sensor_type[!grepl("AquaTROLL", sensor_type)])), collapse = ", "),
    .groups = 'drop'
  )
#make a column called parameters which is a list of the sensor types excluding AquaTROLL
