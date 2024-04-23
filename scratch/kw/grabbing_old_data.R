
files <- list.files('data/sensor_data/pre2022', full.names = TRUE, recursive = TRUE)


old_data_loader <- function(file){

  file <- list.files('data/sensor_data/pre2022', full.names = TRUE, recursive = TRUE)[8]

  site_name <- file %>% str_extract(., "(?<=pre2022/).*?(?=/)")

  # Read the HTML file
  html_content <- read_html(file)

  # Extract the table
  table <- html_content %>% html_nodes("table") %>%
    html_table() %>%
    .[[1]]

  if(any(grepl("Time shown in ", table$X1))){

    time <- table %>% filter(grepl("Time shown in ", X1)) %>%
      .[1,1] %>% as_vector()

  } else if(any(grepl("Time Offset ", table$X1))){

    time <- table %>% filter(grepl("Time Offset ", X1)) %>%
      .[1,1] %>% as_vector()

  }


  row_index <- which(table$X1 == "Date Time")
  nice_table <- table %>% slice(row_index:nrow(table))

  # Remove original column names
  colnames(nice_table) <- slice(nice_table, 1)

  nice_table <- slice(nice_table, 2:nrow(nice_table))

  names(nice_table) <- make.names(names(nice_table))

  if(str_detect(time, "UTC")){

    MST_DT <- nice_table %>%
      dplyr::mutate(site = site_name,
                    DT = lubridate::as_datetime(Date.Time, tz = "UTC")) %>%
      dplyr::mutate(DT = lubridate::with_tz(DT, tzone = "MST"),
                    DT_round = lubridate::round_date(DT, "15 minutes"),
                    DT_join = as.character(DT_round))

  } else if(str_detect(time, "MST")){

    MST_DT <- nice_table %>%
      dplyr::mutate(site = site_name,
                    DT = lubridate::as_datetime(Date.Time, tz = "MST")) %>%
      dplyr::mutate(DT = lubridate::with_tz(DT, tzone = "MST"),
                    DT_round = lubridate::round_date(DT, "15 minutes"),
                    DT_join = as.character(DT_round))

  } else if(str_detect(time, "Time Offset = -07:00:00")){

    MST_DT <- nice_table %>%
      dplyr::mutate(site = site_name,
                    DT = lubridate::as_datetime(Date.Time, tz = "MST"),
                    DT_round = lubridate::round_date(DT, "15 minutes"),
                    DT_join = as.character(DT_round))

  } else if(str_detect(time, "Time Offset = -06:00:00")){

    MST_DT <- nice_table %>%
      dplyr::mutate(site = site_name,
                    DT = lubridate::as_datetime(Date.Time, tz = "MST") - lubridate::hours(1),
                    DT_round = lubridate::round_date(DT, "15 minutes"),
                    DT_join = as.character(DT_round))

  } else if(str_detect(time, "in telemetry device's")){

    offset <- nice_table %>%
      filter(Date.Time == max(as.POSIXlt(Date.Time, tz="America/Denver"))) %>%
      mutate(tz = ifelse(dst(as.POSIXlt(Date.Time, tz="America/Denver")) == TRUE, 1, 0)) %>%
      pull(tz)

    MST_DT <- nice_table %>%
      dplyr::mutate(site = site_name,
                    DT = lubridate::as_datetime(Date.Time, tz = "MST") - lubridate::hours(offset),
                    DT_round = lubridate::round_date(DT, "15 minutes"),
                    DT_join = as.character(DT_round))

  }

  return(MST_DT)

}


pre2022 <- files %>%
  map(~old_data_loader(.))

# Find the row containing "Date"
date_row <- table %>% html_nodes("tr") %>%
  .[sapply(., function(row) any(grepl("Date", html_text(row))))]

# Extract column headers
column_headers <- date_row %>% html_nodes("th, td") %>%
  html_text(trim = TRUE)

# Read subsequent rows into a data frame
data <- date_row %>% html_nodes(xpath="./following-sibling::tr") #%>%
map(html_nodes, xpath="./th | ./td") %>%
  map(html_text, trim = TRUE) %>%
  matrix(ncol = length(column_headers), byrow = TRUE) %>%
  as.data.frame()

# Set column names
colnames(data) <- column_headers

# Now you have your data frame with the desired headers and data
print(data)
