#' Import Fort Collins Floodwarning Rain Gage Data
#'
#' This function pulls rain data for sites within the FC Floodwarning System
#'
#'
#' @param start Start date of when you want data.
#' @param end End date of when you want data; default is the current date.
#' @param save Whether to save (TRUE) the resulting table or not (FALSE)
#' @param path If `save = TRUE`, the file path to save the shapefile
#'
#' @return A table of time series flow data across the FC Floodwarning System
#'
fc_rain <- function(start = '2018-10-01', end = Sys.Date(), save = TRUE, path = 'data/context_data/'){

  call <- "https://opendata.fcgov.com/api/views/g87z-rviz/rows.csv?accessType=DOWNLOAD"

  #download dataset from FC
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  data <- read_csv(temp1) %>%
    dplyr::mutate(date = as_date(mdy_hms(Timestamp))) %>%
    dplyr::filter(date >= start & date <= end)

  if(save == TRUE){
    write_csv(data, paste0(path, "/fc_rain.csv"))
  }

  return(data)
}
