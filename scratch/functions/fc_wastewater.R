#' Import City of Fort Collins Wastewater Data
#'
#' This function pulls time series of daily City of Fort Collins wastewater in need of reclamation.
#'
#'
#' @param start Start date of when you want data; default is 10/01/2018.
#' @param end End date of when you want data; default is the current date.
#' @param save Whether to save (TRUE) the resulting table or not (FALSE)
#' @param path If `save = TRUE`, the file path to save the shapefile
#'
#' @return A table of time series flow data across the FC Floodwarning System
#'
fc_water_demand <- function(start = '2018-10-01', end = Sys.Date(), save = TRUE, path = 'data/context_data/'){

  call <- "https://opendata.fcgov.com/api/views/dn37-qz6d/rows.csv?accessType=DOWNLOAD"

  #download dataset from FC
  temp1 <- tempfile()
  download.file(paste0(call), destfile = temp1, method = "curl")
  data <- read_csv(temp1) %>%
    dplyr::mutate(DATE = ymd(DATE)) %>%
    dplyr::filter(DATE >= start & DATE <= end)

  if(save == TRUE){
    write_csv(data, paste0(path, "/fc_wastewater.csv"))
  }

  return(data)
}
