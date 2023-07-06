#' Import Larimer County Diversion Data
#'
#' This function pulls discharge data for Larimer County diversions within FC.
#'
#'
#' @param start Start date of when you want data.
#' @param end End date of when you want data; default is the current date.
#' @param save Whether to save (TRUE) the resulting table or not (FALSE)
#' @param path If `save = TRUE`, the file path to save the shapefile
#'
#' @return A table of time series flow data across the FC Floodwarning System
#'
lc_diversions <- function(start = '2018-10-01', end = Sys.Date(), save = TRUE, path = 'data/context_data/'){

  ts <- paste0("https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecday/?format=csv",
                 "&dateFormat=spaceSepToSeconds&encoding=gzip&min-dataMeasDate=",
                 str_sub(start, start=6L, end=7L),"%2F",
                 str_sub(start, start=9L, end=10L), "%2F",
                 str_sub(start, start=1L, end=4L),
                 "&max-dataMeasDate=",
                 str_sub(end, start=6L, end=7L), "%2F",
                 str_sub(end, start=9L, end=10L), "%2F",
                 str_sub(end, start=1L, end=4L),
                 "&wdid=0300926%2C+0300919%2C+0300918%2C+0300913%2C+0300914%2C+0301029%2C+0300915%2C+0300912")

  site <- "https://dwr.state.co.us/Rest/GET/api/v2/structures/?format=csv&wdid=0300926%2C+0300919%2C+0300918%2C+0300913%2C+0300914%2C+0301029%2C+0300915%2C+0300912"

  #download dataset from FC
  temp1 <- tempfile()
  temp2 <- tempfile()
  download.file(paste0(ts), destfile = temp1, method = "curl")
  download.file(paste0(site), destfile = temp2, method = "curl")
  data <- read_csv(temp1, skip = 2) %>%
    inner_join(., read_csv(temp2, skip = 2), by ="wdid")

  if(save == TRUE){
    write_csv(data, paste0(path, "/lc_diversions.csv"))
  }

  return(data)
}
