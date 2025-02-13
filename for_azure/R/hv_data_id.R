#' @title Get data from HydroVu by location ID
#' 
#' @description
#' A function that retrieves data from HydroVu by location ID.
#' 
#' @param loc_id A character string with the location ID.
#' @param start_time A character string with the start time.
#' @param end_time A character string with the end time.
#' @param tz A character string with the timezone.
#' @param token A character string with the token for the HydroVu API.
#' 
#' @return A data frame with the HydroVu data.
#' 
#' @examples
#' hv_data_id(loc_id = "1", start_time = "2021-01-01 00:00:00", end_time = "2021-01-02 00:00:00", tz = "America/Denver", token = "your_token")

hv_data_id <- function(loc_id, start_time = startdate, end_time = enddate, tz = timezone, token) {

  # convert the time to timestamp, convert to UTC for lookup in HydroVu
  start <- as.numeric(lubridate::with_tz(lubridate::ymd_hms(start_time, tz = tz), tzone = "UTC"))
  end <- as.numeric(lubridate::with_tz(lubridate::ymd_hms(end_time, tz = tz), tzone = "UTC"))

  # build the url
  url = "https://www.hydrovu.com/public-api/v1/locations/"
  url <- paste0(url, loc_id, "/data?endTime=", end, '&startTime=', start)

  req <- httr2::request(url)
  print(paste0('Trying site ', loc_id))
  try({
    resp <-  req %>% httr2::req_oauth_client_credentials(token) %>% httr2::req_perform()
    data <- list(resp %>% httr2::resp_body_json())
    h <- resp %>% httr2::resp_headers()

    while (!is.null(h[["X-ISI-Next-Page"]]))
    {
      resp <- req %>% httr2::req_headers("X-ISI-Start-Page" = h[["X-ISI-Next-Page"]]) %>%
        httr2::req_oauth_client_credentials(token) %>% httr2::req_perform()
      data <- c(data, list(resp %>% httr2::resp_body_json()))
      h <- resp %>% httr2::resp_headers()
    }

    # get the params and units
    params <- hv_names(token, return = "params")
    units <- hv_names(token, return = "units")

    # collapse the paginated date and clean up
    df <- purrr::map_dfr(data, flatten_page_params) %>%
      dplyr::mutate(timestamp = lubridate::with_tz(lubridate::as_datetime(timestamp, tz = "UTC"), tzone = tz),
                    Location = loc_id) %>%
      dplyr::inner_join(params, by = "parameterId") %>%
      dplyr::inner_join(units, by = "unitId") %>%
      dplyr::select(-parameterId, -unitId) %>%
      dplyr::arrange(Parameter, timestamp)

      return(df)
  })

}
