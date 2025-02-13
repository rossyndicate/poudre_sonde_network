#' @title Munge API data for QAQC workflow
#'
#' @description
#' A function designed to munge the raw API data for the QAQC workflow.
#'
#' @param api_path Path where the raw API data lives.
#'
#' @return A dataframe with the munged API data.
#'
#' @examples
# munge_api_data(api_path = "data/api/incoming/")

munge_api_data <- function(api_path) {

  api_data <- list.files(path = api_path, full.names = TRUE, recursive = TRUE, pattern = "*.csv") %>%
    purrr::map_dfr(~data.table::fread(.) %>%
                     dplyr::select(-id)) %>%
    # remove overlapping API-pull data
    dplyr::distinct()

  api_data <- api_data %>%
    # remove VuLink data and Virridy sondes
    dplyr::filter(!grepl("vulink|virridy", name, ignore.case = TRUE)) %>%
    dplyr::select(-name) %>%
    # Convert UTC (as it is sent from HydroVU API) to MST:
    dplyr::mutate(DT = lubridate::as_datetime(timestamp, tz = "UTC")) %>%
    dplyr::mutate(DT = lubridate::with_tz(DT, tzone = "MST"),
           DT_round = lubridate::round_date(DT, "15 minutes"),
           DT_join = as.character(DT_round),
           site = tolower(site)) %>%
    # These sites will be considered the same site for this workflow
    dplyr::mutate(site = ifelse(site == "rist", "tamasag",
                  ifelse(site == "elc", "boxelder", site))) %>%
    # Lastly, we swapped Boxelder's sonde out for Rist's late in 2022:
    dplyr::mutate(site = ifelse(site == "tamasag" & DT > lubridate::ymd("2022-09-20", tz = "MST") & DT < lubridate::ymd("2023-01-01", tz = "MST"), "boxelder", site)) %>%
    dplyr::distinct(.keep_all = TRUE)

  return(api_data)

}
