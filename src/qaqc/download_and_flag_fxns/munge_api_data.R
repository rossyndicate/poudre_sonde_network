#' Munge API data for QAQC workflow
#' @param api_path Path where the raw API data lives.
#' @return A dataframe with the munged API data.
#' @require An upstream dependency (i.e., alternative to `{necessary object}`).
#' @examples
# munge_api_data(api_path = "data/api/")

munge_api_data <- function(api_path, require = NULL) {

  api_data <- list.files(path = api_path, full.names = TRUE, pattern = "*.csv") %>%
    purrr::map_dfr(~data.table::fread(.) %>%
                     dplyr::select(-id)) %>%
    # remove overlapping API-pull data
    dplyr::distinct() %>%
    # remove VuLink data
    dplyr::filter(!grepl("vulink", name, ignore.case = TRUE)) %>% # these will throw us errors if there is none?
    # remove Virridy data
    dplyr::filter(!grepl("virridy", name, ignore.case = TRUE)) %>%
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
    dplyr::mutate(site = ifelse(site == "tamasag" & DT > lubridate::ymd("2022-09-20", tz = "MST") & DT < lubridate::ymd("2023-01-01", tz = "MST"), "boxelder", site))

  return(api_data)

}
