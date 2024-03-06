# Munge API data for QAQC workflow
# @param api_path Path to the API data.
# @return A dataframe with the munged API data.
# @examples
# munge_api_data(api_path = "data/api/")

munge_api_data <- function(api_path, require = NULL) {

  api_data <- list.files(path = api_path, full.names = TRUE, pattern = "*.csv") %>%
    map_dfr(~fread(.) %>% select(-id)) %>%
    # remove overlapping API-pull data
    distinct() %>%
    # remove VuLink data
    filter(!grepl("vulink", name, ignore.case = TRUE)) %>% # these will throw us errors if there is none?
    # remove Virridy data (for now)
    filter(!grepl("virridy", name, ignore.case = TRUE)) %>%
    select(-name) %>%
    # Convert UTC (as it is sent from HydroVU API) to MST:
    mutate(DT = as_datetime(timestamp, tz = "UTC")) %>%
    mutate(DT = with_tz(DT, tzone = "MST"),
           DT_round = round_date(DT, "15 minutes"),
           DT_join = as.character(DT_round),
           site = tolower(site)) %>%
    # These sites will be considered the same site for this workflow
    mutate(site = ifelse(site == "rist", "tamasag",
                  ifelse(site == "elc", "boxelder", site))) %>%
    # Lastly, we swapped Boxelder's sonde out for Rist's late in 2022:
    mutate(site = ifelse(site == "tamasag" & DT > ymd("2022-09-20", tz = "MST") & DT < ymd("2023-01-01", tz = "MST"), "boxelder", site))

  return(api_data)

}
