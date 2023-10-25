# Munge API data for QAQC workflow
# @param api_path Path to the API data.
# @return A dataframe with the munged API data.
# @examples
# munge_api_data(api_path = "data/api/")

munge_api_data <- function(api_path = "data/api/") {
  api_data <- list.files(path = api_path, full.names = TRUE, pattern = "*.csv") %>%
    map_dfr(~data.table::fread(.) %>% select(-id)) %>%
    # remove overlapping API-pull data
    distinct() %>%
    # remove VuLink data
    filter(!grepl("vulink", name, ignore.case = TRUE)) %>%
    # remove Virridy data (for now)
    filter(!grepl("virridy", name, ignore.case = TRUE)) %>%
    # Convert DT to MST:
    mutate(DT = as_datetime(timestamp, tz = "UTC")) %>%
    mutate(DT = with_tz(DT, tzone = "MST"),
           DT_round = round_date(DT, "15 minutes"),
           DT_join = as.character(DT_round),
           site = tolower(site)) %>%
    # filter for years that are 2022 and greater
    filter(year(DT_round) >= 2022) %>%
    # These sites will be considered the same site for this workflow
    mutate(site = ifelse(site == "rist", "tamasag",
                         ifelse(site == "elc", "boxelder", site))) %>%
    # Lastly, we swapped Boxelder's sonde out for Rist's late in 2022:
    mutate(site = ifelse(site == "tamasag" & DT > "2022-09-20" & DT < "2023-01-01", "boxelder", site))
  return(api_data)
}
