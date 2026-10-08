
library(tidyverse)
library(httr2)
library(rvest)
library(lubridate)
library(arrow)
library(here)

#' Fetch Water Quality Grab Sample Data from Northern Water KiWIS API
#' Info available here:https://www.northernwater.org/data/data-web-services
#' Data Portal:https://data.northernwater.org/applications/public.html?publicuser=Public#waterdataviewer/stationoverview
#'
#' @param station_no Character. The station identifier code (e.g., "FS-0070").
#' @param params Character vector (optional). Specific parameter names to filter
#'   (matches against `parametertype_name`). If `NULL`, all parameters are returned.
#' @param datasource Character. KiWIS data source code. Defaults to `"0"`.
#' @param quality_filter Character (optional). Filter data by quality status.
#'   One of `"Final"`, `"Approved"`, `"Raw"`, or `NULL` for no filtering.
#' @param period Character. Time window string for KiWIS (e.g., `"P20Y"` for past 20 years).
#'
#' @return A tibble containing raw or filtered water quality grab samples with station details.
#' @export
get_kisters_grab_info <- function(station_no,
                                  params = NULL,
                                  datasource = "0",
                                  quality_filter = NULL,
                                  period = "P20Y") {

  # Base URL for Northern Water KiWIS API
  base_url <- "https://data.northernwater.org/KiWIS/KiWIS"

  # Define requested fields
  return_fields <- paste(
    c("station_name", "sample_timestamp", "parametertype_name", "wqm_trace_no",
      "wqm_spot_label", "sample_depth", "sample_final_depth", "value_sign",
      "value", "unit_symbol","value_remark", "value_quality", "value_lower_qlimit",
      "value_lower_dlimit","value_labcomment", "sample_consecutive_no"),
    collapse = ","
  )

  # Build httr2 request
  req <- request(base_url) %>%
    req_url_query(
      datasource = datasource,
      format = "html",
      period = period,
      request = "getwqmsamplevalues",
      service = "kisters",
      station_no = station_no,
      type = "queryServices",
      returnfields = return_fields,
      dateformat = "yyyy-MM-dd HH:mm:ss"
    )

  # Perform HTTP request and parse HTML table output
  response <- req %>%
    req_perform() %>%
    resp_body_html()

  ts_id_table <- response %>%
    html_element("body") %>%
    html_element("table") %>%
    html_table(header = TRUE) %>%
    as_tibble()

  # Handle empty response tables gracefully
  if (nrow(ts_id_table) == 0) {
    return(tibble())
  }

  # Filter by quality status if specified (checking 'value_quality' column)
  if (!is.null(quality_filter) && "value_quality" %in% names(ts_id_table)) {
    ts_id_table <- ts_id_table %>%
      filter(str_detect(value_quality, regex(quality_filter, ignore_case = TRUE)))
  }

  # Filter by parameter types if specified
  if (!is.null(params) && "parametertype_name" %in% names(ts_id_table)) {
    ts_id_table <- ts_id_table %>%
      filter(parametertype_name %in% params)
  }

  # Ensure numeric values and append input station number
  ts_id_table %>%
    filter(!is.na(station_name), station_name != "") %>%
    mutate(
      station_no = station_no,
      value = as.numeric(value)
    )
}

# PWQN Specific Site Metadata & Data Fetching

# Define Northern Water sites on the Poudre River
poudre_nw_sites <- tibble(
  site = c("pman", "pbd", "bellvue", "salyer", "udall", "riverbend",
           "springcreek", "cottonwood", "elc", "archery", "riverbluffs"),
  station_no = c("FS-0070", "FS-0239", "FS-0060", "FS-0058", "FS-0268", "FS-0059",
                 "FS-0276", "FS-0269", "FS-0066", "FS-0061", "FS-0062"),
  exact_site_match = c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
)

# Iterate across sites, fetch grab sample data, and combine into a single tibble
poudre_nw_data_raw <- poudre_nw_sites$station_no %>%
  map(get_kisters_grab_info) %>%
  keep(~ nrow(.) > 0) %>%
  bind_rows() %>%
  left_join(poudre_nw_sites, by = "station_no")

# Clean datetimes and parse to local Mountain Time
poudre_nw_data_clean <- poudre_nw_data_raw %>%
  mutate(datetime = with_tz(ymd_hms(sample_timestamp), tz = "America/Denver"),
         DT_round = with_tz(floor_date(datetime, unit = "15 minute"), tz = "UTC"))%>%
select(site, DT_round, parametertype_name, value, unit_symbol, everything())

# Quick Data Viz

ggplot(poudre_nw_data_clean, aes(x = datetime, y = value, color = site)) +
  geom_point(alpha = 0.7, size = 1.8) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ parametertype_name, scales = "free_y") +
  labs(
    title = "Northern Water Poudre River Grab Sample Data",
    subtitle = "Historical water quality parameters by monitoring site",
    x = "Date",
    y = "Measured Value",
    color = "Site ID"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

max_date <- as.Date(max(poudre_nw_data_clean$DT_round), na.rm = T)

file_name <- paste0("data/raw/chem/northern_water_chem/nw_grab_wq_", max_date, ".parquet")

write_parquet(x = poudre_nw_data_clean, sink = here(file_name) )
