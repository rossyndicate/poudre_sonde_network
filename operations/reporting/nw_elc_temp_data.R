library(tidyverse)
library(arrow)
library(lubridate)

# 1. Define file paths (using a vector to make it easy to iterate)
files <- c(
  "data/raw/sensor/manual_data_verification/2022_cycle/hydro_vu_pull/flagged/elc_Temperature.parquet",
  "data/raw/sensor/manual_data_verification/2022_cycle/hydro_vu_pull/flagged/elc_Specific Conductivity.parquet",
  "data/raw/sensor/manual_data_verification/2022_cycle/hydro_vu_pull/flagged/archery_Temperature.parquet",
  "data/raw/sensor/manual_data_verification/2022_cycle/hydro_vu_pull/flagged/archery_Specific Conductivity.parquet"
)

# 2. Read and combine all data into one "long" dataframe first
all_data <- map_df(files, read_parquet) %>%
  mutate(
    DT_MT = with_tz(DT_round, tzone = "America/Denver"),
    last_site_visit_MT = with_tz(last_site_visit, tzone = "America/Denver")
  ) %>%
  # 3. Apply cleaning filters
  filter(
    !grepl("sonde unsubmerged|site visit", auto_flag))

# 4. Transform to Wide Format
# This creates columns like: Temperature_mean, Temperature_flag, etc.
wide_data <- all_data %>%
  select(DT_MT, site, parameter, mean, auto_flag) %>%
  mutate(parameter = case_when(
    parameter == "Temperature" ~ "Temp_C",
    parameter == "Specific Conductivity" ~ "SC_uscm",
    TRUE ~ parameter
  )) %>%
  pivot_wider(
    names_from = parameter,
    values_from = c(mean, auto_flag),
    names_glue = "{parameter}_{.value}"
  )%>%
  #remove mean suffix
  rename_with(~ str_replace(., "_mean", ""))

# 5. Split by site and save CSVs
wide_data %>%
  group_split(site) %>%
  walk(~ {
    site_name <- unique(.x$site)
    file_path <- paste0("docs/pwqn/shared/nw_elc_temp/", site_name, "_2022.csv")
    write_csv(.x, file_path)
    message("Saved: ", file_path)
  })

# 6. Quick Visualization Check (Example: Archery SC)
wide_data %>%
  ggplot(aes(x = DT_MT, y = Temp_C)) +
  geom_point(aes(color = `SC_uscm_auto_flag`), alpha = 0.5) +
  facet_wrap(~ site) +
  labs(title = "Archery Specific Conductivity (Cleaned)")+
  theme(legend.position = "bottom")
