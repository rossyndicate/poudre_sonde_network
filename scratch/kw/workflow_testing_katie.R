# Load necessary packages:
source("src/package_loader.R")
lapply(c("data.table", "tidyverse", "rvest", "readxl", "lubridate", "zoo", "padr","plotly", "feather", "RcppRoll", "yaml", "ggpubr", "profvis", "janitor"), package_loader)

walk(list.files('src/qaqc/download_and_flag_fxns', pattern = "*.R", full.names = TRUE, recursive = TRUE), source)

walk(list.files('src/mWater_collate/', pattern = "*.R", full.names = TRUE, recursive = TRUE), source)

old_field_notes <- readxl::read_excel('data/sensor_field_notes.xlsx') %>%
  clean_field_notes()

new_field_notes <- grab_mWater_sensor_notes()

#merge new mwater notes (sensor_notes) and old notes (field notes)
all_field_notes <- rbind(old_field_notes, new_field_notes)
all_data <- munge_api_data(api_path = "data/api/krw_everything_backup2.0_mdt_dl/") %>%
  dplyr::filter(lubridate::year(DT_round) >= 2022)

# Does this data look correct? There have been so many renamings that make me worried...

data_availability <- all_data %>%
  group_by(site) %>%
  summarize(n_years = n_distinct(lubridate::year(DT_round)))


ggplot(data = all_data %>% filter(parameter == "Temperature")) +
  geom_line(aes(x=DT_round, y = value)) +
  facet_wrap(~site, ncol = 1)
# ... this actually appears correct.

# Are avg. temps in the right ballpark?
all_data %>%
  group_by(site) %>%
  filter(parameter == "Temperature") %>%
  filter(value > 0 & value < 30) %>%
  summarize(mean = mean(value, na.rm = TRUE)) %>%
  arrange(., mean)
#... yes, they are! Especially before cleaning....


# Now, I'm comparing the DTs by looking at a specific day...
sub_dt <- all_data %>%
  dplyr::filter(parameter == "pH") %>%
  dplyr::filter(as_date(DT_round) == as_date("2023-04-22"))

## Testing the intrasensor biofouling check:

turbidity <-  (intrasensor_checks[["prospect.Turbidity"]]) %>% filter(month(DT_join) == 10 & year(DT_join) == 2022)

turb_biofouling <- turbidity %>% mutate(bf = ifelse(grepl("biof", flag), "Y", "N")) %>% filter(bf == "Y")

turb_biofouling_no_sensor_mal <- turb_biofouling %>% filter(!grepl("sensor mal", flag))

ggplot() +
  geom_point(data = turbidity, aes(x = DT_join, y = mean)) +
  geom_point(data = turb_biofouling, aes(x = DT_join, y = mean), color = "yellow") +
  geom_point(data = turb_biofouling_no_sensor_mal, aes(x = DT_join, y = mean), color = "blue")
