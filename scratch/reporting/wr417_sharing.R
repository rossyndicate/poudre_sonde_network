
library(tidyverse)
data <- readRDS("data/virridy_verification/all_data_flagged_complete.RDS")


sites_selected <- c("legacy", "timberline", "lincoln")

select_param <- c("Temperature", "Specific Conductivity", "pH", "DO", "Depth")


site_param_combos <- crossing(site_select = sites_selected, param = select_param) %>%
  mutate(combo = paste(site_select, param, sep = "-")) %>%
  pull(combo)

#grab the data for the site and parameter combos the user selected
all_select_data <- site_param_combos %>%
  keep(~ .x %in% names(data)) %>%
  map_dfr(~ data[[.x]])
#grab start and end dates
start_date <- as.POSIXct("2023-01-01")%>% force_tz("MST")
end_date <- as.POSIXct("2024-01-01")%>% force_tz("MST")

#remove basic bad data
bad_data <- tibble(site = c("lincoln"),
                     start_dt = c("2024-08-22"),
                     end_dt = c("2024-08-28"))%>%
  mutate(start_dt = as.POSIXct(start_dt, tz = "MST"),
         end_dt = as.POSIXct(end_dt, tz = "MST"),
         remove = T)


# # Do any transformations...
# if(input$transformation == "None"){
#   #trim to just the dates selected
trim_select_data <- all_select_data %>%
  filter(DT_round >= start_date & DT_round <= end_date)%>%
  #convert depth to ft (easier to see changes than meters)
  mutate(mean = case_when(parameter == "Depth"~ mean*3.28084,
                          TRUE ~ mean))%>%
  #remove rows where "missing data" is included in the flag string but still include rows with NA
  filter(grepl("missing data", flag) == FALSE | is.na(flag))%>%
  #remove all flagged data
  filter(is.na(flag))

final_data <- trim_select_data%>%
  filter(!(site == "lincoln" & DT_round > as.POSIXct("2023-08-22", tz = "MST") & DT_round < as.POSIXct("2023-08-29", tz = "MST")))%>%
#filter(!(site == "timberline" & DT_round > as.POSIXct("2023-05-02", tz = "MST") & DT_round < as.POSIXct("2023-05-04", tz = "MST")))%>%
  #param filters
  filter(!(parameter == "pH" & mean < 5))%>%
  filter(!(site == "legacy" & parameter == "Depth" & DT_round > as.POSIXct("2023-11-25", tz = "MST") & DT_round < as.POSIXct("2023-12-03", tz = "MST")))

# using the bad_data tibble, filter out those instances in trim select data


#make a plot of the data
p <- ggplot(final_data, aes(x = DT_round, y = mean, color = site)) +
  geom_point() +
  geom_line() +
  facet_wrap(~parameter, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Data for Selected Sites and Parameters",
       x = "Date",
       y = "Value",
       color = "Site")
ggplotly(p)

concise_df_long <- final_data %>%
  select(site, parameter, DT_round_mst = DT_round, mean)%>%
  mutate(parameter = case_when(parameter == "Temperature" ~ "Temperature_C",
                               parameter == "Specific Conductivity" ~ "Specific_Conductivity_uS/cm",
                               parameter == "pH" ~ "pH",
                               parameter == "DO" ~ "DO_mg/L",
                               parameter == "Depth" ~ "Depth_ft"))

concise_df_wide <- concise_df_long %>%
  pivot_wider(names_from = parameter, values_from = mean, id_cols = c(site, DT_round_mst))



#save to CSV and RDS
write_csv(concise_df_long, "data/sharing/wr417_2023_data/concise_df_long.csv")
write_csv(concise_df_wide, "data/sharing/wr417_2023_data/concise_df_wide.csv")
write_rds(concise_df_long, "data/sharing/wr417_2023_data/concise_df_long.RDS")
write_rds(concise_df_wide, "data/sharing/wr417_2023_data/concise_df_wide.RDS")

