# AI description of what my code is doing :)
# This script identifies and summarizes "bad data days" for continuous
# water quality sensors in 2023 and 2024 across multiple monitoring sites.
#
# Two types of bad data days are flagged:
#   (1) Sensor malfunction days, where field notes indicate issues
#       (e.g., sensor malfunction, burial, biofouling) and at least
#       half of the day’s observations are affected.
#   (2) Missing-data days, identified using temperature records as a
#       proxy for full sensor outages when at least half of the day’s
#       values are NA.
#
# For each year, the script:
#   - Aggregates bad data to the site–day level
#   - Counts total monitoring days and bad data days per site
#   - Calculates the percentage of bad days by site
#   - Compares 2023 vs. 2024 bad-day totals and percentages
#   - Produces side-by-side visualizations showing the timing of bad
#     data days by site across each monitoring season


# Bad data type 1: sensor malfunction
counts_2023 <- post_verified_data_2023 %>%
  # grab flagged data instances
  filter(flag_better_cat %in% c("needs a flag", "properly flagged")) %>%
  # ... that also had a known instance of poor data from field notes
  filter(grepl("sensor mal|burial|biofoul", flag)) %>%
  # ... that are not NA
  filter(!is.na(value)) %>%
  mutate(site_id = as.numeric(factor(site, levels = c("tamasag", "legacy",
                                                      "lincoln", "timberline",
                                                      "prospect", "boxelder",
                                                      "archery", "riverbluffs")))) %>%
  group_by(site, site_id, parameter, date) %>%
  summarize(total = n()) %>%
  # a "down day" = a day with nearly half of its data "bad"
  filter(total >= 64) %>%
  ungroup() %>%
  distinct(site, site_id, date)

# bad data type 2: no data
counts_2023_p2 <- post_verified_data_2023 %>%
  # choose temperature, since if temp is missing, likely all sensors missing
  filter(parameter == "Temperature") %>%
  filter(is.na(value)) %>%
  mutate(site_id = as.numeric(factor(site, levels = c("tamasag", "legacy",
                                                      "lincoln", "timberline",
                                                      "prospect", "boxelder",
                                                      "archery", "riverbluffs")))) %>%
  group_by(site, site_id, parameter, date) %>%
  summarize(total = n()) %>%
  # half the day's data is missing
  filter(total >= 64) %>%
  ungroup() %>%
  select(site, site_id, date) %>%
  # combine the two "bad data" approaches
  bind_rows(counts_2023) %>%
  distinct(site, site_id, date)

counts_2024 <- read_feather('psn_2024.feather') %>%
  filter(!grepl("virridy", site, ignore.case = TRUE)) %>%
  filter(!parameter %in% c("Depth", "FDOM Fluorescence")) %>%
  filter(site %in% c("tamasag", "legacy", "lincoln", "timberline", "prospect",
                     "boxelder", "archery", "riverbluffs")) %>%
  mutate(site_id = as.numeric(factor(site, levels = c("tamasag", "legacy",
                                                      "lincoln", "timberline",
                                                      "prospect", "boxelder",
                                                      "archery", "riverbluffs")))) %>%
  mutate(date = as_date(DT_join)) %>%
  filter(!is.na(flag) & !grepl("suspect", flag)) %>%
  filter(grepl("sensor mal|burial|biofoul", flag)) %>%
  # lincoln had really bad turbidity drift that we don't want to include
  filter(!grepl("biofoul", flag) & site != "lincoln") %>%
  filter(!is.na(value)) %>%
  group_by(site, site_id, parameter, date) %>%
  summarize(total = n()) %>%
  filter(total >= 64) %>%
  ungroup() %>%
  distinct(site, site_id, date)

counts_2024_p2 <- read_feather('psn_2024.feather') %>%
  mutate(date = as_date(DT_join)) %>%
  filter(!grepl("virridy", site, ignore.case = TRUE)) %>%
  filter(!parameter %in% c("Depth", "FDOM Fluorescence")) %>%
  filter(site %in% c("tamasag", "legacy", "lincoln", "timberline", "prospect",
                     "boxelder", "archery", "riverbluffs")) %>%
  filter(parameter == "Temperature") %>%
  filter(is.na(value)) %>%
  mutate(site_id = as.numeric(factor(site, levels = c("tamasag", "legacy",
                                                      "lincoln", "timberline",
                                                      "prospect", "boxelder",
                                                      "archery", "riverbluffs")))) %>%
  group_by(site, site_id, parameter, date) %>%
  summarize(total = n()) %>%
  filter(total >= 64) %>%
  ungroup() %>%
  select(site, site_id, date) %>%
  bind_rows(counts_2024) %>%
  distinct(site, site_id, date)

total_days_2023 <- post_verified_data_2023 %>%
  mutate(date = as_date(DT_join)) %>%
  distinct(site, date) %>%
  group_by(site) %>%
  summarize(days_2023 = n()) %>%
  left_join(., counts_2023_p2 %>% group_by(site) %>% summarize(bad_days_2023 = n()))

total_days_2024 <-read_feather('psn_2024.feather') %>%
  filter(site %in% c("tamasag", "legacy", "lincoln", "timberline", "prospect",
                     "boxelder", "archery", "riverbluffs")) %>%
  mutate(date = as_date(DT_join)) %>%
  distinct(site, date) %>%
  group_by(site) %>%
  summarize(days_2024 = n()) %>%
  left_join(., counts_2024_p2 %>% group_by(site) %>% summarize(bad_days_2024 = n()))

days <- inner_join(total_days_2023, total_days_2024) %>%
  mutate(bad_days_2024 = ifelse(is.na(bad_days_2024), 0, bad_days_2024)) %>%
  mutate(perc_days_2023 = 100 * (bad_days_2023 / days_2023),
         perc_days_2024 = 100 * (bad_days_2024 / days_2024)) %>%
  mutate(dif = (perc_days_2023 - perc_days_2024))

# difference between 2023 and 2024...
sum(days$bad_days_2023, na.rm = TRUE) - sum(days$bad_days_2024, na.rm = TRUE)

# new site names
site_order <- c("Bellvue", "Salyer", "Udall", "Riverbend", "Cottonwood", "ELC", "Archery", "River Bluffs")

# complete dataset for each year that includes all sites:
complete_2023 <- expand.grid(date = seq(date_range_2023[1], date_range_2023[2], by = "day"),
                             natural_name = site_order) %>%
  mutate(natural_name = factor(natural_name,
                               levels = rev(site_order)))

complete_2024 <- expand.grid(date = seq(date_range_2024[1], date_range_2024[2], by = "day"),
                             natural_name = site_order) %>%
  mutate(natural_name = factor(natural_name,
                               levels = rev(site_order)))

# site names with count data
counts_2023 <- counts_2023_p2 %>%
  left_join(site_names, by = c("site" = "site")) %>%
  # add factor levels for site ordering
  mutate(natural_name = factor(natural_name,
                               levels = rev(site_order)))

counts_2024 <- counts_2024_p2 %>%
  left_join(site_names, by = c("site" = "site")) %>%
  # add factor levels for site ordering
  mutate(natural_name = factor(natural_name,
                               levels = rev(site_order)))

# plots...

plot_theme_2023 <- theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

plot_theme_2024 <- theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# 2023 plot
plot_2023 <- ggplot() +
  # add full dataset as invisible points to maintain structure
  geom_point(data = complete_2023,
             aes(x = date, y = natural_name),
             color = "transparent") +
  geom_point(data = counts_2023,
             aes(x = date, y = natural_name, color = natural_name),
             size = 3) +
  scale_x_date(limits = date_range_2023,
               date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_discrete(limits = rev(site_order)) +
  scale_color_manual(values = color_palette) +
  labs(x = "2023", y = "Site") +
  plot_theme_2023

# 2024 plot
plot_2024 <- ggplot() +
  # add full dataset as invisible points to maintain data structure
  geom_point(data = complete_2024,
             aes(x = date, y = natural_name),
             color = "transparent") +
  geom_point(data = counts_2024,
             aes(x = date, y = natural_name, color = natural_name),
             size = 3) +
  scale_x_date(limits = date_range_2024,
               date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_discrete(limits = rev(site_order)) +
  scale_color_manual(values = color_palette) +
  labs(x = "2024", y = NULL) +
  plot_theme_2024


ggarrange(plot_2023,
          plot_2024,
          align = "h",
          widths = c(1.2, 1))
