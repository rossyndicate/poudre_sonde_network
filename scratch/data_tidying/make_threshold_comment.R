# df <- df %>%
  #   mutate(mean = ifelse(grepl("outside of sensor specification range", flag), ) %>%
  #   group_by(season) %>%
  #   site = paste0(unique(site)),
  #   parameter = paste0(unique(parameter)),
  #   mutate(p01 = quantile(mean, 0.01, na.rm = TRUE),
  #          p99 = quantile(mean, 0.99, na.rm = TRUE)) %>%
  #   # need to pull some of this info out and use it for the range limits to test it
  #   summarize(m_sd_0199 = sd(mean, na.rm = T)) %>%
  #   select(season, m_sd_0199),
  # by = "season")