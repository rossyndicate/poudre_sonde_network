sd_df <- map(all_data_flagged, ~.x %>%
               select(site, parameter, month, year, season, m_sd, m_sd_1090) %>%
               group_by(season) %>%
               mutate(diff = abs(m_sd - m_sd_1090)) %>%
               distinct(month, year, season, m_sd, m_sd_1090, .keep_all = TRUE)) %>%
  bind_rows()
