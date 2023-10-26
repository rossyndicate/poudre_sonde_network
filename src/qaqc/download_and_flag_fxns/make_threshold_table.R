make_threshold_table <- function(df){

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


table <- df %>%
  group_by(season) %>%
  summarize(site = paste0(unique(site)),
            parameter = paste0(unique(parameter)),
            t_mean01 = as.numeric(paste0(unique(m_mean01))),
            t_mean99 = as.numeric(paste0(unique(m_mean99))),
            t_slope_behind_99 = as.numeric(paste0(unique(m_slope_behind_99))),
            t_sd_0199  = as.numeric(paste0(unique(m_sd_0199))))
return(table)
}
