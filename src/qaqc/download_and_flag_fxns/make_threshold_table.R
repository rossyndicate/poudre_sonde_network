make_threshold_table <- function(df){
  
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
