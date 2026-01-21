library(tidyverse)
library(here)
library(arrow)

#Diana had asked for some "general" ranges for parameters in the PWQN sites
# So here is a quick analysis of seasonal means, 95th and 5th percentiles for key parameters at key sites to give an overall view of the system

quick_analysis <- read_parquet(here("data/collated/sensor/compiled_all_sensor_data_2026-01-06.parquet"))

quick_analysis_table <- quick_analysis%>%
  filter(site %in% c("bellvue", "salyer", "udall", "riverbend", "cottonwood", "elc", "archery",  "riverbluffs"))%>%
  filter(parameter %in% c("pH", "Depth", "Specific Conductivity", "Turbidity", "Temperature", "DO"))%>%
  filter(!(parameter == "pH" & mean < 6))%>% #removing instances where sonde was recording in pH 4
  filter(!(parameter == "Turbidity" & mean > 400))%>% # removing outliers
  mutate( month = month(DT_round),
          season = case_when(month %in% c(11, 12, 1, 2, 3,4) ~ "Winter Baseflow",
                             month %in% c(5,6) ~ "Snowmelt",
                             month %in% c(7,8) ~ "Summer Monsoon",
                             month %in% c(9,10) ~ "Fall Baseflow"
          ))%>%
  group_by(site, parameter, season)%>%
  summarise(seasonal_mean = mean(hourly_median, na.rm = T),
            q95 = quantile(hourly_median, 0.99, na.rm = T),
            q5 = quantile(hourly_median, 0.01, na.rm = T),
            sd = sd(hourly_median, na.rm = T) )%>%
  ungroup()%>%
  mutate(upper_lower = if_else(site %in% c("bellvue", "salyer", "udall"),
                               "Upper PWQN", "Lower PWQN"))

quick_analysis_table$season <- factor(quick_analysis_table$season,
                                      levels = c("Winter Baseflow", "Snowmelt", "Summer Monsoon", "Fall Baseflow"))
ggplot(quick_analysis_table%>%
         filter(upper_lower == "Upper PWQN"), aes(x = season, y = seasonal_mean, color = site))+
  geom_point()+
  geom_point(aes(y = q95), shape = 2)+
  geom_point(aes(y = q5), shape = 6)+
  facet_wrap(~parameter, scales = "free_y")+
  labs(y = "Seasonal Mean (circle), 95th Percentile (triangle), 5th Percentile (inverted triangle)",
       x = "Season",
       color = "Site",
       title = "Seasonal Summary of Sensor Data by Site and Parameter",
       subtitle = " `Upper PWQN` Sites: Bellvue, Salyer, Udall")

ggplot(quick_analysis_table%>%
         filter(upper_lower == "Lower PWQN"), aes(x = season, y = seasonal_mean, color = site))+
  geom_point()+
  geom_point(aes(y = q95), shape = 2)+
  geom_point(aes(y = q5), shape = 6)+
  facet_wrap(~parameter, scales = "free_y")+
  labs(y = "Seasonal Mean (circle), 95th Percentile (triangle), 5th Percentile (inverted triangle)",
       x = "Season",
       color = "Site",
       title = "Seasonal Summary of Sensor Data by Site and Parameter",
       subtitle = " `Lower PWQN` Sites: Riverbend, Cottonwood, ELC, Archery, Riverbluffs")
