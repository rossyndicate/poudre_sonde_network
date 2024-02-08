library(tidyverse)
library(arrow)
library(plotly)
library(ggpubr)

grab_raw_data <- function(sites = c("boxelder", "tamasag", "legacy"),
                      parameters = c("Turbidity", "Temperature"),
                      start_dt = "start", end_dt = "end", return_all = FALSE){


all_data_hist <- read_feather("data/pretty/all_data_15min.feather")
all_data_newer <- readRDS("data/flagged/sjs_all_data_flagged.RDS")%>%
  bind_rows()

hist_tidy <- all_data_hist%>%
  pivot_longer(cols = c(Depth_ft, Temperature_C, Specific_Conductivity_µS_cm, DO_ppm, pH, Turbidity_NTU, Chla), names_to = "parameter", values_to = "value")%>%
  filter(!is.nan(value))%>%
  mutate(parameter = case_when(
    parameter == "Temperature_C" ~ "Temperature",
    parameter == "Depth_ft" ~ "Depth",
    parameter == "Turbidity_NTU" ~ "Turbidity",
    parameter == "DO_ppm" ~ "DO",
    parameter == "pH" ~ "pH",
    parameter == "Specific_Conductivity_µS_cm" ~ "Specific Conductivity",
    parameter == "Chla" ~ "Chl-a Fluorescence",
    TRUE ~ "NA"),
  cleaner_flag = NA_character_)%>%
  #pivot_wider(names_from = parameter, values_from = value)%>%
  select(-tracer)%>%
  rename(DT_round = DT)


newer_tidy <- all_data_newer %>%
  filter(year == 2023)%>%
  select(DT_round, parameter, site, value = mean, cleaner_flag)%>%
  #pivot_wider(names_from = parameter, values_from = mean)%>%
  mutate(value = case_when(parameter == "Depth"~ value * 3.28084,
                           TRUE ~ value))


all <- bind_rows(hist_tidy, newer_tidy)%>%
  mutate(unit = case_when(parameter == "Temperature" ~ "C",
                          parameter == "Depth" ~ "ft",
                          parameter == "Turbidity" ~ "NTU",
                          parameter == "Specific Conductivity" ~ "µS_cm",
                          parameter == "DO" ~ "mgL",
                          parameter == "Chl-a Fluorescence" ~ "RFU"))
if("boxelder" %in% sites){
  sites = c(sites, "elc")
}
if("tamasag" %in% sites){
  sites = c(sites, "rist")
}
if("rist" %in% sites){
  sites = c(sites, "tamasag")
}
if("elc" %in% sites){
  sites = c(sites, "boxelder")
}
sites <- unique(sites)

if("all" %in% sites){
  sites <- unique(all$site)
}


if(return_all == TRUE){
  return(all)
}

if(start_dt == "start"){
  start_dt = min(all$DT_round)
}else{
  start_dt = ymd_hm(start_dt, tz = "MST")
}
if(end_dt == "end"){
  end_dt = max(all$DT_round)
}else{
  end_dt = ymd_hm(end_dt, tz = "MST")
}




 all %>%
  filter(site %in% sites & parameter %in% parameters & between(DT_round, start_dt, end_dt)& !is.na(value))

}


# example <- grab_raw_data(sites = "all", parameters = "Specific Conductivity", start_dt = "start", end_dt = "end")
#all<- grab_raw_data(return_all = TRUE)
