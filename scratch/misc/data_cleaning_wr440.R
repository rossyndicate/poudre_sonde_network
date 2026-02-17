

files <- list.files("data/raw/sensor/manual_data_verification/",
                    pattern = "DO_|Temperature_",
                    recursive = TRUE,
                    full.names = TRUE) %>%
  grep("chd|pbd|udall|elc", ., value = TRUE, ignore.case = TRUE)%>%
  grep("intermediary|verified", ., value = TRUE, ignore.case = TRUE)


data_2024_2025 <- map_dfr(files, read_parquet)%>%
  select(DT_round, DT_join, site, parameter, raw_value = mean, units, flag = user_flag, approval_status = final_status)%>%
  mutate(cleaned_value = if_else(approval_status == "OMIT", NA_real_, raw_value))

units_lookup <- distinct(data_2024_2025, parameter, units)%>%
  filter(!is.na(units))

files <- list.files("data/raw/sensor/manual_data_verification/2023_cycle/in_progress/post_verified_directory",
                    pattern = "-DO|-Temperature",
                    recursive = TRUE,
                    full.names = TRUE) %>%
  grep("chd|pbd|lincoln|boxelder", ., value = TRUE, ignore.case = TRUE)

files_remain <- list.files("data/raw/sensor/manual_data_verification/2023_cycle/in_progress/verified_directory",
                    pattern = "-DO|-Temperature",
                    recursive = TRUE,
                    full.names = TRUE) %>%
  grep("chd|pbd", ., value = TRUE, ignore.case = TRUE)

data_2023 <- map_dfr(c(files, files_remain), read_rds)%>%
  filter(year(DT_round)< 2024)%>%
  mutate(approval_status = case_when(
    is.na(flag) & verification_status == "PASS" ~ "PASS",
    is.na(flag) & verification_status == "FAIL" ~ "OMIT",
    !is.na(flag) & verification_status == "PASS" ~ "OMIT",
    !is.na(flag) & verification_status == "FAIL" ~ "OMIT",
  ))%>%
  select(DT_round, DT_join, site, parameter, raw_value = mean, approval_status)%>%
  mutate(cleaned_value = if_else(approval_status == "OMIT", NA_real_, raw_value))



final_data <- bind_rows(data_2024_2025%>%select(-units), data_2023)%>%
  left_join(units_lookup, by = "parameter")%>%
  select(DT_round, DT_join, site, parameter, raw_value, cleaned_value, units, approval_status)%>%
  ross.wq.tools::fix_site_names()



ggplot(final_data%>%filter(parameter == "DO"), aes(x = DT_round, y = raw_value, color = approval_status))+
  geom_point()+
  facet_wrap(~site)


final_data %>%
  group_by(site) %>%
  group_walk(~ {
    # .y contains the grouping key (the site name)
    file_path <- paste0("docs/pwqn/shared/wr440_marguerite/", .y$site, "_DO_Temperature_data.rds")
    write_rds(.x, file_path)
  })
