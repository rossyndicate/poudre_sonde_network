text <- unique(all_data_flagged$`archery-Actual Conductivity`$flag)
!str_detect(x, "^(missing data|sonde not employed;\\nmissing data)$")

print(unique(all_data_flagged$`archery-Actual Conductivity`$flag)[grepl("outside of seasonal range", unique(all_data_flagged$`archery-Actual Conductivity`$flag))])

# lumps all instances where seasonal range flag gets added
all_data_flagged_bound <- all_data_flagged %>% bind_rows
flag_percent <- all_data_flagged_bound %>%
  group_by(flag) %>%
  summarize(count= n())


