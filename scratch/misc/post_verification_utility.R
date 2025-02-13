# data that still needs to get added
final_data <- paste(c("timberline-Temperature", "timberline-pH", "timberline-DO"), collapse = "|")

verified_file_names <- tibble(names = list.files(path = here('data', 'virridy_verification', 'verified_directory'))) %>%
  filter(grepl(site_list, names, ignore.case = T),
         !grepl("virridy", names, ignore.case = T)) %>%
  arrange() %>%
  pull(names)

verified_data <- map(.x = verified_file_names,~{
  read_rds(here('data', 'virridy_verification', 'verified_directory', .x))
})

names(verified_data) <- verified_file_names

last_verified_data <- verified_data[grepl(final_data, names(verified_data))]

iwalk(last_verified_data, ~{
  write_rds(.x, here("data", "virridy_verification", "post_verified_directory", .y))
})
