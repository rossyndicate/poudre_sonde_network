append_historical_api_data <- function(hist_dir, inc_dir) {

  # list of site names
  site_names <- list(
    "Archery",
    "Boxelder",
    "Legacy",
    "Lincoln",
    "Prospect",
    "River Bluffs",
    "Tamasag",
    "Timberline"
  )

  # get the full file names
  inc_full_file_names <- list.files(inc_dir, pattern = "*.csv", full.names = TRUE)
  hist_full_file_names <- list.files(hist_dir, pattern = "*.csv", full.names = TRUE)

  # find the files that match using the site names
  walk(site_names, function(site_name) {

    # find the index of the matching site names in the file names
    inc_site_name_full_path <- grepl(site_name, inc_full_file_names, ignore.case = TRUE)
    hist_site_name_full_path <- grepl(site_name, hist_full_file_names, ignore.case = TRUE)

    # get the file names
    inc_site_path <- inc_full_file_names[inc_site_name_full_path]
    hist_site_path <- hist_full_file_names[hist_site_name_full_path]

    # read in the files
    inc_file <- read_csv(inc_site_path)
    hist_file <- read_csv(hist_site_path)

    # combine the files
    new_file <- bind_rows(hist_file, inc_file) %>%
      distinct()

    # write the file (this will replace the old file)
    write_csv(new_file, paste0(hist_dir, site_name, "_historical.csv"))
  })

  # delete the files in the inc_dir
  walk(inc_full_file_names, unlink)

}

