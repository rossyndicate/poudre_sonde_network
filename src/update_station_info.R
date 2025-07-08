
#' @title Track Sensor Deployments
#'
#' @description This function tracks sensor deployments based on field notes, filtering for the current year and for notes with sensor swaps/deployments
#' It processes the data to identify current and archived sensor numbers for each site.
#' @param field_notes A dataframe containing field notes with columns: site, start_dt, visit_type, sensor_change, sn_removed, sn_deployed, visit_comments.
#' @param sonde_tracking_file_path A string filepath to the current file where sonde info is tracked. This is an .xslx file with the sheet "station_info" where we will update the sonde numbers,
#' and the sheet "ownership" where we will look for sensors that are not to be included in the current sensor deployments.
#' @return A dataframe with columns: site, current_sn, archive_sn, archive_dates, showing the current and archived sensor numbers along with their deployment dates.
#'
update_station_info <- function(field_notes, sonde_tracking_file_path) {

  # Get current field season (year)
  current_year <- year(Sys.Date())

  #look up the vulinks and remove field notes that deal with vulinks
  # read in sensor ownership sheet (contains all SNs and type of equipment)
  vulinks <- readxl::read_xlsx(here(sonde_tracking_file_path), sheet = "ownership")%>%
    #remove sensors given back to CU and YSI chla sensors
    filter(grepl("vulink",Equipment, ignore.case = T))%>%
    mutate(SN = as.numeric(SN))%>%
    pull(SN)

  # Filter to current year
  field_notes_current <- field_notes %>%
    #general filtering, prior to 2025 this was not how this was used
    filter(year(start_dt)>=2025 & grepl("Sensor Calibration, Cleaning or Check", visit_type)& !is.na(sensor_change),
           # specific filtering to this year
           year(start_dt) == current_year)%>%
    select(site, start_dt, visit_type, sensor_change, sn_removed, sn_deployed, visit_comments)%>%
    arrange(site, start_dt)%>%
    mutate(sn_removed = as.numeric(sn_removed),
           sn_deployed = as.numeric(sn_deployed)) %>%
    #make sure these are not accidentally vulinks
    filter(!sn_deployed %in% vulinks & !sn_removed %in% vulinks)

  # Get unique sites
  sites <- unique(field_notes_current$site)

  # Initialize result dataframe
  result_field_notes <- tibble(
    site = character(),
    current_sn = character(),
    archive_sn = character(),
    archive_dates = character()
  )

  # Process each site
  for (site_name in sites) {

    site_data <- field_notes_current %>%
      filter(site == site_name) %>%
      arrange(start_dt)

    # Find original deployed SN (first deployment record for the site)
    original_deployment <- site_data %>%
      filter(!is.na(sn_deployed)) %>%
      slice(1)

    if (nrow(original_deployment) == 0) {
      cat("Warning: No deployed SN found for site", site_name, "\n")
      next
    }

    current_sn <- as.character(original_deployment$sn_deployed)
    archive_sns <- character()
    archive_dates <- character()

    # Find all sensor changes (swaps) for this site
    swaps <- site_data %>%
      filter(sensor_change == "Swapped"&
               #check that sn_deployed and sn_removed are not both NA
               !(is.na(sn_deployed) & is.na(sn_removed)) &
               #check that sn_deployed and sn_removed are not the same (ie no swap actually occured)
               sn_deployed != sn_removed ) %>%
      arrange(start_dt)

    if (nrow(swaps) > 0) {

      for (i in 1:nrow(swaps)) {
        swap_record <- swaps[i, ]

        # Check if removed SN matches current deployed SN
        if (!is.na(swap_record$sn_removed)) {
          if (swap_record$sn_removed != current_sn) {
            stop(paste("ERROR: Mismatch in sensor swap for site", site_name,
                       "on", as_date(swap_record$start_dt),
                       "\nRemoved SN:", swap_record$sn_removed,
                       "does not match current deployed SN:", current_sn,
                       "\nPlease check field notes and data entry."))
          }

          # Move current SN to archive
          archive_sns <- c(archive_sns, current_sn)
          archive_dates <- c(archive_dates, as.character(as_date(swap_record$start_dt)))

          # Update current SN if new one was deployed
          if (!is.na(swap_record$sn_deployed)) {
            current_sn <- as.character(swap_record$sn_deployed)
          }
        } else if (!is.na(swap_record$sn_deployed)) {
          # If only deployment info (no removal info), assume it's replacing current
          archive_sns <- c(archive_sns, current_sn)
          archive_dates <- c(archive_dates, as.character(swap_record$start_dt))
          current_sn <- as.character(swap_record$sn_deployed)
          # cat("    Swap", i, ": Archived SN", archive_sns[length(archive_sns)],
          #     ", Deployed SN", current_sn, "on", as.character(swap_record$start_dt), "\n")
        }
      }
    }

    # Create result row
    result_row <- tibble(
      site = site_name,
      current_sn = current_sn,
      archive_sn = if (length(archive_sns) > 0) paste(archive_sns, collapse = ", ") else NA_character_,
      archive_dates = if (length(archive_dates) > 0) paste(archive_dates, collapse = ", ") else NA_character_
    )

    result_field_notes <- bind_rows(result_field_notes, result_row)
  }
#renaming columns
  sonde_field_notes <- result_field_notes %>%
    select(site_code = site,
           current_sonde_sn = current_sn,
           sonde_archive = archive_sn,
           sonde_swap_date = archive_dates
    )


#repeat for vulinks



  # Filter to current year
  vulink_swaps <- field_notes %>%
    #general filtering, prior to 2025 this was not how this was used
    filter(year(start_dt)>=2025 & grepl("Sensor Calibration, Cleaning or Check", visit_type)& !is.na(sensor_change),
           # specific filtering to this year
           year(start_dt) == current_year)%>%
    select(site, start_dt, visit_type, sensor_change, sn_removed_vulink, sn_deployed_vulink, visit_comments)%>%
    arrange(site, start_dt)%>%
    mutate(sn_removed_vulink = as.numeric(sn_removed_vulink),
           sn_deployed_vulink = as.numeric(sn_deployed_vulink)) %>%
    filter(sn_deployed_vulink %in% vulinks|sn_removed_vulink %in% vulinks)

  # Get unique sites
  sites <- unique(field_notes_current$site)

  # Initialize result dataframe
  result_field_notes_vulink <- tibble(
    site = character(),
    current_sn = character(),
    archive_sn = character(),
    archive_dates = character()
  )

  # Process each site
  for (site_name in sites) {

    site_data <- vulink_swaps %>%
      filter(site == site_name) %>%
      arrange(start_dt)

    # Find original deployed SN (first deployment record for the site)
    original_deployment <- site_data %>%
      filter(!is.na(sn_deployed_vulink)) %>%
      slice(1)

    if (nrow(original_deployment) == 0) {
      cat("Warning: No deployed SN found for site", site_name, "\n")
      next
    }

    current_sn <- as.character(original_deployment$sn_deployed_vulink)
    archive_sns <- character()
    archive_dates <- character()

    # Find all sensor changes (swaps) for this site
    swaps <- site_data %>%
      filter(sensor_change == "Swapped" &
               #check that sn_deployed_vulink and sn_removed_vulink are not both NA
               !(is.na(sn_deployed_vulink) & is.na(sn_removed_vulink)) &
               #check that sn_deployed_vulink and sn_removed_vulink are not the same (ie no swap actually occured)
                   sn_deployed_vulink != sn_removed_vulink ) %>%
      arrange(start_dt)

    if (nrow(swaps) > 0) {

      for (i in 1:nrow(swaps)) {
        swap_record <- swaps[i, ]

        # Check if removed SN matches current deployed SN
        if (!is.na(swap_record$sn_removed_vulink)) {
          if (swap_record$sn_removed_vulink != current_sn) {
            stop(paste("ERROR: Mismatch in vulink swap for site", site_name,
                       "on", as_date(swap_record$start_dt),
                       "\nRemoved SN:", swap_record$sn_removed_vulink,
                       "does not match current deployed SN:", current_sn,
                       "\nPlease check field notes and data entry."))
          }

          # Move current SN to archive
          archive_sns <- c(archive_sns, current_sn)
          archive_dates <- c(archive_dates, as.character(as_date(swap_record$start_dt)))

          # Update current SN if new one was deployed
          if (!is.na(swap_record$sn_deployed_vulink)) {
            current_sn <- as.character(swap_record$sn_deployed_vulink)
          }
        } else if (!is.na(swap_record$sn_deployed_vulink)) {
          # If only deployment info (no removal info), assume it's replacing current
          archive_sns <- c(archive_sns, current_sn)
          archive_dates <- c(archive_dates, as.character(swap_record$start_dt))
          current_sn <- as.character(swap_record$sn_deployed_vulink)
          # cat("    Swap", i, ": Archived SN", archive_sns[length(archive_sns)],
          #     ", Deployed SN", current_sn, "on", as.character(swap_record$start_dt), "\n")
        }
      }
    }

    # Create result row
    result_row <- tibble(
      site = site_name,
      current_sn = current_sn,
      archive_sn = if (length(archive_sns) > 0) paste(archive_sns, collapse = ", ") else NA_character_,
      archive_dates = if (length(archive_dates) > 0) paste(archive_dates, collapse = ", ") else NA_character_
    )

    result_field_notes_vulink <- bind_rows(result_field_notes_vulink, result_row)
  }

    vulink_field_notes <- result_field_notes_vulink %>%
    select(site_code = site,
           current_vulink_sn = current_sn,
           vulink_archive = archive_sn,
           vulink_swap_date = archive_dates
    )


  #read in the station info sheet
  new_station_info <- readxl::read_xlsx(here(sonde_tracking_file_path), sheet = "station_info")%>%
    mutate(site_code = tolower(site_code))%>%
    select(site_code,cable_length_needed, add_equipment, planned_sonde, sonde_owner)%>%
    left_join(sonde_field_notes, by = "site_code")%>% # add in updated sonde field notes
    left_join(vulink_field_notes, by = "site_code") # add in updated vulink field notes


  # Load the workbook
  wb <- loadWorkbook(here(sonde_tracking_file_path))

  # Remove old 'station_info' sheet if it exists
  if ("station_info" %in% names(wb)) {
    removeWorksheet(wb, "station_info")
  }

  # Add updated 'sensor_cur_loc' data
  addWorksheet(wb, "station_info")
  writeData(wb, sheet = "station_info", x = new_station_info)

  # Save workbook (overwrites file)
  saveWorkbook(wb, file = sonde_tracking_file_path, overwrite = TRUE)

}

