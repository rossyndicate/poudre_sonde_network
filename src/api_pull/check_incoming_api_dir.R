check_incoming_api_dir <- function(incoming_dir, archive_dir) {
  # Check if data/api/incoming_api_data exists
  if (dir.exists(incoming_dir)) {
    # Check if incoming directory is empty
    if (length(list.files(incoming_dir)) == 0) {
      print(paste0(incoming_dir, " exists and is empty."))
      print("Incoming API data directory is configured properly.")
    } else {
      print(paste0(incoming_dir, " exists but is not empty..."))
      print("Please ensure previous workflow ran properly...")
      stop("Pipeline halted due to non-empty incoming directory.")
    }
  } else {
    print(paste0(incoming_dir, " does not exist..."))
    print("Creating incoming directory...")
    dir.create(incoming_dir, recursive = TRUE)
    print("Incoming directory created.")
  }

  # Check if data/api/archive_api_data exists
  if (dir.exists(archive_dir)) {
    print(paste0(archive_dir, " exists."))
    print("Directory is configured properly.")
  } else {
    print(paste0(archive_dir, " does not exist..."))
    print("Creating archive directory...")
    dir.create(archive_dir, recursive = TRUE)
    print("Archive directory created.")
  }
}
