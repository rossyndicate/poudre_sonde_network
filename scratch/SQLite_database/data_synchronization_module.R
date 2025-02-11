# a system to check if files being tracked in the database still match what's on the filesystem

# sync with file system ----
#' @title Sync files with Database
#'
#' @description
#' Takes a database connection `con` as its input and performs a systematic
#' check of all tracked files.
#'
#' @param con A database connection to the SQLite DB.
#'
#' @seealso [resolve_shared_path()]
#' @seealso [handle_missing_file()]
#' @seealso [handle_modified_file()]
#' TODO: make sure that this works how you expect it to.

sync_with_filesystem <- function(con) {

  # Query the database to get information about all tracked files
  ## file_id: unique identifier for the file in the database
  ## relative_path: the file's location relative to some base directory
  ## file_hash: a stored hash value that represents the file's previous content
  ## verification_state: the current verification status of the file

  files <- dbGetQuery(con, "
    SELECT file_id, relative_path, file_hash, verification_state
    FROM verification_status
  ")

  # Check each file
  purrr::walk(seq_len(nrow(files)), ~{
    current_path <- resolve_shared_path(files$relative_path[.x])

    # File existence check
    if (!file.exists(current_path)) {
      handle_missing_file(con, files[.x, ])
      return()
    }

    # Content check: If the file exists, calculate a new hash fo the current file
    # content and compares it with the stored hash
    current_hash <- digest::digest(current_path, file = TRUE)
    if (current_hash != files$file_hash[.x]) {
      handle_modified_file(con, files[.x, ], current_hash)
    }
  })
}

# handle missing file ----
#' @title Handle missing file discrepancy
#'
#' @description
#' Handles cases where a tracked file is no longer found in its expected location.
#'
#' @param con A database connection to the SQLite DB.
#' @param file_record A record of the file's information

# TODO: Make sure that this is working right
#' @title handle missing file
#' this function has to do with the functions that move files around and the function
#' that checks if everything is all right.
handle_missing_file <- function(con, file_record) {

  if (file_record$verification_state == "in_process") {
    # Move to intermediary folder
    old_path <- resolve_shared_path(file_record$relative_path)
    new_relative_path <- gsub("/pre_verification/", "/intermediary/", file_record$relative_path)
    new_path <- resolve_shared_path(new_relative_path)

    if (!dir.exists(dirname(new_path))) {
      dir.create(dirname(new_path), recursive = TRUE)
    }

    file.rename(old_path, new_path)

    # Update database
    dbExecute(con,
              "UPDATE verification_status
       SET relative_path = ?,
           verification_state = 'in_process'
       WHERE file_id = ?",
              params = list(new_relative_path, file_record$file_id)
    )
  } else {
    # Revert to pre-verification status
    dbExecute(con,
              "UPDATE verification_status
       SET verification_state = 'pre',
           user_initials = NULL,
           file_hash = NULL
       WHERE file_id = ?",
              params = list(file_record$file_id)
    )
  }
}

# handle modified file ----
#' @title update intermediary data
#' this function has to do with updating the data that is in the intermediary folder.
#' this function does not have anything to do with moving data across folders.
#' THIS IS THE UPDATE DATA IN INTERMEDIARY FOLDER FUNCTION

handle_modified_file <- function(con, file_record, new_hash) {
  if (file_record$verification_state == "in_process") {
    # File is being actively worked on; move to intermediary folder
    old_path <- resolve_shared_path(file_record$relative_path)
    new_relative_path <- gsub("/pre_verification/", "/intermediary/", file_record$relative_path)
    new_path <- resolve_shared_path(new_relative_path)

    if (!dir.exists(dirname(new_path))) {
      dir.create(dirname(new_path), recursive = TRUE)
    }

    file.rename(old_path, new_path)

    # Update database
    dbExecute(con,
              "UPDATE verification_status
       SET relative_path = ?,
           file_hash = ?,
           verification_state = 'in_process'
       WHERE file_id = ?",
              params = list(new_relative_path, new_hash, file_record$file_id)
    )
  } else {
    # File was modified but not actively being worked on; revert to pre-verification
    dbExecute(con,
              "UPDATE verification_status
       SET file_hash = ?,
           verification_state = 'pre',
           user_initials = NULL
       WHERE file_id = ?",
              params = list(new_hash, file_record$file_id)
    )
  }
}

# metadata upload ----
#' @title Load file metadata into SQLite database

metadata_upload <- function(con, file_path) {

  # Check the SQLite database to make sure that everything is where it should be
  # this will be a `sync_with_filesystem` call.

  # Sync database with file system
  sync_with_filesystem(con)

  # Check if there are new files in the pre verification folder
  # TODO: this is using the shiny object for the file paths, make sure that this is correct

  # Files in pre-verification folder
  pre_verification_files <- list.files(paths$pre_verification_path)

  # Files being tracked in db
  existing_pre_verification_metadata <- dbGetQuery(con, "
                                                   SELECT relative_path
                                                   FROM verification_status")$relative_path

  # Find files not in db
  new_files <- pre_verification_files[!pre_verification_files %in% existing_pre_verification_metadata]

  # If there are new files in the pre verification folder,
  # first: check if those file names are duplicated elsewhere in the system
  # if they are not anywhere in the system: add the new metadata to the database
  # if they are somewhere in the system:
    # in intermediary: do not overwrite, log a warning about the duplicate to the user
    # in verified: do not overwrite, log a warning about the duplicate to the user
    # in pre: compare the file hashes to determine if the content is the same:
      # if the hashes match, ignore the duplicate
      # if the hashes don't match, log a warning about the conflict, and make the user fix the conflict before allowing them to pick that site-parameter.

  # Get all files from pre_verification_directory into the SQLite database

  # Check the pre verification folder in case there are new files that need to be
  # tracked in the database,
  # If there are new files that were added, check if they are in another directory.

  # If everything is good, we can end this function
}

# move data around functions ----
#' @title move to intermediary folder
#' moves data from pre folder to intermediary folder.
move_to_intermediary <- function(con, file_id) {
  file_record <- dbGetQuery(con,
                            "SELECT relative_path FROM verification_status
     WHERE file_id = ?",
                            params = list(file_id)
  )

  if (nrow(file_record) == 0) return(FALSE)

  old_path <- resolve_shared_path(file_record$relative_path)
  new_relative_path <- gsub("/pre_verification/", "/intermediary/", file_record$relative_path)
  new_path <- resolve_shared_path(new_relative_path)

  if (!dir.exists(dirname(new_path))) {
    dir.create(dirname(new_path), recursive = TRUE)
  }

  file.rename(old_path, new_path)

  dbExecute(con,
            "UPDATE verification_status
     SET relative_path = ?,
         verification_state = 'in_process'
     WHERE file_id = ?",
            params = list(new_relative_path, file_id)
  )

  return(TRUE)
}

move_to_verified <- function(con, file_id) {
  # Update DB metadata
  ## in_process to verified
  ## update relative path

  # Update file and file location
  ## is_verified should all be true
  ## move file from int to ver directory
  file_record <- dbGetQuery(con,
                            "SELECT relative_path FROM verification_status
     WHERE file_id = ?",
                            params = list(file_id)
  )
}



# TODO: Integration with Shiny Workflow
