clean_directories <- function() {

  pre_dir_path <- pre_verification_path
  int_dir_path <- intermediary_path
  ver_dir_path <- verified_path

  pre_dir_names <- list.files(pre_dir_path)
  int_dir_names <- list.files(int_dir_path)
  ver_dir_names <- list.files(ver_dir_path)

  # Pre-verification Directory
  # keep if not in Verified Directory, else if in Verified Directory delete from this Directory
  for (i in pre_dir_names) {
    if (length(ver_dir_names) != 0 & i %in% ver_dir_names) {
      file.remove(here(pre_dir_path, i))
      cat("removed file ", i, " from ",  pre_dir_path, "\n")
    } else {
      next
    }
  }

  # Intermediary Directory
  # keep if skips in the data OR any(!is_verified), else move to Verified Directory
  for (i in int_dir_names) {
    df <- read_rds(here(int_dir_path, i))
    if (any(df$verification_status == 'SKIP') | any(!df$is_verified)) {
      next
    } else if (all(df$verification_status != 'SKIP') & all(df$is_verified)) {
      source_path <- here(int_dir_path, i)
      destination_path <- here(ver_dir_path, i)
      file.copy(source_path, destination_path, overwrite = TRUE)
      file.remove(source_path)
      cat("moved file ", i, " from ", int_dir_path, "to", ver_dir_path, "\n")
    }
  }

  # Verified Directory
  # final destination for data, check that the data that is here should be here
  for (i in ver_dir_names) {
    df <- read_rds(here(ver_dir_path, i))
    if (any(df$verification_status == 'SKIP') | any(!df$is_verified)) {
      cat(i, "should not be in verified directory\n")
      stop("THERE IS DATA WITH SKIPS OR NON-VERIFIED DATA IN THE VERIFIED DIRECTORY")
    } else {
      cat("data in verified directory is correct\n")
      next
    }
  }
}
