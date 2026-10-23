library(tidyverse)
library(fs)

# ---- SETTINGS ---------------------------------------------------------------
src_root <- "timelapse_data"   # original (OneDrive) folder
dst_root <- "tl"                   # destination folder (Symlinked Harddrive)

# For testing: set to a sub folder of timelapse (e.g. "2024_compiled/archery").
# Use "" to run over the whole of timelapse.
sub_folder <- "2025_compiled/elc"

max_retries <- 3        # attempts to download/copy each file
retry_wait  <- 5        # seconds to wait between attempts
# -----------------------------------------------------------------------------

search_dir <- path(src_root, sub_folder)
stopifnot("Search folder does not exist" = dir_exists(search_dir))

# Reading a OneDrive "online-only" placeholder forces it to download.
# We read the whole file in chunks and check that we got every byte.
hydrate_file <- function(file, chunk_size = 64 * 1024^2) {
  expected <- file_info(file)$size
  con <- file(file, "rb")
  on.exit(close(con))

  total <- 0
  repeat {
    bytes <- readBin(con, "raw", n = chunk_size)
    if (length(bytes) == 0) break
    total <- total + length(bytes)
  }

  if (total != expected) stop("Read ", total, " of ", expected, " bytes")
  invisible(TRUE)
}

# Download, copy, then verify the copy is the same size as the original.
copy_one <- function(src, dst) {
  for (attempt in seq_len(max_retries)) {
    ok <- tryCatch({
      hydrate_file(src)
      dir_create(path_dir(dst))
      file_copy(src, dst, overwrite = FALSE)
      if (file_info(src)$size != file_info(dst)$size) stop("Size mismatch after copy")
      TRUE
    }, error = function(e) {
      message("  attempt ", attempt, " failed: ", conditionMessage(e))
      if (file_exists(dst)) file_delete(dst)   # don't leave partial copies behind
      FALSE
    })

    if (ok) return("copied")
    Sys.sleep(retry_wait)
  }
  "failed"
}

# ---- BUILD FILE LIST --------------------------------------------------------
# Could easily map over a list of subfolders, but for now just do one (A ~30 GB folder took ~3 hours)
files <- tibble(src = dir_ls(search_dir, recurse = TRUE, type = "file")) |>
  mutate(
    rel = path_rel(src, start = src_root),   # keeps the same sub folder structure
    dst = path(dst_root, rel),
    exists = file_exists(dst)
  )

message(nrow(files), " files found; ", sum(files$exists), " already exist and will be skipped.")

# ---- COPY -------------------------------------------------------------------
results <- files |>
  mutate(
    status = pmap_chr(list(src, dst, exists), function(src, dst, exists) {
      if (exists) return("skipped")
      message("Copying: ", path_rel(src, start = src_root))
      copy_one(src, dst)
    })
  )

# ---- SUMMARY ----------------------------------------------------------------
results |> count(status)

failed <- results |> filter(status == "failed")
if (nrow(failed) > 0) {
  message("Failed files:")
  print(failed$rel)
}
