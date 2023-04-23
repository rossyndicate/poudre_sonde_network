# library(exifr)
# library(tidyverse)
# library(lubridate)
# library(fs)
#
# # List the jpg files in the folder
#
# old_files <- list.files("data/timelapse_photos/new_blackout", full.names = TRUE)
# old_files
#
# # Create vector of new files
#
# new_files <- paste0("data/timelapse_photos/blackout/",1:length(old_files),".JPG")
# new_files
#
# # Copy from old files to new files
#
# file.copy(from = old_files, to = new_files)
#
# ############################
#
# # reading in all the different pictures we need to rename
# files <- list.files("data/timelapse_photos/timberline", full.names=TRUE)
#
# photo_renamer <- function(files) {
#   dt <- read_exif(path = files,
#             tags = c("DateTimeOriginal")) %>%
#     pull(DateTimeOriginal) %>%
#     parse_date_time("YmdHMS", tz="MST")
#
#   correct_dt <- (round_date(ymd_hms(dt, tz = 'us/mountain'), '1 hour')) %>%
#     str_replace_all(., c("\\:" = "")) %>%
#     str_replace_all(., c("\\ " = "_"))
#
#   file.rename(files, paste0('data/timelapse_photos/timberline_all/timberline_',correct_dt,'.JPG'))
# }
#
# map(files,photo_renamer)
#
# #### THIS IS FOR WHEN THE ACTUAL DT METADATA IS WRONG::
#
# exif_renamer <- function(files) {
#   hrs <- hours(2868) # <- HOUR OFFSET FOR IT TO BE CORRECT
#   dt <- read_exif(path = files,
#                   tags = c("DateTimeOriginal")) %>%
#     pull(DateTimeOriginal) %>%
#     parse_date_time("YmdHMS", tz="MST")
#   correct_dt <- (dt + hrs) %>% # PLUS OR MINUS DEPENDING ON NECESSARY OFFSET
#     str_replace_all(., c("\\:" = "")) %>%
#     str_replace_all(., c("\\ " = "_"))
#   file.rename(files, paste0('NewFiles/',correct_dt,'.JPG'))
# }
#
# map(files,exif_renamer)
#
# ##### THIS IS FOR WHEN YOU NEED TO ADD BLACK IMAGES BETWEEN PHOTO GAPS #####
#
# blackout_files <- list.files("data/timelapse_photos/blackout", full.names=TRUE)
#
# right_dates <- blackout_filter %>% #test %>%
#   mutate(FileName = paste0(1:nrow(.),".JPG"))
#
# blackout_renamer <- function(blackout_files) {
#
#   dt <- read_exif(path = blackout_files)
#
#   correct_dt <- right_dates %>%
#     inner_join(dt, by="FileName") %>%
#     pull(dt) %>%
#     str_replace_all(., c("\\:" = "")) %>%
#     str_replace_all(., c("\\ " = "_"))
#
#   file.rename(blackout_files, paste0('data/timelapse_photos/new_blackout/timberline_',correct_dt,'.JPG'))}
#
# map(blackout_files, blackout_renamer)
#
#
