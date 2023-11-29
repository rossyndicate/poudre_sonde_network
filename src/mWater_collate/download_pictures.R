## Photos

# Goals:
# Download all user created photos ( upstream, downstream, clarity, filter and other pictures)
# Label according to site, date, description in the format site_YYYYMMDD_descriptor.jpg
# Only download photos which have not yet been downloaded




download_pictures <- function(){
  #source to grab all notes cleaned
  source("src/mWater_collate/clean_mwater_notes.R")
  # Find all the downloaded pictures
  all_file_names <- list.files(path = "data/field_pics/", recursive = TRUE)
  #grab notes
  sampling_photos <- all_notes_cleaned%>%
    #grab needed columns
    select(site, start_dt,photos_downloaded,upstream_pic,downstream_pic,clarity,filter_pic,other_pic,other_pic_descriptor)%>%
    mutate(
      #Date format for pictures
      yyyymmdd = format(start_dt, "%Y%m%d"),
      #create filenames ONLY if there is a URL associated with the site visit
      upstream_filename = case_when(
        !is.na(upstream_pic) ~ paste0(site, "_", yyyymmdd, "_upstream.jpg"),
        TRUE ~ NA_character_
      ),
      downstream_filename = case_when(
        !is.na(downstream_pic) ~ paste0(site, "_", yyyymmdd, "_downstream.jpg"),
        TRUE ~ NA_character_
      ),
      clarity_filename = case_when(
        !is.na(clarity) ~ paste0(site, "_", yyyymmdd, "_clarity.jpg"),
        TRUE ~ NA_character_
      ),
      filter_filename = case_when(
        !is.na(filter_pic) ~ paste0(site, "_", yyyymmdd, "_filter.jpg"),
        TRUE ~ NA_character_
      ),
      # check to see if the photo is already downloaded to folder
      upstream_downloaded = case_when(
        is.na(upstream_filename) ~ NA,
        upstream_filename %in% all_file_names ~ TRUE,
        TRUE ~ FALSE
      ),
      clarity_downloaded = case_when(
        is.na(clarity_filename) ~ NA,
        clarity_filename %in% all_file_names ~ TRUE,
        TRUE ~ FALSE
      ),
      filter_downloaded = case_when(
        is.na(filter_filename) ~ NA,
        filter_filename %in% all_file_names ~ TRUE,
        TRUE ~ FALSE
      ),
      downstream_downloaded = case_when(
        is.na(downstream_filename) ~ NA,
        downstream_filename %in% all_file_names ~ TRUE,
        TRUE ~ FALSE
      )
    )
# basic path to field pics
path <- "data/field_pics/"


  # loop thru dataset and download the photo ONLY if it is not yet downloaded and not NA
  for (i in 1:nrow(sampling_photos)) {
    if (!is.na(sampling_photos$upstream_downloaded[i]) && !sampling_photos$upstream_downloaded[i]) {
      download.file(sampling_photos$upstream_pic[i], destfile = paste0(path,sampling_photos$upstream_filename[i]))
    }

    if (!is.na(sampling_photos$downstream_downloaded[i]) && !sampling_photos$downstream_downloaded[i]) {
      download.file(sampling_photos$downstream_pic[i], destfile = paste0(path, sampling_photos$downstream_filename[i]))
    }
    if (!is.na(sampling_photos$clarity_downloaded[i]) && !sampling_photos$clarity_downloaded[i]) {
      download.file(sampling_photos$clarity[i], destfile = paste0(path, sampling_photos$clarity_filename[i]))
    }
    if (!is.na(sampling_photos$filter_downloaded[i]) && !sampling_photos$filter_downloaded[i]) {
      download.file(sampling_photos$filter_pic[i], destfile = paste0(path, sampling_photos$filter_filename[i]))
    }
  }

  #grab notes for sites with other pictures
  other_photos <- all_notes_cleaned%>%
    #grab needed columns
    select(site, start_dt,other_pic,other_pic_descriptor)%>%
    #get rid of instances with no other pics
    filter(!is.na(other_pic))%>%
    mutate(
      #Date format for pictures
      yyyymmdd = format(start_dt, "%Y%m%d"),
      # separate multiple URLs in other pic column
      other_pic_sep = str_split(other_pic, "; "),
      #seperate multiple descriptors in the descriptor column
      other_descriptor_sep = str_split(other_pic_descriptor, ","))%>%
    #for rows with multiple pictures, create a new row for each picture
    unnest(cols = c(other_pic_sep, other_descriptor_sep))%>%
    #remove excess columns and rename sep columns to match old columns
    select(site, start_dt,yyyymmdd, other_pic = other_pic_sep, other_pic_descriptor = other_descriptor_sep)%>%
    # make descriptor lower case and remove any spaces in the name
    mutate(other_pic_descriptor = tolower(str_replace_all(other_pic_descriptor, " ", "")),
           other_filename = case_when(!is.na(other_pic) ~ paste0(site, "_", yyyymmdd, "_", other_pic_descriptor, ".jpg")),
           # Check to see if photo has already been downloaded
           other_downloaded = case_when(
             is.na(other_filename) ~ NA,
             other_filename %in% all_file_names ~ TRUE,
             TRUE ~ FALSE
           ))

  # loop thru dataset and download the photo ONLY if it is not yet downloaded and not NA
  for (i in 1:nrow(other_photos)) {
    if (!is.na(other_photos$other_downloaded[i]) && !other_photos$other_downloaded[i]) {
      download.file(other_photos$other_pic[i], destfile = paste0(path,other_photos$other_filename[i]))
    }}

cat("\nAll Available Pictures Downloaded\n")
}

download_pictures()
#RUN TO DOWNLOAD NEW PICTURES
# It takes about 2-5 minutes to download ~25-50 photos
# Sometimes the request to mWater time out, just re run the function below if that happens
