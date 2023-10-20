library(tidyverse)
library(lubridate)
library(exiftoolr)
library(jpeg)
install_exiftool()

timelapse_folder_rename <- function(){

  #grab filenames for folder we want to change
  raw_filenames <- list.files("data/timelapse_raw", full.names = T)
  file_list <- list.files(paste0(raw_filenames[1], "/"), full.names = T)
  
  if(is_empty(raw_filenames)){
    print("No Folders to rename")
  }else{
get_site <- function(site_prompt){
  while (TRUE) {
    user_input <- readline(prompt = paste(site_prompt))
    user_input <- tolower(user_input)
    
    sites <- c("tamasag", "legacy", "lincoln", "timberline", "prospect", "boxelder", "archery", "riverbluffs")
    
    if (user_input %in% sites) {
      return(user_input)
    } else {
      cat("Invalid input. Please enter a site in the PWQN.\n")
    }
  }
  
}

prompt_rename <- function(){
  

  # Read the fifth JPEG image (sometimes the first few are for set up)
    img <- readJPEG(file_list[3])
    
    gg_image <- ggplot() +
      annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
    
    # Display the image using ggplot
    print(gg_image)
    # Ask to wait and then pause other text for 10 sec
    print("Please Wait while picture loads")
    Sys.sleep(10) 
  site_choice_prompt <- 'What site does this photo belong to?  '
  site_choice <- get_site(site_choice_prompt)
  return(site_choice)
}

#run prompt rename (with get site nested inside)
#User will be shown photo and asked for site name
site <- prompt_rename()

#grab metadata for first and last site photo in folder

first_metadata <- exif_read(file_list[1])
last_metadata <- exif_read(file_list[length(file_list)])

# Extract start and end dates and format to YYYYMMDD
start <- format(strptime(first_metadata$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S"),"%Y%m%d" )
last <- format(strptime(last_metadata$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S"),"%Y%m%d" )
#Get current folder name
current_folder_name <- raw_filenames[1]
#Create new folder name
folder_rename <- paste0("data/timelapse_photos/", site, "_", start, "_", last)
#Change the folder name and move to timelapse clean
file.rename(current_folder_name, folder_rename)
# Print folder rename to user
cat("\n Folder renamed to ", folder_rename,"\n")
}

}

