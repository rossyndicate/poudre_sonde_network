
#@param folder_name: input the raw photo folder that is named the site you wish to compile
compile_files <- function(folder_name){
  site <- str_extract(folder_name, "tamasag|legacy|timberline|prospect|boxelder|archery|riverbluffs")
  photo_files <- list.files(path = folder_name, full.names = TRUE, recursive = TRUE)
  #files in folder
  new_folder_files <- list.files(path = paste0('data/timelapse_photos/2023_compiled/',site), full.names = TRUE)

  photo_renamer <- function(file) {
    #grab dt from file
    dt <- read_exif(path = file,
                    tags = c("DateTimeOriginal")) %>%
      pull(DateTimeOriginal) %>%
      parse_date_time("YmdHMS", tz="MST")%>%
      format("%Y%m%d_%H%M")
    #create new file name from dt
    new_file_name <- paste0('data/timelapse_photos/2023_compiled/',site,"/", dt,'.JPG')

    #check to see if this file is already in the folder
    if(new_file_name %nin% new_folder_files){
      #if it is not, copy it over
      file.copy(file,to =  new_file_name)
    }

  }
  map(photo_files, photo_renamer)
  print(paste0("Finished ", folder_name))

}
