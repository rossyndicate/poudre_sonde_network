# @ param site: string, name of site to get filenames for, select from "tamasag" "legacy", "timberline" "prospect" "boxelder" "archery" "riverbluffs"
get_tl_photo_filenames <- function(site = "legacy",start_dt = "2023-05-20 12:00" ,end_dt = "2023-07-20 12:00"){
  #convert dates to DT objects
  start_dt <- ymd_hm(start_dt, tz = "MST")
  end_dt <- ymd_hm(end_dt, tz = "MST")
  if(site == "river bluffs"){
    site <- "riverbluffs"
  }

  if(site %nin% c("tamasag", "legacy", "timberline", "prospect", "boxelder", "archery", "riverbluffs")){
    print("Invalid site name, please select from: 'tamasag' 'legacy' 'timberline' 'prospect' 'boxelder' 'archery' 'riverbluffs'")

  nah <- tibble()
    return(nah)
  }

  #look through all the files in the site's folder
  all_files <- tibble(filename = list.files(paste0("data/timelapse_photos/2023_compiled/",site), full.names = TRUE, recursive = TRUE))%>%
    mutate(DT = str_extract(filename, "[0-9]{8}_[0-9]{4}"),
           DT = parse_date_time(DT, "Ymd_HM", tz = "MST"),
           #round DT to 30 min to match with sensor data
           DT_round = round_date(DT, "30 minutes"))%>%
    # if there are multiple photos with the same DT_round value, only keep one row with that DT_round
    distinct(DT_round, .keep_all = TRUE)%>%
    select(-DT)%>%
    filter(between(DT_round, start_dt, end_dt))

  if(nrow(all_files) == 0){
    print("No files found for this site and date range")
    nah <- tibble()
    return(nah)
  }else{
   return(all_files)
  }


}

#test <- get_tl_photo_filenames(site = "legacy", start_dt = "2023-7-20 12:00", end_dt = "2023-12-06 12:00")




