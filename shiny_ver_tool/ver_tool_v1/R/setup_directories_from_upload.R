setup_directories_from_upload <- function(uploaded_file_path, timezone = "MST"){

  #for testing:setwd("~/Documents/fork_yeah/poudre_sonde_network/shiny_ver_tool/ver_tool_v1")

  testing_path = here("shiny_ver_tool", "ver_tool_v1")
  all_path = here("shiny_ver_tool", "ver_tool_v1", "data", "all_data_directory")
  pre_verification_path = here("shiny_ver_tool", "ver_tool_v1","data", "pre_verification_directory")
  intermediary_path = here("shiny_ver_tool", "ver_tool_v1","data", "intermediary_directory")
  verified_path = here("shiny_ver_tool", "ver_tool_v1","data", "verified_directory")
  raw_data_path = here("shiny_ver_tool", "ver_tool_v1","data", "raw_data")

  uploaded_file_type <- tools::file_ext(uploaded_file_path)


  #Check if zip file and unzip to main working directory, this should be a data folder that was previously created by the user
  if(uploaded_file_type == "zip"){
    unzip(uploaded_file_path,exdir = testing_path )
    #Remove unneeded macoxs files
    macosx_path <- list.files(pattern = "MACOSX",full.names = TRUE)
    if (dir.exists(macosx_path) & !is_empty(macosx_path)){
      unlink(macosx_path, recursive = TRUE)  # Remove the folder and its contents
    }
    #Since this was previously created by the user, we can assume that the all_data and raw data folders exist, if they do not, the user will be prompted to create them
    if(!dir.exists(all_path)|length(list.files(all_path)) == 0){
      #check to see if raw_data folder exists
      if(dir.exists(raw_data_path) & length(list.files(raw_data_path)) > 0){
        #change uploaded data & type to data in raw_data folder
        uploaded_file_path <- list.files(raw_data_path, full.names = TRUE)
        #print("Raw Data selected")
      }else{
        return("No data found in all_data_directory and no raw_data folder found")
      }
    }else{
      #TODO: probably not the best way to overwrite things like this?
      return("Data is loaded from zip file and contains all_data_directory data. To add new data, please remove data from all_data_directory and ensure that all orignial data is in raw_data folder")
    }
  }

  #check to see if data folder/sub folders exist
  if(!dir.exists(here("shiny_ver_tool", "ver_tool_v1", "data"))){
    dir.create(here("shiny_ver_tool", "ver_tool_v1","data"))
  }else{
    #check to see if data folder has all_data_directory
    if(!dir.exists(all_path)){
      dir.create(all_path)
    }
    #check to see if data folder has pre_verification_directory
    if(!dir.exists(pre_verification_path)){
      dir.create(pre_verification_path)
    }
    #check to see if data folder has intermediary_directory
    if(!dir.exists(intermediary_path)){
      dir.create(intermediary_path)
    }
    #check to see if data folder has verified_directory
    if(!dir.exists(verified_path)){
      dir.create(verified_path)
    }
    #create raw data folder
    if(!dir.exists(raw_data_path)){
      dir.create(raw_data_path)
    }
    if(!dir.exists(here("shiny_ver_tool", "ver_tool_v1","data", "meta"))){
      dir.create(here("shiny_ver_tool", "ver_tool_v1","data", "meta"))
    }
  }

  #this will read in the file based on its type and check to make sure there is no missing columns
  parse_raw_file <- function(file_path){
    #get type
    file_type <- tools::file_ext(file_path)

    if(file_type == "feather"){
      raw_data <- read_feather(file_path)
    } else if(file_type == "csv"){
      raw_data <- read_csv(file_path)
    } else if(file_type == "rds"){
      raw_data <- read_rds(file_path)
    } else if(file_type == "xlsx"){
      raw_data <- read_xlsx(file_path)
    }
    #check to make sure site, parameter, DT round and mean all exist as columns
    if(!all(c("site", "parameter", "DT_round", "value") %in% colnames(raw_data))){
      #find which column is missing
      missing_cols <- paste(setdiff(c("site", "parameter", "DT_round","value"), colnames(raw_data)), collapse = ", ")
      return(paste0("Columns (", missing_cols, ") must exist in the file: ", file_path))
    }

    raw_data_parsed <- raw_data %>%
      #convert DT_round to date time
      mutate(DT_round = anytime(DT_round, tz = timezone))

    #if there is no flag column, add one
    if(!("flag" %in% colnames(raw_data_parsed))){
      raw_data_parsed$flag = NA
    }

    return(raw_data_parsed)
  }
  #read in all data (either from uploaded file or from raw data folder)
  all_data_parsed <- map_dfr(uploaded_file_path, parse_raw_file)%>%
    bind_rows()
  #Taking all_data_parsed and formatting to save into all and pre directories
  site_params <- expand_grid(site = unique(all_data_parsed$site), parameter = unique(all_data_parsed$parameter))
  #create a list were each is named after the site and parameter combination
  flagged_list <- pmap(site_params, ~ all_data_parsed %>% filter(site == ..1, parameter == ..2))
  rm(all_data_parsed) #save some memory here
  names(flagged_list) <- paste(site_params$site, site_params$parameter, sep = "-")
  #remove empty lists
  flagged_list <- keep(flagged_list, ~nrow(.x) > 0)
  #add additional columns needed
  pre_processed_data <- map(.x = flagged_list, \(x)
                            x %>%
                              mutate(
                                mean = value, # actual data for DS use
                                mean_verified = NA, # if verification status pass, this is mean
                                is_verified = FALSE, # whether or not the data has been verified
                                verification_status = NA, # can only be pass/fail/skip
                                year = year(DT_round),
                                week = week(DT_round),
                                weekday = wday(DT_round, week_start = 7),
                                y_w = paste(year, "-", week),
                                day = yday(DT_round),
                                y_d = paste(year, "-", day),
                                user_flag = flag, # Keep Auto flags in flag column, user added flags/alterations live in user flag
                                brush_omit = FALSE, # User Brush Omit instances, default to FALSE
                                user = NA, #User initials
                                final_status = NA, # Final status of the data point after weekly decision
                                week_decision = NA) #store weekly decision
  )

  rm(flagged_list) #save some memory here

  iwalk(pre_processed_data, \(x, idx) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    data_hash <- digest::digest(x)
    new_filename <- glue("{idx}_{timestamp}_{data_hash}.rds")
    saveRDS(x, here(all_path, new_filename))  # Ensure you use new_filename, not idx
  })

  R.utils::copyDirectory(all_path, pre_verification_path)
  return("Files saved to all and pre directory")
}
