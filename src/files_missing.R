## Determining uploads

#This function looks at the user inputs for calibration report collect and logs collected.
#Based on these inputs, it looks at all the uploaded logs or calibration reports
#and will print out what logs are missing and who to contact to get those files uploaded.




files_missing <- function(){

  `%nin%` = Negate(`%in%`)
  # #source clean mwater script for all notes cleaned
  #
  # source("src/mWater_collate/load_mWater_notes.R")

  #grab context metadata
  site_meta <- read_csv("data/metadata/water_sampling_sites.csv",show_col_types = FALSE)%>%
    select(site = site_code, Site_Name, site_label_rmrs)
  # sort for sites in upper network (ie. acronyms rather than street names)
  upper_sites <- read_csv("data/metadata/water_sampling_sites.csv",show_col_types = FALSE)%>%
    filter(watershed != "CLP  Mainstem-Fort Collins")%>%
    #this is to help match with user input
    mutate(site_code = tolower(site_code))

  field_season <- year(Sys.Date())

  #grab cal reports from folder
  cal_reports_simple <- str_extract(list.files(path = "data/calibration_reports/"), ".*_\\d{8}" )%>%tolower()
  logs_simple <- str_extract(list.files(path = paste0("data/sensor_data/", field_season), recursive = TRUE), "\\w+_\\d{8}_(vulink|troll)")%>%tolower()




  #grab sensor notes that have logs or cal reports that should be  downloaded
  sensor_files <- all_notes_cleaned%>%
    filter(year(DT_round) == field_season)%>%
    filter(grepl("Sensor",visit_type, ignore.case = TRUE))%>%
    filter(cal_report_collected|log_downloaded)%>%
    select(site, crew, start_DT,end_dt, cal_report_collected, cals_performed, log_downloaded, log1_type,log1_mmdd,  log2_type, log2_mmdd)%>%
    mutate(
      #make all site names lower
      site = tolower(site),
      # Create basis for calibration report name
      # this will be used to check for calibration report in data files and then b
      cal_report_name = case_when(cal_report_collected == TRUE ~ paste0(site, "_", format(start_DT, "%Y%m%d")),
                                  cal_report_collected == NA ~ NA),
      full_cal_name = case_when(cal_report_collected == TRUE ~ paste0(site, "_", format(end_dt, "%Y%m%d_%H%M_mst")),
                                cal_report_collected == NA ~ NA),
      log1_mmdd = case_when(nchar(as.character(log1_mmdd)) == 3 ~ paste0("0",log1_mmdd),
                            TRUE ~ as.character(log1_mmdd)),
      log1_type = case_when( grepl("aquatroll", log1_type,ignore.case = TRUE) ~  "troll",
                             grepl("at", log1_type,ignore.case = TRUE) ~  "troll",
                             grepl("aqua troll", log1_type,ignore.case = TRUE) ~  "troll",
                             grepl("vulink", log1_type,ignore.case = TRUE) ~  "vulink",
                             grepl("vu link", log1_type,ignore.case = TRUE) ~  "vulink",
                             TRUE ~ tolower(log1_type)),
      log2_type = case_when( grepl("aquatroll", log2_type,ignore.case = TRUE) ~  "troll",
                             grepl("at", log2_type,ignore.case = TRUE) ~  "troll",
                             grepl("aqua troll", log2_type,ignore.case = TRUE) ~  "troll",
                             grepl("vulink", log2_type,ignore.case = TRUE) ~  "vulink",
                             grepl("vu link", log2_type,ignore.case = TRUE) ~  "vulink",
                             TRUE ~ tolower(log2_type)),
      log2_mmdd = case_when(nchar(as.character(log2_mmdd)) == 3 ~ paste0("0",log2_mmdd),
                            TRUE ~ as.character(log2_mmdd)),
      log1_user_error = case_when( is.na(log_downloaded)~ FALSE,
                                   log_downloaded == FALSE ~ FALSE,
                                   is.na(log1_mmdd) | is.na(log1_type) ~ TRUE,
                                   TRUE ~ FALSE ),
      log2_user_error = case_when(is.na(log_downloaded)~ FALSE,
                                  log_downloaded == FALSE ~ FALSE,
                                  is.na(log2_mmdd) & is.na(log2_type) ~ FALSE,
                                  is.na(log2_mmdd) | is.na(log2_type) ~ TRUE,
                                  TRUE ~ FALSE),
      log1_name = case_when(!(is.na(log1_mmdd) | is.na(log1_type)) ~ paste0(site,"_",year(start_DT),log1_mmdd,
                                                                            "_", format(start_DT, "%Y%m%d"), "_", log1_type),
                            (is.na(log1_mmdd) | is.na(log1_type))  ~ NA),
      log2_name = case_when(!(is.na(log2_mmdd) | is.na(log2_type)) ~ paste0(site,"_",year(start_DT),log2_mmdd,
                                                                            "_", format(start_DT, "%Y%m%d"), "_", log2_type),
                            (is.na(log2_mmdd) | is.na(log2_type))  ~ NA),
      log_missing1 = case_when(
        is.na(log_downloaded)| log_downloaded == FALSE ~ FALSE,
        is.na(log1_name) ~ FALSE,
        log1_name %nin% logs_simple ~ TRUE,
        TRUE ~ FALSE
      ),
      log_missing2 = case_when(
        is.na(log_downloaded)| log_downloaded == FALSE ~ FALSE,
        is.na(log2_name) ~ FALSE,
        log2_name %nin% logs_simple ~ TRUE,
        TRUE ~ FALSE
      ),
      log_missing = case_when(
        log_missing1 | log_missing2 ~ TRUE,
        TRUE ~ FALSE
      ),
      cal_missing = case_when(
        is.na(cal_report_collected) ~ FALSE,
        cal_report_name %nin% cal_reports_simple ~ TRUE,
        TRUE ~ FALSE)
      )



  for (i in 1:nrow(sensor_files)) {

    #if log missing print out missing logs or cal reports
    if(sensor_files$log_missing[i]){
      cat("\nLog Missing: ", sensor_files$log1_name[i], " and/or ", sensor_files$log2_name[i], "\nContact: ", sensor_files$crew[i], "\n")

    }
    #if log missing print out missing logs or cal reports
    if(sensor_files$cal_missing[i]){
      cat("\nCal Missing: ", sensor_files$full_cal_name[i]," \nContact: ", sensor_files$crew[i], "\n")
    }


  }
  cat("\nFile Check Complete")

}

