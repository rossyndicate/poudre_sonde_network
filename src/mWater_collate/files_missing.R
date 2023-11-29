## Determining uploads

#This function looks at the user inputs for calibration report collect and logs collected.
#Based on these inputs, it looks at all the uploaded logs or calibration reports
#and will print out what logs are missing and who to contact to get those files uploaded.




files_missing <- function(){

  #source clean mwater script for all notes cleaned

  source("src/mWater_collate/clean_mwater_notes.R")

  #grab cal reports from folder
  cal_reports_simple <- str_extract(list.files(path = "data/calibration_reports/"), ".*_\\d{8}" )
  logs_simple <- str_extract(list.files(path = "data/sensor_data/2023", recursive = TRUE), "\\w+_\\d{8}_(vulink|troll)")

  #grab sensor notes that have logs or cal reports that should be  downloaded
  sensor_files <- all_notes_cleaned%>%
    filter(grepl("Sensor",visit_type, ignore.case = TRUE))%>%
    filter(cal_report_collected|log_downloaded)%>%
    select(site, crew, start_dt_mst, cal_report_collected, cals_performed, log_downloaded, log1_type,log1_mmdd,  log2_type, log2_mmdd)%>%
    mutate(# Create basis for calibration report name
      # this will be used to check for calibration report in data files and then b
      cal_report_name = case_when(cal_report_collected == TRUE ~ paste0(tolower(site), "_", format(start_dt_mst, "%Y%m%d")),
                                  cal_report_collected == NA ~ NA),
      log1_mmdd = case_when(nchar(as.character(log1_mmdd)) == 3 ~ paste0("0",log1_mmdd),
                            TRUE ~ as.character(log1_mmdd)),
      log1_type = tolower(log1_type),
      log2_type = tolower(log2_type),
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
      log1_name = case_when(!(is.na(log1_mmdd) | is.na(log1_type)) ~ paste0(site,"_",year(start_dt_mst),log1_mmdd,
                                                                            "_", format(start_dt_mst, "%Y%m%d"), "_", log1_type),
                            (is.na(log1_mmdd) | is.na(log1_type))  ~ NA),
      log2_name = case_when(!(is.na(log2_mmdd) | is.na(log2_type)) ~ paste0(site,"_",year(start_dt_mst),log2_mmdd,
                                                                            "_", format(start_dt_mst, "%Y%m%d"), "_", log2_type),
                            (is.na(log2_mmdd) | is.na(log2_type))  ~ NA),
      log_missing = case_when(
        log1_name %nin% logs_simple | log2_name %nin% logs_simple ~ TRUE,
        TRUE ~ FALSE
      ),
      cal_missing = case_when(
        cal_report_name %nin% cal_reports_simple ~ TRUE,
        TRUE ~ FALSE
      )
    )


  for (i in 1:nrow(sensor_files)) {

    #if log missing print out missing logs or cal reports
    if(sensor_files$log_missing[i]){
      cat("\nLog Missing: ", sensor_files$log1_name[i], " or ", sensor_files$log2_name[i], "\nContact: ", sensor_files$crew[i], "\n")

    }
    #if log missing print out missing logs or cal reports
    if(sensor_files$cal_missing[i]){
      cat("\nCal Missing: ", sensor_files$cal_report_name[i]," \nContact: ", sensor_files$crew[i], "\n")
    }


  }
  cat("\nFile Check Complete")

}
