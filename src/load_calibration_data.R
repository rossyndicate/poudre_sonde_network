#' @title Load Calibration Data
#'
#' @description
#' Loads or generates calibration coefficient and drift correction data from
#' HTML calibration files. This function manages calibration data by either
#' loading existing processed data or extracting fresh data from source HTML
#' files. Creates a global calibration_data object organized by year and
#' site-parameter combinations for use throughout the calibration workflow.
#'
#' @param cal_data_file_path Character string specifying the file path for
#'   saved calibration data RDS file (default: munged_calibration_data.RDS
#'   in data/calibration_reports/0_cal_data_munge/)
#' @param update Logical flag indicating whether to regenerate calibration data
#'   from source HTML files (default: FALSE)
#' @param ... Additional arguments passed to underlying calibration extraction
#'   functions
#'
#' @seealso [cal_extract_markup_data()]
#' @seealso [join_sensor_calibration_data()]

load_calibration_data <- function(cal_data_file_path = here("data", "calibration_reports", "0_cal_data_munge", "munged_calibration_data.RDS"),
                                  update = FALSE, ...){

  # Load existing calibration data (typical usage)
  if (file.exists(cal_data_file_path) & !update) {
    message("Calibration data exists! Loading calibration_data...")
    calibration_data <<- read_rds(cal_data_file_path)
    message("The calibration data will not be updated.")
  }

  # Force update of existing calibration data
  if (update) {
    message("Generating the calibration data, this may take a second...")
    calibration_data <<- cal_extract_markup_data()
    write_rds(calibration_data, cal_data_file_path)
    message("The updated calibration data is saved in ", cal_data_file_path)
  }

  # Generate and save calibration data when file doesn't exist and update requested
  if (!file.exists(cal_data_file_path) & update) {
    message("Calibration data does not exist!")
    message("Generating the calibration data, this may take a second...")
    calibration_data <<- cal_extract_markup_data()
    write_rds(calibration_data, cal_data_file_path)
    message("The generated calibration data is saved in ", cal_data_file_path)
  }

  # Generate calibration data without saving when file doesn't exist and no update requested
  if (!file.exists(cal_data_file_path) & !update) {
    message("Calibration data does not exist!")
    message("Generating the calibration data, this may take a second...")
    calibration_data <<- cal_extract_markup_data()
    message("The generated calibration data is not saved in ", cal_data_file_path)
  }
}
