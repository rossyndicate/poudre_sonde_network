#' Restart Post-Verification for a Specific Site and Parameter
#'
#' @description
#' This function resets the manual data verification process for a specific site,
#' parameter, and year. It removes the associated post-verification parquet file
#' and clears the corresponding entries from the central drift corrections Excel sheet.
#'
#' @param site_oi Character. The site identifier of interest (e.g., "salyer").
#' @param parameter_oi Character. The parameter identifier of interest (e.g., "Turbidity").
#' @param year Numeric or Character. The data cycle year (e.g., 2020).
#'
#' @return Invisible \code{NULL}. The function is called for its side effects (deleting files and modifying Excel sheets).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Locates the post-verification `.parquet` file matching the site and parameter.
#'   \item If found, it looks for a central `drift_corrections.xlsx` file.
#'   \item It filters **out** the target site-parameter rows from the Excel sheet and overwrites it.
#'   \item It deletes the target `.parquet` file from the disk.
#' }
#'

#' @examples
#' \dontrun{
#' restart_site_parameter(site_oi = "salyer", parameter_oi = "Turbidity", year = 2020)
#' }
#' @export
restart_site_parameter <- function(site_oi, parameter_oi, year){
  #find the post verification file for the site-param-year combo
  post_ver_file <- list.files(here::here("data/raw/sensor/manual_data_verification", paste0(year, "_cycle"), "post_verification"),
                              pattern = paste0(site_oi, "-", parameter_oi, ".*\\.parquet$"), ignore.case = TRUE, full.names = TRUE)
  if(length(post_ver_file) == 0) {
    message(paste("No existing post-verification file found for", site_oi,"-", parameter_oi))
  } else {

    corrections_sheet <- here::here("data/raw/sensor/manual_data_verification", paste0(year, "_cycle"), "post_verification", "drift_corrections.xlsx")
    if(file.exists(corrections_sheet)) {
      #remove that site-param from the corrections sheet
      readxl::read_xlsx(corrections_sheet) %>%
        dplyr::mutate(site_param = paste(site, parameter, sep = "-")) %>%
        dplyr::filter(site_param != paste(site_oi, parameter_oi, sep = "-")) %>%
        dplyr::select(-site_param) %>%
        openxlsx::write.xlsx(corrections_sheet, overwrite = TRUE)
      #delete file from post_verification folder
      file.remove(post_ver_file)
      message(paste("Removed existing post-verification file for", site_oi,"-", parameter_oi))

    }else{
      message("Corrections file does not exist. No changes made to corrections sheet.")
    }

  }

}
