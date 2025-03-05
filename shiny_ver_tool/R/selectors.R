#copied over from `source`, select site for sub plots
relevant_sonde_selector <- function(site_arg) {
  if (site_arg == "joei") {
    plot_filter <- c("cbri")
  }
  if (site_arg == "cbri") {
    plot_filter <- c("joei", "chd")
  }
  if (site_arg == "chd") {
    plot_filter <- c("cbri", "pfal")
  }
  if (site_arg == "pfal") {
    plot_filter <- c("chd", "sfm")
  }
  if (site_arg == "sfm") {
    plot_filter <- c("pfal", "pbd")
  }
  if (site_arg == "penn") {
    plot_filter <- c("sfm")
  }
  if (site_arg == "lbea") {
    plot_filter <- c("sfm")
  }
  if (site_arg == "pbd") {
    plot_filter <- c("sfm", "tamasag")
  }
  if (site_arg == "tamasag") {
    plot_filter <- c("pbd", "legacy")
  }
  if (site_arg == "legacy") {
    plot_filter <- c("tamasag", "lincoln")
  }
  if (site_arg == "lincoln") {
    plot_filter <- c("legacy", "timberline", "timberline virridy")
  }
  if (site_arg == "timberline") {
    plot_filter <- c("lincoln", "timberline virridy", "prospect")
  }
  if (site_arg == "timberline virridy") {
    plot_filter <- c("lincoln", "timberline", "prospect", "prospect virridy")
  }
  if (site_arg == "springcreek") {
    plot_filter <- c("prospect virridy", "prospect")
  }
  if (site_arg == "prospect") {
    plot_filter <- c("timberline", "prospect virridy", "boxelder")
  }
  if (site_arg == "prospect virridy") {
    plot_filter <- c("timberline virridy", "prospect", "boxelder")
  }
  if (site_arg == "boxelder") {
    plot_filter <- c("prospect",
                     "prospect virridy",
                     "archery",
                     "archery virridy")
  }
  if (site_arg == "boxcreek") {
    plot_filter <- c("archery", "archery virridy")
  }
  if (site_arg == "archery") {
    plot_filter <- c("boxelder", "archery virridy", "river bluffs")
  }
  if (site_arg == "archery virridy") {
    plot_filter <- c("boxelder", "archery", "river bluffs")
  }
  if (site_arg == "river bluffs") {
    plot_filter <- c("archery", "archery virridy")
  }
  return(plot_filter)
}



# # Get parameters available for a site
# get_parameters <- function(datasets, directory, site) {
#   if (directory == "pre_verification") {
#     data_list <- datasets$pre_verification_data
#   } else {
#     data_list <- datasets$intermediary_data
#   }
#
#   names(data_list) %>%
#     keep(str_detect(., paste0("^", site, "-"))) %>%
#     str_remove(paste0(site, "-"))
# }

get_auto_parameters <- function(parameter) {
  tryCatch({
     read_csv(here("shiny_ver_tool", "data", "meta", "parameter_autoselections.csv"), show_col_types = F) %>%
      filter(main_parameter == parameter) %>%
      pull(sub_parameters) %>%
      first() %>%
      str_split(",", simplify = TRUE) %>%
      as.character() %>%
      str_trim() %>%
      .[. != ""] # Remove any empty strings
  }, error = function(e) character(0))
}


retrieve_relevant_data_name <- function(df_name_arg, year_week_arg = NULL) {

  if (df_name_arg %in% names(verified_data) & any(year_week_arg %in% verified_data[[df_name_arg]]$y_w)) {
    return("verified_data")
  }
  if (df_name_arg %in% names(intermediary_data) & any(year_week_arg %in% intermediary_data[[df_name_arg]]$y_w)) {
    return("intermediary_data")
  }
  if (df_name_arg %in% names(pre_verification_data) & any(year_week_arg %in% pre_verification_data[[df_name_arg]]$y_w)) {
    return("pre_verification_data")
  }
  if (df_name_arg %in% names(all_data) & any(year_week_arg %in% all_data[[df_name_arg]]$y_w)) {
    return("all_data")
  }

}
