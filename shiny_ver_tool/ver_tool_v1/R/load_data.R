get_filenames <- function(){
  all_dir_path <- here("shiny_ver_tool", "ver_tool_v1", "data", "all_data_directory")# all_data_path
  pre_dir_path <- here("shiny_ver_tool", "ver_tool_v1", "data", "pre_verification_directory")# pre_verification_path
  int_dir_path <- here("shiny_ver_tool", "ver_tool_v1", "data", "intermediary_directory")# intermediary_path
  ver_dir_path <- here("shiny_ver_tool", "ver_tool_v1", "data", "verified_directory")# verified_path

  all_dir_names <- list.files(all_dir_path)
  pre_dir_names <- list.files(pre_dir_path)
  int_dir_names <- list.files(int_dir_path)
  ver_dir_names <- list.files(ver_dir_path)

  #create a tibble that has the relevant filenames in a column and their directory in another
  file_paths <- tibble(
    filename = c(all_dir_names, pre_dir_names, int_dir_names, ver_dir_names),
    directory = c(rep("all_data", length(all_dir_names)),
                  rep("pre_verification", length(pre_dir_names)),
                  rep("intermediary", length(int_dir_names)),
                  rep("verified", length(ver_dir_names)))
  )
  return(file_paths)

}


load_all_datasets <- function() {

  pre_verification_path = here("shiny_ver_tool", "ver_tool_v1","data", "pre_verification_directory")
  intermediary_path = here("shiny_ver_tool", "ver_tool_v1","data", "intermediary_directory")
  verified_path = here("shiny_ver_tool", "ver_tool_v1","data", "verified_directory")

  list(
    # all_data = set_names(
    #   map(list.files(paths$all_path, full.names = TRUE), read_rds),
    #   list.files(paths$all_path)
    # ),
    pre_verification_data = set_names( map(list.files(pre_verification_path, full.names = TRUE), read_rds),
                                       list.files(pre_verification_path)
    ),
    intermediary_data = set_names(
      map(list.files(intermediary_path, full.names = TRUE), read_rds),
      list.files(intermediary_path)
    ),
    verified_data = set_names(
      map(list.files(verified_path, full.names = TRUE), read_rds),
      list.files(verified_path)
    )
  )
}

# #For Online version
# load_data_directories <- function() {
#   list(
#     all_path =  "data/all_data_directory",
#     pre_verification_path =  "data/pre_verification_directory",
#     intermediary_path = "data/intermediary_directory",
#     verified_path = "data/verified_directory"
#   )
# }
#
# load_all_datasets <- function(paths) {
#   list(
#     all_data = set_names(
#       map(list.files(paths$all_path, full.names = TRUE), read_rds),
#       list.files(paths$all_path)
#     ),
#     pre_verification_data = set_names(
#       map(list.files(paths$pre_verification_path, full.names = TRUE), read_rds),
#       list.files(paths$pre_verification_path)
#     ),
#     intermediary_data = set_names(
#       map(list.files(paths$intermediary_path, full.names = TRUE), read_rds),
#       list.files(paths$intermediary_path)
#     ),
#     verified_data = set_names(
#       map(list.files(paths$verified_path, full.names = TRUE), read_rds),
#       list.files(paths$verified_path)
#     )
#   )
# }
