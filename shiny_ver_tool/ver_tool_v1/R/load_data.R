
load_data_directories <- function() {
  list(
    all_path = here("shiny_ver_tool", "ver_tool_v1", "data", "all_data_directory"),
    pre_verification_path = here("shiny_ver_tool", "ver_tool_v1", "data", "pre_verification_directory"),
    intermediary_path = here("shiny_ver_tool", "ver_tool_v1", "data", "intermediary_directory"),
    verified_path = here("shiny_ver_tool", "ver_tool_v1", "data", "verified_directory")
  )
}

load_all_datasets <- function(paths) {
  list(
    all_data = set_names(
      map(list.files(paths$all_path, full.names = TRUE), read_rds),
      list.files(paths$all_path)
    ),
    pre_verification_data = set_names(
      map(list.files(paths$pre_verification_path, full.names = TRUE), read_rds),
      list.files(paths$pre_verification_path)
    ),
    intermediary_data = set_names(
      map(list.files(paths$intermediary_path, full.names = TRUE), read_rds),
      list.files(paths$intermediary_path)
    ),
    verified_data = set_names(
      map(list.files(paths$verified_path, full.names = TRUE), read_rds),
      list.files(paths$verified_path)
    )
  )
}
