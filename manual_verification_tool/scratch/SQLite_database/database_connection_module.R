# Creates a function to handle database connections
get_db_connection <- function() {
  # Establishes database connection
  dbConnect(
    SQLite(), # Specifies we're using SQLite database
    normalizePath(here("shiny_ver_tool", "ver_tool_v1", "data", "SQLite_database", "verification_tracker.db")) # Converts relative path to absolute path
  )
}

# Creates a function to handle file paths
# TODO: where is relative path getting made and used?
resolve_shared_path <- function(relative_path) {
  # Combines path components (base path + relative path components)
  file.path(normalizePath(here()), relative_path)
}

