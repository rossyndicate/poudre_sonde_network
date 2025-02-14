library(DBI)        # This loads the Database Interface package, which provides a consistent way to connect to databases in R
library(RSQLite)    # This loads the SQLite implementation for R, allowing us to work with SQLite databases

initialize_verification_db <- function() {

  # Creates a connection to a SQLite database file in the specified path
  con <- dbConnect(SQLite(), here("shiny_ver_tool", "ver_tool_v1", "data", "SQLite_database", "verification_tracker.db"))

  # Create metadata table for tracking verification status
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS verification_status (
    file_id INTEGER PRIMARY KEY AUTOINCREMENT,    -- Unique identifier for each file
    sensor_id TEXT NOT NULL,                      -- Identifier for the sensor
    parameter TEXT NOT NULL,                      -- Parameter being verified
    relative_path TEXT NOT NULL,                  -- Path to the file
    verification_state TEXT CHECK(                -- Current verification state
      verification_state IN ('pre', 'in_process', 'verified')
    ) DEFAULT 'pre',
    user_initials TEXT,                          -- User performing verification
    start_time REAL,                             -- Start timestamp
    completion_time REAL,                        -- Completion timestamp
    file_hash TEXT,                              -- File integrity hash
    UNIQUE(sensor_id, parameter)                 -- Prevents duplicate combinations
    )
  ")

  # Create metadata table for audit tracking
  dbExecute(con,
  "CREATE TABLE IF NOT EXISTS verification_audit (
    audit_id INTEGER PRIMARY KEY AUTOINCREMENT,   -- Unique ID for audit entries
    file_id INTEGER,                             -- References the verified file
    event_time REAL,                             -- When event occurred
    event_type TEXT,                             -- Type of event
    user_initials TEXT,                          -- Who performed the action
    notes TEXT,                                  -- Additional notes
    FOREIGN KEY(file_id) REFERENCES verification_status(file_id)  -- Links to status table
    )
  ")

  # Create metadata table for issues

  dbDisconnect(con) # Safely closes the database connection
}

initialize_verification_db() # Calls the function to set up the database
