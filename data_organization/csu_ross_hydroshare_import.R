# Script to unpack CSU ROSS HydroShare Publication
# Author: Sam Struthers, 2026
# Description: This script automates the extraction of ROSS sensor data from
# HydroShare-formatted ZIP files, joins sensor observations with time-varying
# site metadata, and provides example visualizations for data analysis.
# AI disclosure : This script's comments were generated with the assistance of AI (Google Gemini 3.5) and reviewed by the Author

library(tidyverse)
library(arrow)
library(here)
library(tmap)

# --- Path Configuration ---
# Ensure file_path points to the downloaded .zip from HydroShare.
# The download_path is the final destination for the unzipped 'contents' folder.

#### USER UPDATE AS NEEDED ####
file_path <- "ROSS_HydroShare_Publication.zip"
download_path <- here("ross_sensor_data")
#### ^^ USER UPDATE AS NEEDED ^^ ####

temp_extraction_dir <- file.path(tempdir(), "ross_temp_unzip")

# --- Extraction Logic ---
# Creates a clean temporary environment to isolate the ZIP contents before moving
# to the final project directory.
if(dir.exists(temp_extraction_dir)) unlink(temp_extraction_dir, recursive = TRUE)
dir.create(temp_extraction_dir)

# Native system unzip is utilized for better performance with large environmental datasets
unzip(file_path, exdir = temp_extraction_dir, unzip = "unzip")

# Locates the 'contents' subdirectory within the HydroShare structure
content_dir_source <- list.files(temp_extraction_dir,
                                 pattern = "contents",
                                 recursive = TRUE,
                                 full.names = TRUE,
                                 include.dirs = TRUE)

# Transfer files to final destination
if (length(content_dir_source) > 0) {
  if(!dir.exists(download_path)) dir.create(download_path, recursive = TRUE)

  # file.copy is used to ensure compatibility across different local file systems
  file.copy(from = list.files(content_dir_source, full.names = TRUE),
            to = download_path,
            recursive = TRUE)

  message("Success: Content extracted to ", download_path)
} else {
  stop("Could not find a 'content' folder within the zip file.")
}

# Remove temporary artifacts
unlink(temp_extraction_dir, recursive = TRUE)

#### --- Data Integration: Sensors & Metadata --- ####

# Identify data (years) subdirectories while excluding the metadata folder
data_dirs <- list.dirs(download_path, recursive = FALSE) %>%
  setdiff(file.path(download_path, "metadata"))

# Batch read all cleaned CSV files into a single unified dataframe
cleaned_files <- list.files(data_dirs, pattern = "cleaned.*\\.csv$", full.names = TRUE)

cleaned_data <- map(cleaned_files, read_csv, show_col_types = FALSE) %>%
  bind_rows()

# Load site-specific metadata including coordinates and deployment windows
metadata_file <- file.path(download_path, "metadata", "CSU_ROSS_sonde_location_metadata.csv")
if(file.exists(metadata_file)) {
  metadata <- read_csv(metadata_file, show_col_types = FALSE)%>%
    # A 1-day buffer accounts for potential timezone offsets or deployment overlap
    mutate(
      join_start = data_start_date - days(1),
      join_end = data_end_date + days(1)
    )
} else {
  stop("Metadata file not found at expected location: ", metadata_file)
}

# --- Non-Equi Join ---
# Joins metadata to observations based on both site ID and the specific time window
# to ensure correct spatial coordinates for sensors that may have moved over time.
cleaned_data_with_metadata <- cleaned_data %>%
  left_join(
    metadata,
    by = join_by(
      site,
      DateTime_UTC >= join_start,
      DateTime_UTC <= join_end
    )
  )

####---- Visualization & Data Exploration ---- ####

# Generate a spatial object for mapping
sites_sf <- cleaned_data_with_metadata %>%
  select(site, site_description,latitude, longitude) %>%
  distinct() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Interactive Map View
tmap_mode("view")

tm_basemap("CartoDB.Positron") +
  tm_shape(sites_sf) +
  tm_symbols(
    fill = "site",
    fill.scale = tm_scale_categorical(values = "Set3"),
    size = 0.8,
    shape = 21,
    title.fill = "Site Name"
  ) +
  tm_text("site", size = 0.7, ymod = 1.2) +
  tm_layout(
    title = "CSU ROSS Sensor Locations",
    title.position = c("center", "top"),
    legend.position = c("right", "bottom"),
    legend.frame = TRUE,
    legend.title.size = 1,
    legend.text.size = 0.6,
    inner.margins = c(0.1, 0.1, 0.1, 0.1)
  )

# --- Analysis: Site Comparison ---
# Subset data for targeted comparison of Temperature and Specific Conductivity
data_subset <- cleaned_data_with_metadata %>%
  filter(site %in% c("bellvue", "archery") & parameter %in% c("Temperature", "Specific Conductivity"))

# Dynamic labeller to include units automatically in plot facets
parameter_labs <- data_subset%>%
  distinct(units, parameter)%>%
  mutate(title = paste0(parameter, " (", units, ")"))%>%
  select(parameter, title) %>%
  deframe()

# Time Series Visualization
ggplot(data_subset, aes(x = DateTime_UTC, y = value_final, color = site)) +
  geom_line() +
  facet_wrap(~ parameter, scales = "free_y", labeller = as_labeller(parameter_labs)) +
  scale_color_manual(values = c("bellvue" = "#E70870", "archery" = "#256BF5")) +
  labs(title = "Temperature and Specific Conductivity at Bellvue and Archery",
       x = "Date Time (UTC)",
       y = NULL,
       color = "Site")

# 1:1 Scatter Plot for site-to-site correlation analysis
data_subset%>%
  pivot_wider(id_cols = c("DateTime_UTC", "parameter"), names_from = site, values_from = value_final)%>%
  ggplot(aes(x = bellvue, y = archery, color = yday(DateTime_UTC))) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ parameter, scales = "free", labeller = as_labeller(parameter_labs)) +
  labs(title = "Bellvue vs Archery: Temperature and Specific Conductivity",
       x = "Bellvue",
       y = "Archery",
       color = "Day of Year") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black")
