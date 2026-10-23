# Quick script to create a map of all ROSS CLP sites and their monitoring status.

library(tidyverse)
library(sf)
library(tmap)
library(here)


loc <- read_csv("data/raw/chem/ross_clp_chem/v2025.11.14/data/metadata//location_metadata.csv" )%>%
  mutate(site_code = tolower(site_code))%>%
  select(site_code, Lat, Long, Site_Name)%>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

loc_trim <- loc%>%
  mutate(`Monitoring Status` = case_when(site_code %in% c("pbd", "sfm", "pfal","pbr", "pman") ~ "Sensor & Sampling Ongoing",
                                   site_code %in% c( "legacy", "lincoln", "timberline", "prospect", "boxelder", "archery", "riverbluffs") ~ "Sensor Ongoing",
                                   site_code %in% c("beav", "brnr","cbri","cbrr" , "comi" ,
                                                    "comr", "hori" ,"horr","joei", "joer",
                                                    "jwc" ,"lbea",  "lngr", "penn","pjw","pnf","psf","ptrr", "sawm") ~ "Monitoring Concluded"),
         `Data Collected` = case_when(site_code %in% c("pbd", "sfm", "pfal","pbr","joei","lbea","cbri",
                                                  "pman","legacy", "lincoln", "timberline","penn",
                                                  "prospect", "boxelder", "archery", "riverbluffs") ~ "Sensor & Sampling",
                                 site_code %in% c("beav", "brnr","cbrr" , "comi" ,
                                                  "comr", "hori" ,"horr", "joer",
                                                  "jwc" ,  "lngr", "pjw","pnf","psf","ptrr", "sawm") ~ "Sampling Only"))%>%
  filter(!is.na(`Monitoring Status`))


# Calculate a buffered bounding box (in meters)
loc_bbox_buf <- loc_trim %>%
  st_transform(32613) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(dist = 6500) %>%
  st_transform(4326)

# Plot
tmap_mode("plot")

tm <- tm_shape(loc_bbox_buf) +
  tm_basemap("OpenStreetMap") +
  tm_shape(loc_trim) +
  tm_symbols(
    fill = "Monitoring Status",
    fill.scale = tm_scale_categorical(
      values = c("Sensor & Sampling Ongoing" = "#1E4D2B",
                 "Sensor Ongoing" = "#256BF5",
                 "Monitoring Concluded" = "#d9d9d9")),
    shape = "Data Collected",
    shape.scale = tm_scale_categorical(
      values = c("Sensor & Sampling" = 21,
                 "Sampling Only" = 22) )) +
  tm_layout(title = "ROSS CLP WQ Monitoring Sites",
            title.position = c("center", "top"),
            legend.position = c("right", "top"),
            legend.title.size = 1,
            legend.text.size = 0.8)


tm
tmap_save(
  tm,
  filename = here("docs", "uclp_dss","figs","all_ross_sites_map.png"),
  width = 12,height = 10, dpi = 300
)


