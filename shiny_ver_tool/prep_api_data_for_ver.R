library(here)
library(tidyverse)
library(plotly)
library(arrow)
library(furrr)
# loading packages
package_loader <- function(x) {
  if (x %in% installed.packages()) {
    suppressMessages({
      library(x, character.only = TRUE)
    })
  } else {
    suppressMessages({
      install.packages(x)
      library(x, character.only = TRUE)
    })
  }
}

invisible(
  lapply(c("arrow",
           "data.table",
           "httr2",
           "tidyverse",
           "lubridate",
           "zoo",
           "padr",
           "stats",
           "RcppRoll",
           "yaml",
           "here",
           #"fcw.qaqc",
           "furrr"
  ),
  package_loader)
)

# select correct verification cycle to load
current_cycle_folder = "2024_cycle"

params_to_use <- c("Chl-a Fluorescence","Depth", "Specific Conductivity",
                   "Temperature", "Turbidity", "ORP", "pH", "DO", "FDOM Fluorescence")
params <- paste(params_to_use, collapse = "|")

#get files in hydrovu_2024_data
files <- tibble(filename = list.files(here("data","manual_data_verification",current_cycle_folder, "hydro_vu_pull", "flagged_sam"), full.names = TRUE))%>%
  filter(grepl(pattern = params, filename))%>%
  #remove extras
  filter(!grepl(pattern = "Level|MV", filename))


# read in files
all_data <- map_dfr(files, ~read_csv(.x, show_col_types = F)) %>%
  #save DT_join as a character after converting it to MST
  mutate(DT_join = with_tz(DT_join, tz = "MST") %>% as.character())%>%
  #turn into individual dataframes by site and parameter
  split(f = list(.$site, .$parameter), sep = "-") %>%
  keep(~nrow(.) > 0)


# clean up columns to match shiny app requirements

all_data_tidy <- all_data%>%
  bind_rows()%>%
  mutate(DT_round = with_tz(DT_round, tz = "MST"))%>%
  rename(value = mean,
         flag = auto_flag)

ggplot(all_data_tidy%>%filter(site == "sfm"), aes(x = DT_round, y = value, colour = flag)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free_y") +
  labs(
       x = "Date and Time (MST)",
       y = "Value",
       color = "Parameter")


# save to raw data file to be processed later on
write_rds(all_data_tidy, here("data", "manual_data_verification", current_cycle_folder, "in_progress","raw_data","raw_data.rds" ))
