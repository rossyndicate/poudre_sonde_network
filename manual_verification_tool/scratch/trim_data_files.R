setwd("shiny_ver_tool/ver_tool_v1/")
getwd()
library(tidyverse)
#loop through each file in the folder data/pre_verification_directory, load it, trim it so the max date is 2023-06-01
#and overwrite the original file
files <- list.files("data/pre_verification_directory", full.names = TRUE)

for (file in files) {
  data <- read_rds(file)
  new_data <- data%>%
    filter(DT_round <= ymd_hms("2023-06-01 00:00:00"))
  write_rds(new_data, file)
}
