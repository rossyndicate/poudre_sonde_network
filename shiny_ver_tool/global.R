#global variables

library(shiny)
library(bslib)
library(DT)
library(tidyverse)
library(lubridate)
library(here)
library(ggpubr)
library(gridExtra)
library(plotly)
library(keys)
library(patchwork)
library(digest)
library(fs)
library(shinyFiles)
library(shinyWidgets)
library(glue)
library(anytime)
options(shiny.maxRequestSize = 10000 * 1024^2)

`%nin%` = Negate(`%in%`)

##### Colors + parameters #####

site_color_combo <- tibble(site = c("joei", "cbri", "chd", "pfal", "sfm", "lbea", "penn", "pbd","bellvue","salyer", "udall", "riverbend_virridy", "riverbend",
                                    "cottonwood_virridy", "cottonwood","elc",  "archery_virridy", "archery", "boxcreek", "springcreek", "riverbluffs"),
                           color = c("#771155", "#AA4488", "#CC99BB", "#114477", "#4477AA", "#77AADD", "#117777", "#44AAAA", "#77CCCC",
                                     "#117744", "#44AA77", "#88CCAA", "#777711", "#AAAA44","#DDDD77", "#774411", "#AA7744", "#DDAA77", "#771122", "#AA4455", "#DD7788"))


final_status_colors <- c("PASS" = "green",
                         "OMIT" = "red",
                         "FLAGGED" = "orange")



available_parameters <- get_filenames()%>%mutate(
  parameter = map_chr(filename, ~ split_filename(.x)$parameter))%>%
  pull(parameter)%>%
  unique()


available_sites <- get_filenames()%>%mutate(
  site = map_chr(filename, ~ split_filename(.x)$site))%>%
  pull(site)%>%
  unique()

#TODO: Automate for public version or ask for user input
available_flags <- read_csv(here("shiny_ver_tool", "data", "meta", "available_flags.csv"), show_col_types = F)%>%
  pull(flags)%>%
  unique()

###### End Helper Functions ######
