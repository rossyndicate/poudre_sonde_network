
#pull in data from 9/1/19 - 12/31-19 for 06752260
#usgs lincoln: CLAFORCO
#usgs boxelder: CLABOXCO
#devtools::install_github("anguswg-ucsb/cdssr")
library(cdssr)
library(tidyverse)
library(plotly)

start_dt = "2019-09-01"
end_dt = "2019-12-31"

lincoln <- get_telemetry_ts(abbrev = "CLAFORCO", start_date = start_dt, end_date = end_dt, timescale = "raw")
boxelder <- get_telemetry_ts(abbrev = "CLABOXCO", start_date = start_dt, end_date = end_dt, timescale = "raw")

ggplotly(
  ggplot() +
    geom_line(data = lincoln, aes(x = datetime, y = meas_value)) +
    labs(x = "Date", y = "Streamflow (cfs)")
)

ggplotly(
  ggplot() +
    geom_line(data = boxelder, aes(x = datetime, y = meas_value)) +
    labs(x = "Date", y = "Streamflow (cfs)")
)
