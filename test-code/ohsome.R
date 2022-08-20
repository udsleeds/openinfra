  # Notes from https://2022.stateofthemap.org/sessions/R7GW3X/

remotes::install_github("GIScience/ohsome-r")
library(ohsome)
library(tidyverse)

leeds_boundary = osmdata::getbb(place_name = "leeds", format_out = "sf_polygon", limit = 1)
q = ohsome_elements_count(leeds_boundary, filter = "highway=cycleway")
ohsome_post(query = q)
library(ggplot2)

cycleway_count = q |> 
  set_time("2009/2022/P1M") |>
  ohsome_post()

cycleway_count |>
  ggplot(aes(x = timestamp, y = value)) +
  geom_line()


cycleway_length = leeds_boundary |> 
  ohsome_elements_length(filter = "highway=cycleway") |> 
  set_time("2009/2022/P1M") |>
  ohsome_post()
tail(cycl)

cycleway_length |>
  ggplot(aes(x = timestamp, y = value / 1000)) +
  geom_line() +
  ylab(label = "Cycleway length (km)")

