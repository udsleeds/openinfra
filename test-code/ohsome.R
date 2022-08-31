  # Notes from https://2022.stateofthemap.org/sessions/R7GW3X/

remotes::install_github("GIScience/ohsome-r")
library(ohsome)
library(tidyverse)

leeds_boundary = osmdata::getbb(place_name = "leeds", format_out = "sf_polygon", limit = 1)
q = ohsome_elements_count(leeds_boundary, filter = "highway=cycleway")
ohsome_post(query = q)

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

transport_regions = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/regions_new.geojson")

i = 1
cycleway_lengths = NULL
transport_regions = transport_regions |> 
  filter(str_detect(string = Region_name, pattern = "Yorks|Manc|Lond|Brist|West of|Camb|West Mid|Ox"))
# for(i in 1:3) {
for(i in seq(nrow(transport_regions))) {
# for(i in seq(nrow(transport_regions))) {
  message("Getting data for ", transport_regions$Region_name[i])
  region = transport_regions$geometry[i]
  cycleway_length = region |> 
    ohsome_elements_length(filter = "highway=cycleway") |> 
    set_time("2009/2022/P1M") |>
    ohsome_post() |> 
    mutate(region = transport_regions$Region_name[i])
  cycleway_lengths = bind_rows(cycleway_lengths, cycleway_length)
}

cycleway_lengths |>
  ggplot(aes(x = timestamp, y = value / 1000, fill = region)) +
  geom_area() +
  ylab(label = "Cycleway length (km)")

cycleway_lengths |>
  ggplot(aes(x = timestamp, y = value / 1000)) +
  geom_area() +
  facet_wrap(~region) +
  ylab(label = "Cycleway length (km)")
