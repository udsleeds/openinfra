# Aim: get cycleways in west yorkshire

library(osmextract) # cran version
library(tidyverse)

region_name = "isle of wight" # region to get cycleways for

osm_highways = osmextract::oe_get_network(place = region_name, mode = "cycling")


