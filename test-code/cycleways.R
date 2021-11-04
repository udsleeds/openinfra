# Aim: get cycleways in west yorkshire

library(osmextract) # cran version
library(tidyverse)

region_name = "isle of wight" # region to get cycleways for

osm_highway = osmextract::oe_get(place = region_name, layer = "lines", force_vectortranslate = TRUE)
osm_highways_cycling = osmextract::oe_get_network(place = region_name, mode = "cycling")

nrow(osm_highway) / nrow(osm_highways_cycling) # 3 times fewer lines in cycling data

names(osm_highway)
names(osm_highways_cycling)

et = "maxspeed"
osm_highway_maxspeed = osmextract::oe_get(region_name, extra_tags = et)
names(osm_highway_maxspeed)

# experiment: try missing tag
et = c("maxspeed", "dfdkdkdfdsdfd")
osm_highway_test1 = osmextract::oe_get(region_name, extra_tags = et)
unique(osm_highway_test1$dfdkdkdfdsdfd)
class(osm_highway_test1$dfdkdkdfdsdfd)
head(osm_highway_test1$dfdkdkdfdsdfd)

# extract all other tags
?osmextract::oe_get_keys(osm_highways)
osmextract::oe_get_keys(osm_highways_cycling)

# identify any rows that have "cycle" in other_tags
osm_cycle_other_tags = osm_highway %>% 
  filter(str_detect(other_tags, "cycle"))
table(osm_cycle_other_tags$other_tags)

# experiment: try with a subtag
# et = c("maxspeed", "cycleway:right:track") # seemingly non-existent tag?
et = c("maxspeed", "cycleway:left")
osm_highway_test1 = osmextract::oe_get("west yorkshire", extra_tags = et, force_vectortranslate = T)
table(osm_highway_test1$cycleway_left)
# buffered_lane           lane             no opposite_track   share_busway 
# 8            104            101              1             10 
# share_sidewalk    shared_lane          track  unmarked_lane            yes 
# 1             18             10              1              3 

osm_highway_cycleway_left = osm_highway_test1 %>% 
  filter(!is.na(cycleway_left) & cycleway_left != "no")
table(osm_highway_cycleway_left$cycleway_left)

saveRDS(osm_highway_cycleway_left, "osm_highway_cycleway_left.Rds")


# spatial analysis of data
nrow(osm_highway_cycleway_left) # less than 1000 rows - can map interactive 
library(tmap)
tmap_mode("view")
qtm(osm_highway_cycleway_left)
# if datset is hug
osm_highway_cycleway_left %>% 
  sample_n(size = 100) %>% 
  qtm()

# geographic subset
leeds = pct::get_pct_zones(region = "west-yorkshire")
sf::sf_use_s2(FALSE)
leeds_with_cycleway_touching = leeds[osm_highway_cycleway_left, ]
qtm(leeds_with_cycleway_touching)

leeds_with_cycleway_within = leeds[osm_highway_cycleway_left, , op = sf::st_contains]
qtm(leeds_with_cycleway_within)

osm_centroids = sf::st_centroid(osm_highway_cycleway_left)

leeds_names = leeds %>% select(geo_code, bicycle)
osm_joined = sf::st_join(osm_centroids, leeds_names)
names(osm_joined)

osm_n_infra_per_zone = osm_joined %>% 
  sf::st_drop_geometry() %>% 
  group_by(geo_code) %>% 
  summarise(n_cycleways = n()) # in future: calculate length and other things

leeds_joined = left_join(leeds_names, osm_n_infra_per_zone)
leeds_joined %>% select(n_cycleways) %>% plot()

# check buffers ?
