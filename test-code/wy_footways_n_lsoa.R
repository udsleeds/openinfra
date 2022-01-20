# libraries
library(tidyverse)
library(pct)
library(tmap)
library(sf)


# piggyback::pb_download("leeds_pct.Rds")
# leeds_pct = readRDS("leeds_pct.Rds")
# piggyback::pb_download("wy_walking.Rds")
# wy_walking = readRDS("wy_walking.Rds")
# piggyback::pb_download("leeds_walking_ped.RDS")
# leeds_walking_ped = readRDS("leeds_walking_ped.RDS")

leeds_walking = wy_walking[leeds_pct, op = sf::st_intersects]
leeds_nomot = wy_walking[leeds_pct, op = sf::st_intersects] %>% filter(highway != "motorway" | highway != "motorway_link")

leeds_pct_geom = leeds_pct %>% rename(geom = geometry,
                                      foot_census = foot)

# leeds_walking_gr1 = sf::st_join(leeds_pct_geom,
#                                 leeds_walking_ped_nona) %>% 
#   select(pedestrian_friendly, foot_census, geo_code)

leeds_foot = leeds_nomot %>% filter(highway == "footway")
leeds_walking_gr2 = sf::st_join(leeds_foot,
                                leeds_pct_geom) %>% 
  select(highway, foot, geo_code)

leeds_foot_n = leeds_walking_gr2 %>% 
  group_by(geo_code) %>% 
  summarize(N = n())

tm_shape(leeds_pct_geom)+
  tm_polygons("foot_census")+
tm_shape(leeds_foot_n)+
  tm_lines("N",
           palette = "Dark2")

