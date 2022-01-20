
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
leeds_walking_ped = wy_walking[leeds_pct, op = sf::st_intersects] %>% recode_pedestrian()

leeds_walking_ped %>% pull(pedestrian_friendly) %>% table()
leeds_walking_ped %>% names

leeds_walking_ped_yes = leeds_walking_ped %>% filter(pedestrian_friendly == "yes") %>% select(pedestrian_friendly, osm_id, name, highway,foot, sidewalk, footway)
leeds_walking_ped_yes %>% mapview::mapview()

leeds_walking_ped_no = leeds_walking_ped %>% filter(pedestrian_friendly == "no") %>% select(pedestrian_friendly, osm_id, name, highway,foot, sidewalk, footway)
leeds_walking_ped_no %>% mapview::mapview()



tmap_mode("view")
tm_shape(leeds_walking_ped_yes)+
  tm_lines(col = "green")+
  tm_shape(leeds_walking_ped_no)+
  tm_lines(col = "red")

leeds_walking_ped_yes[leeds_walking_ped_no, op = st_within]

# et_walking <- c("wheelchair",
# "kerb",
# "disabled",
# "mobility_scooter",
# "handicap",
# "foot",
# "lit", # https://wiki.openstreetmap.org/wiki/Key:lit
# "access",
# "sidewalk",
# "footway",
# "incline",
# "smoothness",
# "est_width",
# "ramp",
# "sidewalk_left",
# "sidewalk_right",
# "ramp_wheelchair",
# "footway_left",
# "footway_right",          
# "footway_surface", 
# "priority",
# "sidewalk_both_surface", 
# "path",                                   
# "pedestrian",
# "capacity_disabled",
# "sidewalk_left_width",                    
# "sidewalk_right_surface",
# "width",
# "tactile_paving")
# 
# region_name <- "West Yorkshire"

# wy_walking_dec <- osmextract::oe_get(region_name,
#                                  force_vectortranslate = TRUE,
#                                  extra_tags = et_walking
#                                  )

leeds_walking_dec_ped = wy_walking_dec[leeds_pct, op = sf::st_intersects] %>% recode_pedestrian()

leeds_walking_dec_ped_yes = leeds_walking_dec_ped %>% filter(pedestrian_friendly == "yes") %>% select(pedestrian_friendly, osm_id, name, highway,foot, sidewalk, footway)

leeds_walk_diff = st_difference(x = leeds_walking_dec_ped, y = leeds_walking_dec_ped)

leeds_walking_dec_ped %>% pull(pedestrian_friendly) %>% table()
leeds_walking_ped %>% pull(pedestrian_friendly) %>% table()


leeds_yes_diff_plot = tm_shape(leeds_walking_dec_ped_yes)+
  tm_lines(col = "red")+
  tm_shape(leeds_walking_ped_yes)+
  tm_lines(col = "blue")
tmap_save(leeds_yes_diff_plot,
          "leeds_yes_diff_plot.png")

leeds_walking_new = leeds_walking %>% 
  mutate(pedestrian_friendly = case_when(
    highway %in% highway_footway_yes ~ "yes",
    highway %in% highway_footway_maybe ~ "maybe",
    highway %in% highway_footway_no ~ "no"
  ))

tmap::qtm(leeds_walking_new, "pedestrian_friendly")

leeds_walking_new2 = leeds_walking %>% recode_pedestrian(encouraged = "no")

leeds_walking_new %>% pull(pedestrian_friendly) %>% table()
wy_walking %>% pull(width) %>% table()
leeds_walking_new2 %>% pull(width) %>% class


## ---- echo=FALSE,warning=FALSE,message=FALSE,error=FALSE, results='hide',fig.keep='hide'----
leeds_pct_geom = leeds_pct %>% rename(geom = geometry,
                                      foot_census = foot)
leeds_pct$geometry %>% plot
leeds_pct_geom$geom %>% plot

leeds_walking_gr1 = sf::st_join(leeds_pct_geom,
                                leeds_walking_ped_nona) %>% 
  select(pedestrian_friendly, foot_census, geo_code)
leeds_walking_gr2 = sf::st_join(leeds_walking_ped_nona,
                                leeds_pct_geom) %>% 
  select(pedestrian_friendly, foot_census, geo_code)

# "yes"
leeds_walking_grouped = leeds_walking_gr2 %>%
  #sf::st_drop_geometry() %>% 
  filter(pedestrian_friendly == "yes") %>% 
  select(pedestrian_friendly, geo_code) %>% 
  group_by(geo_code) %>% 
  summarise(n_ped_lsoa = n())

tm_shape(leeds_walking_grouped)+
  tm_lines(col = "n_ped_lsoa")

leeds_walking_grouped1 = leeds_walking_gr1 %>%
  #sf::st_drop_geometry() %>% 
  filter(pedestrian_friendly == "yes") %>% 
  select(pedestrian_friendly, geo_code) %>% 
  group_by(geo_code) %>% 
  summarise(n_ped_lsoa = n())
tm_shape(leeds_walking_grouped1)+
  tm_polygons(col = "n_ped_lsoa")

# "maybe"
leeds_walking_grouped2 = leeds_walking_gr2 %>%
  #sf::st_drop_geometry() %>% 
  filter(pedestrian_friendly == "maybe") %>% 
  select(pedestrian_friendly, geo_code) %>% 
  group_by(geo_code) %>% 
  summarise(maybe_ped_lsoa = n())

tm_shape(leeds_walking_grouped2)+
  tm_lines(col = "maybe_ped_lsoa")

leeds_walking_grouped3 = leeds_walking_gr1 %>%
  #sf::st_drop_geometry() %>% 
  filter(pedestrian_friendly == "maybe") %>% 
  select(pedestrian_friendly, geo_code) %>% 
  group_by(geo_code) %>% 
  summarise(maybe_ped_lsoa = n())
tm_shape(leeds_walking_grouped1)+
  tm_polygons(col = "n_ped_lsoa")

tm_shape(leeds_walking_grouped3)+
  tm_polygons(col = "maybe_ped_lsoa")

# "no"
leeds_walking_grouped4 = leeds_walking_gr2 %>%
  #sf::st_drop_geometry() %>% 
  filter(pedestrian_friendly == "no") %>% 
  select(pedestrian_friendly, geo_code) %>% 
  group_by(geo_code) %>% 
  summarise(no_ped_lsoa = n())
tm_shape(leeds_walking_grouped2)+
  tm_lines(col = "no_ped_lsoa")

leeds_walking_grouped5 = leeds_walking_gr1 %>%
  #sf::st_drop_geometry() %>% 
  filter(pedestrian_friendly == "no") %>% 
  select(pedestrian_friendly, geo_code) %>% 
  group_by(geo_code) %>% 
  summarise(no_ped_lsoa = n())
tm_shape(leeds_walking_grouped5)+
  tm_polygons(col = "no_ped_lsoa")


leeds_walking_polygons = dplyr::left_join(leeds_walking_grouped1, st_drop_geometry(leeds_walking_grouped3),
                                          by = "geo_code") %>% 
  dplyr::left_join(st_drop_geometry(leeds_walking_grouped5),
                   by = "geo_code")

tm_shape(leeds_walking_polygons) +
  tm_polygons(c("n_ped_lsoa", 
                "maybe_ped_lsoa", 
                "no_ped_lsoa"))

tm_shape(leeds_walking_linestrings) +
  tm_lines(c("n_ped_lsoa"))

most_n_ped = leeds_walking_grouped %>% filter(geo_code == "E01011265") %>% sf::st_cast("LINESTRING") # polygon to linestrings
maybe_most_n_ped = leeds_walking_grouped2 %>% filter(geo_code == "E01011265") %>% sf::st_cast("LINESTRING")
no_most_n_ped = leeds_walking_grouped4 %>% filter(geo_code == "E01011265") %>% sf::st_cast("LINESTRING")

leeds_walking_grouped2

tmap_mode("view")
leeds_ped_yes_lsoa_tm = tm_shape(most_n_ped)+
  tm_lines()+
  tm_shape(maybe_most_n_ped)+
  tm_lines(col = "red") +
  tm_shape(no_most_n_ped)+
  tm_lines(col = "blue")

tmap_save(leeds_ped_yes_lsoa_tm,
          "leeds_ped_yes_lsoa_tm.html")

maybe_most_n_ped$maybe_ped_lsoa %>% table