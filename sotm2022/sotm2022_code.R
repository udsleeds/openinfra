# load packages
library(tidyverse)
library(sf)
library(mapview)
library(tmap)
library(osmextract)

# downloaded 2022-05-16
wy = readRDS("wy.RDS")

# tags_needed = c("cycleway",
#                 "bicycle",
#                 "wheelchair",
#                 "kerb",
#                 "disabled",
#                 "mobility_scooter",
#                 "handicap",
#                 "foot",
#                 "lit",
#                 "access",
#                 "sidewalk",
#                 "footway",
#                 "incline",
#                 "smoothness",
#                 "est_width",
#                 "width",
#                 "ramp",
#                 "sidewalk_left",
#                 "sidewalk_right",
#                 "ramp_wheelchair",
#                 "footway_left",
#                 "footway_right",
#                 "footway_surface",
#                 "priority",
#                 "sidewalk_both_surface",
#                 "sidewalk_both_width",
#                 "path",
#                 "pedestrian",
#                 "sidewalk_left_width",
#                 "sidewalk_right_width",
#                 "sidewalk_right_surface",
#                 "sidewalk_left_surface",
#                 "maxspeed",
#                 "segregated",
#                 "sloped_curb",
#                 "surface",
#                 "tactile_paving",
#                 "crossing"
# )
# 
# osmextract::oe_match_pattern("west yorkshire")
# region_wy = "West Yorkshire"
# wy = osmextract::oe_get(place = region_wy,
#                         layer = "lines",
#                         force_download = TRUE,
#                         force_vectortranslate = TRUE,
#                         extra_tags = tags_needed)

# saveRDS(wy,
#         "wy.RDS")

# Inclusive Mobility function

inclusive_mobility_get = function(osm_sf) {
  osm_sf_im = osm_sf %>% 
    # kerb: flush or not
    dplyr::mutate(im_kerb = dplyr::if_else(kerb == "flush" | kerb == "no", "flush", "other")) %>% 
    # footway is a ‘pavement’ adjacent to a road
    dplyr::mutate(im_footway = dplyr::case_when(
      footway %in% c("left", "right", "both", "sidewalk") |
        sidewalk %in% c("left", "right", "both", "yes", "separate") |
        # trying to capture footways shared with cyclists
        !is.na(cycleway) & # map cycling infrastructure that is an inherent part of the road
        foot %in% c("yes", "designated") |
        segregated %in% "yes"
      ~ "yes",
      TRUE ~ "no" 
    ) 
    ) %>% 
    # footpath is any other right of way for pedestrians, that does not run adjacent to a road.
    dplyr::mutate(im_footpath = dplyr::case_when(
      highway %in% "footway" & 
        im_footway %in% "no" | 
        # not (always) an inherent part of the road
        highway %in% c("cycleway", "bridleway", "path") & # foot = "designated" is implied
        im_footway %in% "no" &
        ! foot %in% c("no", "private") | 
        ! access %in% c("no", "private") &
        segregated %in% "no" # shared space
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>%
    # presence of a crossing: give-way, signal controlled, none, or yes (but the type is unknown)
    dplyr::mutate(im_crossing = dplyr::case_when(
      stringr::str_detect(crossing, "zebra|uncontr|marked")~ "give-way",
      stringr::str_detect(crossing, "toucan|pedex|puffin|equestrian|light|signal")~ "signal-controlled",
      highway %in% "crossing" | footway  %in% "crossing" | !is.na(crossing) ~ "yes",
      TRUE ~ "no"
    )) %>% 
    # implied footways but there's a lack of data to verify
    dplyr::mutate(im_footway_imp = dplyr::case_when(
      im_footway %in% "no" &
        im_footpath %in% "no" &
        im_crossing %in% "no"
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>% 
    # lighting: yes or no
    dplyr::mutate(im_light = dplyr::case_when( 
      # highway %in% "street_lamp" |
      ! lit %in% c("no", "disused") & ! is.na(lit)
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>% 
    # recategorize speed
    dplyr::mutate(im_maxspeed = maxspeed %>% 
                    parse_number(),
                  im_maxspeed =  dplyr::case_when(
                    im_maxspeed > 1 & im_maxspeed <= 20 ~ "1-20", # up to 20 mph
                    im_maxspeed > 20 & im_maxspeed <= 40 ~ "21-40", # 21 - 40 mph
                    im_maxspeed > 40 & im_maxspeed <= 60 ~ "41-60", # 41 - 60 mph
                    im_maxspeed > 60 ~ "61" # over 60 mph
                  )
    ) %>% 
    # tactile paving: yes, no
    dplyr::mutate(im_tactile = dplyr::case_when(
      ! tactile_paving %in% c("no", "incorrect", "bad") & ! is.na(tactile_paving) 
      ~ "yes",
      ! is.na(tactile_paving)
      ~ "no"
    )
    ) %>% 
    # surface: paved, unpaved, or other
    dplyr::mutate(
      im_surface_paved = dplyr::case_when(
        highway %in% "cycleway"
        ~ "paved",
        
        stringr::str_detect(surface,
                            "pav|asph|chipseal|concrete|paving|sett|cobble|metal|wood|stepping")
        ~ "paved",
        highway %in% c("footway", "bridleway") & # highway = footway implied surface value is unpaved
          ! surface %in% stringr::str_detect(surface, "pav|asph|chipseal|concrete|paving|sett|cobble|metal|wood|stepping")
        ~ "unpaved",
        stringr::str_detect(surface, "unpav|compact|gravel|rock|pebble|ground|dirt|grass|mud|sand|woodchips|snow|ice|salt")
        ~ "unpaved",
        TRUE & !is.na(surface) ~ "other"
      )
    ) %>% 
    # surface: even  or not
    dplyr::mutate(im_surface = dplyr::case_when(
      stringr::str_detect(surface, "asph|concrete")
      ~ "even",
      
      im_surface_paved %in% "paved" &
        smoothness %in% c("excellent", "good")
      ~ "even",
      ! is.na(im_surface_paved) 
      ~ "uneven"
    )
    ) %>% 
    # width: under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      im_width =  width %>% 
        parse_number(),
      im_width = case_when(
        im_width > 0 & im_width < 1.5 ~ " 1.5",
        im_width <= 1.5 & im_width <= 2 ~ "1.5 - 2",
        im_width > 2 ~ "2"
      )
    ) %>% 
    # estimated width: under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      im_width_est = est_width %>% 
        parse_number(),
      im_width_est = case_when(
        im_width_est > 0 & im_width_est < 1.5 ~ " 1.5",
        im_width_est <= 1.5 & im_width_est <= 2 ~ "1.5 - 2",
        im_width_est > 2 ~ "2"
      )
    )
}


# filter out motorways + apply IM function
wy_im = wy %>% filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") %>%  inclusive_mobility_get() 

# proportions
## proportion function
im_proportion_yes = function(osm_im, im_col){
  (osm_im %>% filter(im_col == "yes") %>% nrow()) / osm_im %>% nrow()
}

footpath_prop = im_proportion_yes(wy_im, wy_im$im_footpath) %>%
  cbind("footpath") %>% 
  as.data.frame() %>% 
  setNames(c("prop", "value"))

footway_prop = im_proportion_yes(wy_im, wy_im$im_footway) %>%
  cbind("footway") %>% 
  as.data.frame() %>% 
  setNames(c("prop", "value"))

joined = rbind(footpath_prop,
               footway_prop) %>% 
  as.data.frame() 

# visualizations
## plot a histogram
ggplot2::ggplot(joined,
                aes(x = value,
                    y = prop))+
  geom_bar(stat = "identity")

## plot an interactive
tmap::tmap_mode("view")
tmap::tm_shape(wy_im %>% filter(im_footpath == "yes"))+
  tmap::tm_lines(col = "red")+
  tmap::tm_shape(wy_im %>% filter(im_footway == "yes"))+
  tmap::tm_lines(col = "blue")

# get LCC footfall data

footfall = read.csv("~/Desktop/lcc_footfall.csv")
footfall %>% names()
footfall %>% select(Location) %>% unique()
footfall %>% str()

wy_im %>% names()

# a list of unique locations
location_vec = footfall %>% 
  select(Location) %>% 
  unique() %>% 
  unlist() %>% 
  as.vector()
### excluding Dortmund Square as it's not a highway
location_list = c("Albion Street",
                  "Briggate",
                  "Commercial Street",
                  "The Headrow",
                  "Park Row")

## filter based on location_list
osm_locations = wy_im %>%
  filter(name %in% location_list)
osm_locations %>% nrow()
# plotting those locations based on OSM name tag

tmap::tmap_mode("view")
osm_locations %>% tmap::qtm()

### defining central Leeds as that's the area we need
### there are sterets with the names in `osm_locations` which are not in Leeds. We need Leeds only to match footfall data
map_locations = mapview::mapview(osm_locations)
map_leeds_locations = mapedit::editMap(map_locations)
box = sf::st_bbox(map_leeds_locations$drawn$geometry)
lcc = wy_im[map_leeds_locations$drawn$geometry, op=sf::st_within]
lcc %>% tmap::qtm()
lcc_locations = lcc %>% 
  filter(name %in% location_list)
lcc_locations %>% tmap::qtm()

## check if the streets have data needed for IM and if so, what
lcc_locations %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(!is.na(im_surface)) %>% 
  dplyr::select(im_surface) %>% 
  table()

lcc_locations %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(!is.na(im_tactile)) %>% 
  dplyr::select(im_tactile) %>% 
  table()

tmap::tm_shape(lcc_locations)+
  tmap::tm_lines('im_surface',
                 palette = "Dark2",
                 colorNA = "black",
                 lwd = 2)

tmap::tm_shape(lcc_locations)+
  tmap::tm_lines('im_surface_paved',
                 palette = "Dark2",
                 colorNA = "black",
                 lwd = 2)
tmap::tm_shape(lcc_locations)+
  tmap::tm_lines('im_tactile')

# create a preliminary index

lcc_locations_index = lcc_locations %>% 
  # sf::st_drop_geometry() %>% 
  dplyr::mutate(
    foot_index = dplyr::case_when(
      im_footway == "yes" |
        im_footpath == "yes" |
        im_footway_imp == "yes"
      ~ 1,
      TRUE~ 0
    ),
    kerb_index = dplyr::case_when(
      im_kerb == "flush"
      ~ 1,
      TRUE ~ 0
      ),
    surface_index = dplyr::case_when(
      im_surface == "even" 
      ~ 1,
      TRUE ~ 0
    ),
    tactile_index = dplyr::case_when(
      im_tactile == "yes"
      ~ 1,
      TRUE ~ 0
    ),
    width_index = dplyr::case_when(
      !is.na(im_width) |
        !is.na(im_width_est) ~ 1,
      TRUE ~ 0 
    )
      )  %>% 
  dplyr::select(dplyr::contains("_index")) %>% 
  dplyr::mutate(index = rowSums(.[,1:5, drop = T])) # issue that helped me here: https://github.com/r-spatial/sf/issues/497

lcc_locations_index %>% dplyr::pull(index) %>% table()

## mapping index
tmap::tm_shape(lcc_locations_index)+
  tm_lines("index",
           palette = "Dark2",
           as.count = TRUE,
           lwd = 2)
