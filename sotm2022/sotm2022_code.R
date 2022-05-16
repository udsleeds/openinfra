# load packages
library(tidyverse)
library(sf)
library(mapview)
library(tmap)
library(osmextract)

# download OSM highway data for West Yorkshire (downloaded 2022-04-01)
wy = sf::st_read("https://github.com/udsleeds/openinfra/releases/download/v0.1/wy-01-04-2022.geojson")

mers = sf::st_read("https://github.com/udsleeds/openinfra/releases/download/v0.1/mers-01-04-2022.geojson")


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

wy_im = inclusive_mobility_get(wy)


tmap::tmap_mode("plot")
tmap::tm_shape(wy %>% filter(highway == "cycleway"))+
  tmap::tm_lines()


tags_needed = c("cycleway",
                "bicycle",
                "wheelchair",
                "kerb",
                "disabled",
                "mobility_scooter",
                "handicap",
                "foot",
                "lit",
                "access",
                "sidewalk",
                "footway",
                "incline",
                "smoothness",
                "est_width",
                "width",
                "ramp",
                "sidewalk_left",
                "sidewalk_right",
                "ramp_wheelchair",
                "footway_left",
                "footway_right",
                "footway_surface",
                "priority",
                "sidewalk_both_surface",
                "sidewalk_both_width",
                "path",
                "pedestrian",
                "sidewalk_left_width",
                "sidewalk_right_width",
                "sidewalk_right_surface",
                "sidewalk_left_surface",
                "maxspeed",
                "segregated",
                "sloped_curb",
                "surface",
                "tactile_paving",
                "crossing"
                )


osmextract::oe_match_pattern("west yorkshire")
region_wy = "West Yorkshire"
wy = osmextract::oe_get(place = region_wy,
                         layer = "lines",
                         force_download = TRUE,
                         force_vectortranslate = TRUE,
                         extra_tags = tags_needed)

saveRDS(wy,
        "wy.RDS")

wy_df = wy %>% sf::st_drop_geometry() %>% filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") %>%  inclusive_mobility_get() 
wy_im = wy %>% filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") %>%  inclusive_mobility_get() 


wy_im %>% filter(im_footway == "yes") %>% 
  tmap::tm_shape()+
  tmap::tm_lines()
