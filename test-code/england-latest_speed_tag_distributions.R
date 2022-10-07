

# Library set-up ----------------------------------------------------------

pkgs = c("sf",
         "osmextract",
         "tidyverse",
         "tmap",
         "openinfra"
        )
# Load packages
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]


# Parameters --------------------------------------------------------------

required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                  "kerb", "footway", "sidewalk", "cycleway", "segregated",
                  "highway", "crossing", "lit", "tactile_paving", "surface",
                  "smoothness", "width", "est_width", "lit_by_led", "ref",
                  "amenity", "sidewalk", "sidewalk:left", "sidewalk:right",
                  "sidewalk:both", "source:maxspeed", "maxspeed:type",
                  "zone:maxspeed", "zone:traffic", "maxspeed", "HFCS", "rural",
                  "cycleway_left", "cycleway_right", "cycleway_both",
                  "separation"
                 )

lit_way_vals = c("05:00-00:00", "05:00-dawn;dusk-24:00", "24/7", "automatic",
                 "both", "dusk-dawn", "sunset-00:00,05:00-sunrise UTC", "yes"
                )

region_name = "England"


# Get data ----------------------------------------------------------------

england_osm = osmextract::oe_get(
  place = region_name,
  extra_tags = required_tags,
  max_file_size = 9e+08,
  layer = "lines"
) 

# Remove waterways & aerial ways
message(nrow(england_osm))
england_osm = england_osm %>% dplyr::filter(! is.na(highway))
message(nrow(england_osm))

# All highway=residential ways in England
eng_res_highway = england_osm %>% dplyr::filter(highway == "residential")
# All highway=residential ways in England that ARE lit
lit_eng_res_highway = eng_res_highway %>% dplyr::filter(lit %in% lit_way_vals)

# Remove residential ways with NA maxspeed (unknown condition)
eng_res_highway_parsed = eng_res_highway %>% dplyr::filter(! is.na(maxspeed))
lit_eng_res_highway_parsed = lit_eng_res_highway %>% dplyr::filter(! is.na(maxspeed))

# Parse maxspeed column as number
eng_res_highway_parsed[["maxspeed"]] = eng_res_highway_parsed[["maxspeed"]] %>% parse_number()
lit_eng_res_highway_parsed[["maxspeed"]] = lit_eng_res_highway_parsed[["maxspeed"]] %>% parse_number()

# Get value count of all maxspeeds (Visualisation)
eng_res_highway_speeds = as.data.frame(table(eng_res_highway_parsed$maxspeed))
lit_eng_res_highway_speeds = as.data.frame(table(lit_eng_res_highway_parsed$maxspeed))

# Find highway="residential" maxspeed combinations
good_res = eng_res_highway_parsed %>% dplyr::filter(maxspeed <= 30)
too_fast_res = eng_res_highway_parsed %>% dplyr::filter(maxspeed > 30)
too_fast_res_lit = lit_eng_res_highway_parsed %>% dplyr::filter(maxspeed > 30)
good_res_lit = lit_eng_res_highway_parsed %>% dplyr::filter(maxspeed <= 30)
  
# We can see that there are 2,610 ways tagged highway="residential" in England that
# have a maxspeed value exceeding 30 mph. Thus, it is probably not a safe assumption
# to automatically assign highway=residential as 30 mph, though this is expected given 
# the extensive range of highway=residential across urban and rural areas

# However, a safer assumption may be the tagging of highway="residential" ways that
# are lit. By definition, any residential street that is lit should have a maxspeed of
# 30 mph, unless changed by a local order. 

# From `too_fast_res_lit`, we observe the number of highway=residential ways that 
# do have lighting and a maxspeed exceeding 30 mph. Specifically 40, 50 and 60. The
# likely result of this are new residential builds springing up off of the side of
# primary roads possessing that speed limit. 
# For an example, please see https://user-images.githubusercontent.com/58815827/194595370-049dcb4c-e220-49e0-ab7a-9e693407b41a.png

# Given discussions within #147 (udsleeds/openinfra), it is reasonable to make the 
# assumption that cases described in the previous paragraph will have correct
# maxspeed values entered to reflect their exception from the norm, that is that
# a 30 mph speed limit should apply. 

# Noting the obvious exception that similar areas to those described two paragraphs
# above WILL exist, and WILL NOT have been mapped with an appropriate, extraordinary
# maxspeed value, at least yet..., it would be reasonable to infer a maximum speed of
# 30 mph on ways tagged highway=residential, lit=yes & maxspeed=NA to be added as a 
# SEPERATE column to the data packs (clearly stating this is an inferred speed, not input).

# As time passes, OSM will be updated with correct maxspeed values as these areas 
# become mapped, at which point the maxspeed==NA condition (to infer a maxspeed)
# will not be met, and the OSM entered maxspeed value will instead prevail and no
# maxspeed needs to be inferred.


