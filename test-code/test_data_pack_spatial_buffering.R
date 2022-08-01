# This script tests the functionality of spatially sub-setting an osmextract OSM
# transport infrastructure dataset based on the bounding polygons of local TAs
# within England. 

pkgs = c("sf",
         "osmextract",
         "dplyr",
         "tmap")

lapply(pkgs, library, character.only = TRUE)[length(pkgs)]

LADs = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/lads_joined_2021.geojson")

stroud = LADs %>% dplyr::filter(LADs$LAD21NM == "Stroud")

stroud_poly = stroud %>% select(geometry)

# Request data using osmextract
all_extra_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                   "kerb", "footway", "sidewalk", "cycleway", "segregated", "highway",
                   "crossing", "lit", "tactile_paving", "surface", "smoothness", "width",
                   "est_width", "lit_by_led")

network = osmextract::oe_get(stroud_poly, extra_tags = all_extra_tags)

# Remove highway == NA (train tracks,  waterways etc.)
network = network %>% dplyr::filter(! is.na(highway))

# Subset the data using LAD polygon
small_network = network[stroud, ]

# Now we have our network area defined - apply openinfra functions!
devtools::load_all() # Load openinfra (locally)

stroud_road_class = openinfra::recode_road_class(small_network)
stroud_max_speed = openinfra::oi_clean_maxspeed_uk(small_network, no_NA = TRUE)
stroud_lighting = openinfra::oi_is_lit(small_network, remove = TRUE)
stroud_walking = openinfra::oi_active_walk(small_network, remove = FALSE)
stroud_cycling = openinfra::oi_active_cycle(small_network, remove = FALSE)
stroud_IM = openinfra::oi_inclusive_mobility(small_network)


# Visualise our current output(s)!
map =tmap::tm_shape(small_network) + 
  tmap::tm_lines(col = "highway", col.title = "Highway type") +
  tmap::tm_shape(stroud_poly) +
    tmap::tm_polygons(alpha = 0.45) + 
  tmap::tm_shape(stroud_road_class) + 
    tmap::tm_lines(col = "road_desc") + 
  tmap::tm_shape(stroud_max_speed) + 
    tmap::tm_lines(col = "oi_maxspeed") + 
  tmap::tm_shape(stroud_lighting)  + 
    tmap::tm_lines(col = "oi_is_lit") + 
  tmap::tm_shape(stroud_walking) + 
    tmap::tm_lines(col = "oi_walk") + 
  tmap::tm_shape(stroud_cycling) + 
    tmap::tm_lines(col = "oi_cycle") + 
  tmap::tm_shape(stroud_IM) + 
    tmap::tm_lines()
# View the map
map
# Save the map
#tmap::tmap_save(map , "stroud_data_pack_example.html")