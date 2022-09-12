# Create example data pack for Leeds! 


# Setup -------------------------------------------------------------------

#remotes::install_github("udsleeds/openinfra")
# Library Imports
pkgs = c("sf",
         "osmextract",
         "tidyverse",
         "tmap",
         "openinfra")
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]
# # Uncomment to create new release:
# piggyback::pb_new_release(tag = "0.3")

# Key parameters ----------------------------------------------------------

region_name = "leeds"

#required_tags = c(colnames(openinfra::sotm_data))
required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                  "kerb", "footway", "sidewalk", "cycleway", "segregated", 
                  "highway", "crossing", "lit", "tactile_paving", "surface", 
                  "smoothness", "width", "est_width", "lit_by_led", "ref", 
                  "amenity", "sidewalk", "sidewalk:left", "sidewalk:right", 
                  "sidewalk:both", "source:maxspeed", "maxspeed:type", 
                  "zone:maxspeed", "zone:traffic", "maxspeed", "HFCS", "rural")

# Data acquisition ---------------------------------------------------------

# Set up buffer
leeds_centre_point = sf::st_sfc(sf::st_point(c(-1.549, 53.801)), 
                              crs = "EPSG:4326")
leeds_buffer = sf::st_buffer(leeds_centre_point, dist = 2000) #2000 metre radius

# Download lines network
leeds_lines_network = oe_get(
  place = region_name,
  layer = "lines",
  extra_tags = required_tags,
  boundary = leeds_buffer,
  boundary_type = "clipsrc",
  force_download = TRUE
)
# Remove NA highways (waterways, railways, aerialways, etc.)
leeds_lines_network = leeds_lines_network %>% dplyr::filter(! is.na(highway))

# Download points network. 
leeds_pois_network = osmextract::oe_get(
  place= region_name,
  layer = "points",
  extra_tags = required_tags,
  boundary = leeds_buffer,
  boundary_type = "clipsrc",
  force_download = TRUE,
  never_skip_vectortranslate = TRUE
)
# Data processing ---------------------------------------------------------
lines_network = leeds_lines_network
points_network = leeds_pois_network

# GeoJson of Leeds network obtained with osmextract 2/07/2022.
# TODO: upload the data used here to releases so this can be reproducible.  
#a_test_network = sf::read_sf("https://github.com/udsleeds/openinfra/releases/download/v0.2/Leeds.geojson")


# Create data packs. ------------------------------------------------------

# Lines networks below.
active_cycle_pack = oi_active_cycle(lines_network, remove=FALSE)
active_walk_pack = oi_active_walk(lines_network, remove=FALSE)
recode_road_pack = oi_recode_road_class(lines_network, del=TRUE)
is_lit_pack = oi_is_lit(lines_network, remove = FALSE)
clean_maxspeed_pack = oi_clean_maxspeed_uk(lines_network, no_NA=FALSE, del=FALSE)
road_names_pack = oi_road_names(lines_network, remove=TRUE)
cycle_crossings_pack = oi_cycle_crossings(lines_network, remove=TRUE)
IM_pack = oi_inclusive_mobility(lines_network)
#TODO: add below functions to data pack examples
cycle_infra_pack = oi_cycle_separation(lines_network)

# Points networks below
cycle_parking_pack = oi_bicycle_parking(points_network, remove=TRUE)

# TODO: review if this can be deleted
# old code  ---------------------------------------------------------------

# Select relevant columns for data_pack
# test_network_datapack = a_test_network %>% dplyr::select(c(
#   "osm_id", "highway", "road_desc", "oi_maxspeed", "oi_walk", "oi_cycle",
#   "oi_is_lit", "im_kerb", "im_footway", "im_footpath", "im_crossing", 
#   "im_footway_imp", "im_light", "im_tactile", "im_surface_paved", "im_surface",
#   "im_width", "im_width_est")
# )
#a_test_network = a_test_network %>%
#  select(osm_id, highway, matches(match = "oi_|im_"))
#a_test_network = sf::st_sf(
#  a_test_network %>% sf::st_drop_geometry(),
#  geometry = a_test_network$geometry
#)
#names(a_test_network)

# Upload data -------------------------------------------------------------

#data_pack_basename = paste0("datapack_", region_name)
#data_pack_basename
#formats = c(".geojson", ".gpkg")
#for (f in formats) {
#  data_pack_filename = paste0(data_pack_basename, f)
#  message("Writing data for ", region_name, ": ", data_pack_filename)
#  sf::write_sf(a_test_network, data_pack_filename)
#  message("Uploading data for ", region_name, ": ", data_pack_filename)
#  piggyback::pb_upload(data_pack_filename)
#}
# create shapefile (not by default)
#data_pack_filename_shp = paste0(data_pack_basename, ".shp")
#dir.create(paste0(data_pack_basename, "_shp"))
#sf::write_sf(a_test_network, file.path(paste0(data_pack_basename, "_shp"), data_pack_filename_shp))
#waldo::compare(names(a_test_network), names(a_test_shp))
#a_test_shp = sf::read_sf("datapack_leeds_shp/datapack_leeds.shp")
#zip(zipfile = paste0(data_pack_basename, ".zip"), files = paste0(data_pack_basename, "_shp"))
#piggyback::pb_upload(paste0(data_pack_basename, ".zip"))


# Create example plots ----------------------------------------------------

#___________MAPS_____________________________
#0 Default OSM highway values
default_OSM_highways = tmap::tm_shape(lines_network) + 
  tmap::tm_lines(col = "highway")
tmap::tmap_save(default_OSM_highways, "/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/deault_OSM_highways.html")

#1
active_cycle_map = tmap::tm_shape(active_cycle_pack) + 
  tmap::tm_lines(col = "openinfra_cycle")
tmap::tmap_save(active_cycle_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/active_cycle_map.html')

#2
active_walk_map = tmap::tm_shape(active_walk_pack) + 
  tmap::tm_lines(col = "openinfra_walk")
tmap::tmap_save(active_walk_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/active_walk_map.html')

#3
road_desc_map = tmap::tm_shape(recode_road_pack) + 
  tmap::tm_lines(col = "openinfra_road_desc")
tmap::tmap_save(road_desc_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/road_desc_map.html')

#4
is_lit_map = tmap::tm_shape(is_lit_pack) + 
  tmap::tm_lines(col = "openinfra_is_lit")
tmap::tmap_save(is_lit_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/is_lit_map.html')

#5
clean_maxspeed_map = tmap::tm_shape(clean_maxspeed_pack) + 
  tmap::tm_lines(col = "openinfra_maxspeed")
tmap::tmap_save(clean_maxspeed_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/clean_maxspeed_map.html')

#6
road_names_map = tmap::tm_shape(road_names_pack) + 
  tmap::tm_lines(col = "openinfra_road_name")
tmap::tmap_save(road_names_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/road_names_map.html')

#7
cycle_crossings_map = tmap::tm_shape(cycle_crossings_pack) + 
  tmap::tm_lines(col = "openinfra_cycle_crossings")
tmap::tmap_save(cycle_crossings_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/cycle_crossings_map.html')

#8
im_map = tmap::tm_shape(IM_pack %>% dplyr::select(c("openinfra_im_kerb", "openinfra_im_footway", "openinfra_im_footpath", "openinfra_im_crossing", 
                                                                  "openinfra_im_footway_imp", "openinfra_im_light", "openinfra_im_tactile", "openinfra_im_surface_paved", "openinfra_im_surface",
                                                                  "openinfra_im_width", "openinfra_im_width_est"))) + 
  tmap::tm_lines()
tmap::tmap_save(im_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/im_map.html')

#9 
cycle_parking_map = tmap::tm_shape(cycle_parking_pack) + 
  tmap::tm_dots(col = "openinfra_cycle_parking")
tmap::tmap_save(cycle_parking_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/cycle_parking_map.html')

# 10
cycle_infra_map = tmap::tm_shape()


# Load maps into R --------------------------------------------------------
# Lines 
active_cycle_map
active_walk_map
road_desc_map
is_lit_map
clean_maxspeed_map
road_names_map
cycle_crossings_map
im_map

# Points
cycle_parking_map

