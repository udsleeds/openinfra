# Create example data pack for Leeds! 


# Setup -------------------------------------------------------------------

remotes::install_github("udsleeds/openinfra")
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
release_tag = "0.3"

# GeoJson of Leeds network obtained with osmextract 2/07/2022.
a_test_network = sf::read_sf("https://github.com/udsleeds/openinfra/releases/download/v0.2/Leeds.geojson")

# Apply Openinfra functions to create datapack
a_test_network = oi_active_cycle(a_test_network, remove = FALSE)
a_test_network = oi_active_walk(a_test_network, remove = FALSE)
a_test_network = oi_clean_maxspeed_uk(a_test_network, no_NA = FALSE, del = FALSE)
a_test_network = oi_inclusive_mobility(a_test_network)
a_test_network = oi_is_lit(a_test_network, remove = FALSE)
a_test_network = recode_road_class(a_test_network)

# Select relevant columns for data_pack
# test_network_datapack = a_test_network %>% dplyr::select(c(
#   "osm_id", "highway", "road_desc", "oi_maxspeed", "oi_walk", "oi_cycle",
#   "oi_is_lit", "im_kerb", "im_footway", "im_footpath", "im_crossing", 
#   "im_footway_imp", "im_light", "im_tactile", "im_surface_paved", "im_surface",
#   "im_width", "im_width_est")
# )
a_test_network = a_test_network %>%
  select(osm_id, highway, matches(match = "oi_|im_"))
a_test_network = sf::st_sf(
  a_test_network %>% sf::st_drop_geometry(),
  geometry = a_test_network$geometry
)
names(a_test_network)

# Upload data -------------------------------------------------------------

data_pack_basename = paste0("datapack_", region_name)
data_pack_basename
formats = c(".geojson", ".gpkg")
for (f in formats) {
  data_pack_filename = paste0(data_pack_basename, f)
  message("Writing data for ", region_name, ": ", data_pack_filename)
  sf::write_sf(a_test_network, data_pack_filename)
  message("Uploading data for ", region_name, ": ", data_pack_filename)
  piggyback::pb_upload(data_pack_filename)
}
# create shapefile (not by default)
data_pack_filename_shp = paste0(data_pack_basename, ".shp")
dir.create(paste0(data_pack_basename, "_shp"))
sf::write_sf(a_test_network, file.path(paste0(data_pack_basename, "_shp"), data_pack_filename_shp))
waldo::compare(names(a_test_network), names(a_test_shp))
a_test_shp = sf::read_sf("datapack_leeds_shp/datapack_leeds.shp")
zip(zipfile = paste0(data_pack_basename, ".zip"), files = paste0(data_pack_basename, "_shp"))
piggyback::pb_upload(paste0(data_pack_basename, ".zip"))

#___________MAPS_____________________________

cycle_map = tmap::tm_shape(test_network_datapack) + 
  tmap::tm_lines(col = "oi_cycle")
#tmap::tmap_save(cycle_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/cycle_map.html')

walk_map = tmap::tm_shape(test_network_datapack) + 
  tmap::tm_lines(col = "oi_walk")
#tmap::tmap_save(walk_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/walk_map.html')

maxspeed_map = tmap::tm_shape(test_network_datapack) + 
  tmap::tm_lines(col = "oi_maxspeed")
#tmap::tmap_save(maxspeed_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/maxspeed_map.html')

im_map = tmap::tm_shape(test_network_datapack %>% dplyr::select(c("im_kerb", "im_footway", "im_footpath", "im_crossing", 
                                                                  "im_footway_imp", "im_light", "im_tactile", "im_surface_paved", "im_surface",
                                                                  "im_width", "im_width_est"))) + 
  tmap::tm_lines()
#tmap::tmap_save(im_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/im_map.html')

is_lit_map = tmap::tm_shape(test_network_datapack) + 
  tmap::tm_lines(col = "oi_is_lit")
#tmap::tmap_save(is_lit_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/is_lit_map.html')

road_desc_map = tmap::tm_shape(test_network_datapack) + 
  tmap::tm_lines(col = "road_desc")
#tmap::tmap_save(road_desc_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/road_desc_map.html')