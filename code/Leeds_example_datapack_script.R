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
                  "zone:maxspeed", "zone:traffic", "maxspeed", "HFCS", "rural",
                  "cycleway_left", "cycleway_right", "cycleway_both",
                  "separation")

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
# TODO : Update lines & pois network to contain new required tags. 
# Load reproducible example
#leeds_lines_network = sf::read_sf(paste0("https://github.com/udsleeds/openinfr",
#                                         "a/releases/download/0.4.2/leeds_line",
#                                         "s_network.geojson"))

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

# Load reproducible example
#leeds_pois_network = sf::read_sf(paste0("https://github.com/udsleeds/openinfra",
#                                        "/releases/download/0.4.2/leeds_points",
#                                        "_network.geojson"))
# GeoJsons of Leeds networks obtained with osmextract 28/08/2022.

# Data processing ---------------------------------------------------------
# Remove NA highways (waterways, railways, aerialways, etc.)
leeds_lines_network = leeds_lines_network %>% dplyr::filter(! is.na(highway))

lines_network = leeds_lines_network
points_network = leeds_pois_network


# Create data packs. ------------------------------------------------------

# Lines networks below.
active_cycle_pack = oi_active_cycle(lines_network, remove=FALSE)
active_walk_pack = oi_active_walk(lines_network, remove=FALSE)
recode_road_pack = oi_recode_road_class(lines_network, del=TRUE)
is_lit_pack = oi_is_lit(lines_network, remove = FALSE)
clean_maxspeed_pack = oi_clean_maxspeed_uk(lines_network, no_NA=FALSE, del=FALSE)
road_names_pack = oi_road_names(lines_network, remove=TRUE)
cycle_crossings_pack = oi_cycle_crossings(lines_network, remove=TRUE)
im_flush_kerb_pack = oi_im_flush_kerb(lines_network)
im_pavement_widths = oi_im_pavement_width(lines_network)
im_pedestrian_infra = oi_im_pedestrian_infra(lines_network)
im_surfaces = oi_im_surfaces(lines_network)
im_tactile_paving = oi_im_tactile_paving(lines_network)
cycle_infra_pack = oi_cycle_separation(lines_network, remove=TRUE)

# Points networks below
cycle_parking_pack = oi_bicycle_parking(points_network, remove=TRUE)

# TODO: review if this can be deleted
# old code  ---------------------------------------------------------------

# Select relevant columns for data_pack
#a_test_network = a_test_network %>%
#  select(osm_id, highway, matches(match = "oi_|im_"))

# Put geometry column at the end of the data.frame - good sf practice. 
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


# Create example plots for data_packs.Rmd ---------------------------------
dir_path = "/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/"
f = ".html"

#___________MAPS_____________________________
# 0 Default OSM highway values
default_OSM_highways = tmap::tm_shape(lines_network) + 
  tmap::tm_lines(col = "highway")
tmap::tmap_save(default_OSM_highways, paste0(dir_path, "deault_OSM_highways", f))

# 1
active_cycle_map = tmap::tm_shape(active_cycle_pack) + 
  tmap::tm_lines(col = "openinfra_cycle")
tmap::tmap_save(active_cycle_map, paste0(dir_path, "active_cycle_map", f))

# 2
active_walk_map = tmap::tm_shape(active_walk_pack) + 
  tmap::tm_lines(col = "openinfra_walk")
tmap::tmap_save(active_walk_map, paste0(dir_path, "active_walk_map", f))

# 3
road_desc_map = tmap::tm_shape(recode_road_pack) + 
  tmap::tm_lines(col = "openinfra_road_desc")
tmap::tmap_save(road_desc_map, paste0(dir_path, "road_desc_map", f))

# 4
is_lit_map = tmap::tm_shape(is_lit_pack) + 
  tmap::tm_lines(col = "openinfra_is_lit")
tmap::tmap_save(is_lit_map, paste0(dir_path, "is_lit_map", f))

# 5 - Remove maxspeed == NA to stop "Missing" in map legend
clean_maxspeed_pack = clean_maxspeed_pack %>% dplyr::filter(! is.na(openinfra_maxspeed))
clean_maxspeed_map = tmap::tm_shape(clean_maxspeed_pack) + 
  tmap::tm_lines(col = "openinfra_maxspeed")
tmap::tmap_save(clean_maxspeed_map, paste0(dir_path, "clean_maxspeed_map", f))

# 6
road_names_map = tmap::tm_shape(road_names_pack) + 
  tmap::tm_lines(col = "openinfra_road_name")
tmap::tmap_save(road_names_map, paste0(dir_path, "road_names_map", f))

# 7
cycle_crossings_map = tmap::tm_shape(cycle_crossings_pack) + 
  tmap::tm_lines(col = "openinfra_cycle_crossings")
tmap::tmap_save(cycle_crossings_map, paste0(dir_path, "cycle_crossings_map", f))

im_kerb_map = tmap::tm_shape(im_flush_kerb_pack %>% 
                               dplyr::select(osm_id, highway,
                                             matches(match = "openinfra_im_"))) + 
  tmap::tm_lines()
tmap::tmap_save(im_kerb_map, paste0(dir_path, "im_kerb_map", f))

# 8.2
im_pavements_map = tmap::tm_shape(im_pavements_pack %>% 
                                     dplyr::select(osm_id, highway,
                                                   matches(match = "openinfra_im_"))) + 
  tmap::tm_lines()
tmap::tmap_save(im_pavements_map, paste0(dir_path, "im_pavements_map", f))

#8.3
im_ped_infra_map = tmap::tm_shape(im_pedestrian_infra %>% 
                                    dplyr::select(osm_id, highway, 
                                                  matches(match = "openinfra_im_"))) +
  tmap::tm_lines()
tmap::tmap_save(im_ped_infra_map, paste0(dir_path, "im_ped_infra_map", f))

#8.4
im_surfaces_map = tmap::tm_shape(im_surfaces %>% 
                                   dplyr::select(osm_id, highway,
                                                 matches(match = "openinfra_im_"))) +
  tmap::tm_lines()
tmap::tmap_save(im_surfaces_map, paste0(dir_path, "im_surfaces_map", f))

#8.5
im_tactile_paving_map = tmap::tm_shape(im_tactile_paving %>% 
                                         dplyr::select(osm_id, highway, 
                                                       matches(match = "openinfra_im_"))) +
  tmap::tm_lines()
tmap::tmap_save(im_tactile_paving_map, paste0(dir_path, "im_tactile_paving_map", f))

# 9 
cycle_parking_map = tmap::tm_shape(cycle_parking_pack) + 
  tmap::tm_dots(col = "openinfra_cycle_parking")
tmap::tmap_save(cycle_parking_map, paste0(dir_path, "cycle_parking_map" ,f))

# 10
cycle_infra_map = tmap::tm_shape(cycle_infra_pack) + 
  tmap::tm_lines(col = "openinfra_cycle_infra")
tmap::tmap_save(cycle_infra_map, paste0(dir_path, "cycle_infra_map", f))


# Load maps into R --------------------------------------------------------
# Lines
# 1
active_cycle_map
# 2
active_walk_map
# 3
road_desc_map
# 4
is_lit_map
# 5
clean_maxspeed_map
# 6
road_names_map
# 7
cycle_crossings_map

# 8.1
im_kerb_map
# 8.2
im_pavements_map
# 8.3
im_ped_infra_map
# 8.4
im_surfaces_map
# 8.5
im_tactile_paving_map

# 10
cycle_infra_map
#591 --> 655

# Points
# 9
cycle_parking_map

