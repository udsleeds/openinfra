# Script testing (and maybe doing) the workflow to be used in the generation
# and upkeep of the openinfra transport infrastructure data packs. 


# ---- Workflow Idea ----
# 1- Download England.psm.pbf
# 2- For each LAD, create a network by subsetting each LAD from England.osm
# 3- for each LAD network, apply each of the openinfra functions

# Library Imports
pkgs = c("sf",
         "osmextract",
         "dplyr",
         "tmap")
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]

# Request England data using osmextract
required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                   "kerb", "footway", "sidewalk", "cycleway", "segregated", "highway",
                   "crossing", "lit", "tactile_paving", "surface", "smoothness", "width",
                   "est_width", "lit_by_led", "boundary", "admin_level", "name", "ref")

england = osmextract::oe_get(
  place = "England",
  extra_tags = required_tags,
  download_only = TRUE,
  skip_vectortranslate = TRUE
)

# Obtain LADs 
LADs = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/lads_joined_2021.geojson")

LADs_test = LADs[1:20, ] #Use only first 20 rows whilst testing functionality.
LADs = LADs[50:60, ] # Test creation using only 10 

LAD_polys = LADs %>% dplyr::select(c("LAD21NM", "geometry"))

LAD_networks = LAD_polys %>% dplyr::select("LAD21NM") 
LAD_networks = LAD_networks %>% dplyr::mutate(network = NA)

translate_options = c(
  "-nlt", "PROMOTE_TO_MULTI",
  "-where", "highway IS NOT NULL"
)

# Checks if network already exists! 
network_dir = "/home/james/Desktop/LIDA_OSM_Project/openinfra/openinfra/data_pack_networks/" 

print(c("Creating data packs now", format(Sys.time(), "%a %b %d %X %Y")))

for (name in LADs$LAD21NM){
  # Generate a file name
  filename = paste0(gsub(" ", "_", name), ".geojson")
  
  # Check if a network exists for current name: 
  if (filename %in% list.files(network_dir)) {
    # In this instance the network has already been made, so skip to next LAD
    print(paste("Error:", name, "already exists - moving on."))
    next
  }
  print(paste(name, "network not found - creating now."))
  
  # Get current LAD polygon
  subset_poly = LAD_polys %>% dplyr::filter(LAD_polys$LAD21NM == name) %>% dplyr::select("geometry")
  # Check if polygon is invalid, and fixes if it is.
  subet_poly = sf::st_make_valid(subset_poly)
  
  # Get network by sub setting England.osm.pbf, save output as geojson.
  network = oe_get(
    "England", 
    vectortranslate_options = translate_options,
    extra_tags = required_tags,
    boundary = subset_poly,
    boundary_type = "clipsrc",
    quiet = TRUE)
  
  # Store/Save network
  # STORE
  #LAD_networks$network[LAD_networks$LAD21NM == name] = list(network)
  # SAVE
  sf::st_write(network, paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/openinfra/data_pack_networks/", filename))
  
  # Print status
  print(c(name, "network created", format(Sys.time(), "%a %b %d %X %Y")))
}

# Next we must go through the stored networks, apply Openinfra functions and 
# create the data pack outputs.
network_dir = "/home/james/Desktop/LIDA_OSM_Project/openinfra/openinfra/data_pack_networks/" 
network_files = list.files(network_dir)

# Load Openinfra functions
devtools::load_all()
# Directory to store data packs
data_pack_dir = "/home/james/Desktop/LIDA_OSM_Project/openinfra/openinfra/data_packs/"

# For all network files, apply openinfra functions & save data pack. 

for (filename in network_files){
  # Load current LAD network
  print(paste("Opening file:", filename, "@",format(Sys.time(), "%a %b %d %X %Y")))
  network = sf::read_sf(paste0(network_dir, filename))
  
  # Check if this data pack already exists - skip if it does. 
  if (filename %in% list.files(data_pack_dir)){
    # In this instance the data pack exists so skip to next network
    message(paste("Error:", filename, "data pack already exists - moving on."))
    next
  }
  message(paste(filename, "data pack not found - creating now."))
  # Apply all openinfra functions to network (create data pack)
  network = oi_active_cycle(network, remove = FALSE)
  network = oi_active_walk(network, remove = FALSE)
  network = oi_clean_maxspeed_uk(network, no_NA = FALSE, del = FALSE)
  network = oi_inclusive_mobility(network)
  network = oi_is_lit(network, remove = FALSE)
  network = recode_road_class(network)
  
  # Select relevant columns for data packs
  network_data_pack = network %>% dplyr::select(c(
    "osm_id", "highway", "road_desc", "oi_maxspeed", "oi_walk", "oi_cycle",
    "oi_is_lit", "im_kerb", "im_footway", "im_footpath", "im_crossing", 
    "im_footway_imp", "im_light", "im_tactile", "im_surface_paved", "im_surface",
    "im_width", "im_width_est")  
  )
  
  # Save the data packs
  filename = gsub(".geojson", "_data_pack.geojson", filename)
  sf::st_write(network_data_pack, paste0(data_pack_dir, filename))
  print(paste("Data pack created for LAD:", gsub("_data_pack.geojson","",filename), "@",format(Sys.time(), "%a %b %d %X %Y")))
}

#_________MAPSSSSS____________________________________

#cycle_map = tmap::tm_shape(test_network_datapack) + 
#  tmap::tm_lines(col = "oi_cycle")
#tmap::tmap_save(cycle_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/cycle_map.html')

#walk_map = tmap::tm_shape(test_network_datapack) + 
#  tmap::tm_lines(col = "oi_walk")
#tmap::tmap_save(walk_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/walk_map.html')

#maxspeed_map = tmap::tm_shape(test_network_datapack) + 
#  tmap::tm_lines(col = "oi_maxspeed")
#tmap::tmap_save(maxspeed_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/maxspeed_map.html')

#im_map = tmap::tm_shape(test_network_datapack %>% dplyr::select(c("im_kerb", "im_footway", "im_footpath", "im_crossing", 
#                                                                  "im_footway_imp", "im_light", "im_tactile", "im_surface_paved", "im_surface",
#                                                                  "im_width", "im_width_est"))) + 
#  tmap::tm_lines()
#tmap::tmap_save(im_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/im_map.html')

#is_lit_map = tmap::tm_shape(test_network_datapack) + 
#  tmap::tm_lines(col = "oi_is_lit")
#tmap::tmap_save(is_lit_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/is_lit_map.html')

#road_desc_map = tmap::tm_shape(test_network_datapack) + 
#  tmap::tm_lines(col = "road_desc")
#tmap::tmap_save(road_desc_map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/road_desc_map.html')
