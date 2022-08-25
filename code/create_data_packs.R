################################################################################
# Info  -------------------------------------------------------------------    #
# This script contains the workflow that generates transport infrastructure    #
# data packs for each Local Authority District (LAD) within England - we look  #
# to update this workflow to also contain the LADs of Scotland and potentially #
# Norther Ireland (NI) provided tags here are consistent with the rest of the  #
# United Kingdom (UK).                                                         #
#                                                                              #
# Workflow proposal:                                                           #
# 1 - Download England-latest.osm.pbf and store locally                        #
#                                                                              #
# 2 - For each LAD, get default OSM networks by subsetting from England-latest #
#     using oe_read(), pointing to local England-latest as input file. Save    #
#     each LAD network as a .geojson                                           #
#                                                                              #
# 3 - For each LAD network, read the .geojson and load into R, then apply the  #
#     openinfra functions to create a data pack for that LAD                   #
#                                                                              #
# 4 - Write the data pack locally as both .geojson and .gpkg                   #
#                                                                              #
# 5 - Upload the local data packs to github releases using piggyback package   #
#                                                                              #
################################################################################

# Key parameters  ---------------------------------------------------------

overwrite_network = FALSE             # Overwrite existing network geojsons? 
overwrite_datapack = FALSE            # Overwrite existing data pack geojsons? 
local_save_data_pack = TRUE           # Save data packs locally?  
piggyback_data_packs = TRUE           # Upload data packs to releases? 
save_formats = c(".geojson", ".gpkg") # Data pack file formats
release_tag = "0.4"                   # Releases tag for piggyback
creation_date = "25_08_2022"          # Date of download for england-latest.osm

# File path or URL to LAD bounding polygons
LAD_polygons_path = paste0("https://github.com/udsleeds/openinfra/raw/",
                           "main/data-small/lads_joined_2021.geojson")
# File path to store default OSM geojson networks
network_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/openinfra",
                     "/data_pack_networks/")
# File path to store default OSM geojson networks
data_pack_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                       "openinfra/data_packs/")

if (piggyback_data_packs){
  # If uploading, local save must be true as pigggyback needs a local 
  # file to upload
  local_save_data_pack = TRUE 
}

# Library installs --------------------------------------------------------
# Run this section if you are missing any of the required libraries from
# within Library imports. 

pkgs = c("remotes",
         "sf",
         "dplyr",
         "tidyverse",
         "piggyback")

for (pkg in pkgs){
  if (pkg %in% installed.packages()){
    # Do nothing - package is installed
  } else {
    # Package not installed.
    message(pkg, "is not installed - installing now...")
    install.packages(pkg)
  }
}

# Check if osmextract dev is installed - install if not
if (!("osmextract" %in% installed.packages())){
  remotes::install_github("ropensci/osmextract")
} 

# Check if openinfradev is installed - install if not
if (!("openinfra" %in% installed.packages())){
  remotes::install_github("udsleeds/openinfra")
}

# Remove remotes from pkgs - not needed after installing dev packages above
pkgs = pkgs[! pkgs %in% "remotes"]

# Library imports ---------------------------------------------------------
pkgs = c(pkgs, "osmextract", "openinfra")
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]

# Create required directories ---------------------------------------------
#Create directory for current date to store england-latest.osm.pbf
dir.create(paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                  "eng_osm_downloads/", creation_date))
# Create dir for current date to store default OSM networks from eng-latest.osm
dir.create(paste0(network_dir, creation_date))
# Create dir for current date to store data packs created from default networks
dir.create(paste0(data_pack_dir, creation_date))

# Get required data -------------------------------------------------------

# Tags required to make data packs
required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                  "kerb", "footway", "sidewalk", "cycleway", "segregated", 
                  "highway", "crossing", "lit", "tactile_paving", "surface", 
                  "smoothness", "width", "est_width", "lit_by_led", "boundary",
                  "admin_level", "name", "ref")


# Download & save OSM data
dl_eng_latest = oe_get(
  place = "England",
  extra_tags = required_tags,
  download_only = TRUE,
  skip_vectortranslate = TRUE,
  download_directory = paste0("/home/james/Desktop/LIDA_OSM_Project/",
                              "openinfra/eng_osm_downloads/", creation_date)
)

# File path to the downloaded (above) england-latest.osm.pbf
eng_latest_fp = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                       "eng_osm_downloads/", creation_date,
                       "/geofabrik_england-latest.osm.pbf")

# Download LAD boundary polygons
LADs = sf::read_sf(LAD_polygons_path)
LADs = sf::st_make_valid(LADs)
LADs = LADs[1:25, ] # only use first 5 whilst testing. (comment out when done)
LAD_polys = LADs %>% dplyr::select(c("LAD21NM", "geometry"))

# Create & save default OSM networks per LAD -------------------------------

# Specify default parsing requirements 
translate_options = c(
  "-nlt", "PROMOTE_TO_MULTI",       # Check this
  "-where", "highway IS NOT NULL")  # Highway cannot be NA

# Specify file path to save the default networks.
network_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                     "openinfra/data_pack_networks/", creation_date, "/")

message(c("Creating data packs now ", format(Sys.time(), "%a %b %d %X %Y")))

# Iterates through all LADs, saving a geojson of each default OSM network. 
for (region_name in LADs$LAD21NM){
  # Generate file name
  filename = paste0(gsub(" ", "_", region_name), ".geojson")
  
  # Check if a network exists for current region: overwrite or not 
  if (filename %in% list.files(network_dir)) {
    # In this instance the network exists in network_dir - what to do next...
    
    if (overwrite_network){
      # overwrite the existing network...
      message(region_name, " network found - overwrite is TRUE so re-writing")
      # (Do nothing here, network will be overwritten by code below)
    
      } else{
      # Don't overwrite network, move to the next region.
      message(paste("Skipping:", region_name,
                    "as network .geojson already exists"))
      next
    }
  } else {
    # The network geojson does not exist
    message(paste(region_name, "network not found - creating now."))
  }
  
  # Get current LAD polygon
  subset_poly = LAD_polys %>% 
    dplyr::filter(LAD_polys$LAD21NM == region_name) %>% 
    dplyr::select("geometry")
  # Check if polygon is invalid, fixes if it is.
  subet_poly = sf::st_make_valid(subset_poly)
  
  # Get network by sub setting England.osm.pbf, save output as geojson.
  message("Reading for: ", region_name)
  network = oe_read(
    eng_latest_fp,
    vectortranslate_options = translate_options,
    extra_tags = required_tags,
    boundary = subset_poly,
    boundary_type = "clipsrc",
    quiet = TRUE
  )
  
  # Save LAD network as geojson 
  sf::st_write(network, paste0(network_dir, filename),
               append = FALSE, quiet = TRUE)
  
  # Print status
  message(c(region_name, "network created @ ",
            format(Sys.time(), "%a %b %d %X %Y")))
}


# Generate infrastructure data packs --------------------------------------
# Go through stored networks, apply Openinfra functions and create data packs. 

network_files = list.files(network_dir) # Load stored files
# Dir. to store data packs (Note we can just use piggyback if u don't want to 
#                           store these locally - maybe add boolean check ?)
data_pack_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                       "openinfra/data_packs/", creation_date, "/")

# For all network files, generate a data pack.
for (network_filename in network_files[1:25]){
  # Load current LAD network
  region_name = gsub(".geojson", "", network_filename)
  message(paste0("Opening file: ", network_filename, " @ ", 
                 format(Sys.time(), "%a %b %d %X %Y")))
  network = sf::read_sf(paste0(network_dir, network_filename))
  
  # Check if this data pack already exists
  if (network_filename %in% substring(gsub("_data_pack", "", list.files(data_pack_dir)), 12 )){
    # In this instance the data pack already exists - what to do next... ?
    if (overwrite_datapack){
      # Delete data pack .geojson
      unlink(paste0(creation_date,"_datapack_",network_filename)) 
      # Delete data pack .gpkg
      unlink(paste0(creation_date,"_datapack_",
                    sub(".geojson", ".gpkg", network_filename)))
      # Nothing else - code below will re-write new data pack.
    } else {
      # Don't overwrite the data pack, move onto the next region
      message(paste0("Error:" , region_name, 
                     " already has a data pack, skipping region."))
      next
    }
  }
  message(paste(region_name, "data pack missing - creating now."))
 
  # Apply all openinfra functions to network (create the data pack)
  network_data_pack = oi_active_cycle(network, remove = FALSE)
  network_data_pack = oi_active_walk(network_data_pack, remove = FALSE)
  network_data_pack = oi_clean_maxspeed_uk(network_data_pack, no_NA = FALSE, del = FALSE)
  network_data_pack = oi_inclusive_mobility(network_data_pack)
  network_data_pack = oi_is_lit(network_data_pack, remove = FALSE)
  network_data_pack = oi_recode_road_class(network_data_pack, del = FALSE)
  network_data_pack = oi_road_names(network_data_pack)
  
  # Select relevant columns for data packs
  network_data_pack = network_data_pack %>%
    select(osm_id, highway, matches(match = "oi_|im_"))
  # Put geometry column at the end (good sf practice)
  network_data_pack = sf::st_sf( network_data_pack %>%
                                   sf::st_drop_geometry(),
                                 geometry = network_data_pack$geometry)
  
  # Save data packs locally
  if (local_save_data_pack) {
    for (f in save_formats){
      #region_name
      data_pack_filename = paste0(creation_date, "_", "datapack_",
                                  gsub(".geojson", "", network_filename), f)
      message("Writing data pack for: ", region_name, "with format: ", f)
      sf::st_write(network_data_pack, paste0(data_pack_dir, data_pack_filename), append = FALSE) 
    }
  }
  # Upload data packs with piggyback
  if (piggyback_data_packs){
    for (f in save_formats){
      data_pack_filename = paste0(creation_date, "_", "datapack_",
                                  gsub(".geojson", "", network_filename), f)
      message("Uploading data pack for: ", region_name, "with format: ", f)
      piggyback::pb_upload(paste0(data_pack_dir, data_pack_filename), tag = release_tag)
    }
  }
}

# Save/Upload data pack as shapefile ? (NOT by default) ------------------------
#data_pack_filename_shp = paste0(data_pack_basename, ".shp")
#dir.create(paste0(data_pack_basename, "_shp"))
#sf::write_sf(a_test_network, 
#             file.path(paste0(data_pack_basename, "_shp"), 
#             data_pack_filename_shp))
#waldo::compare(names(a_test_network), names(a_test_shp))
#a_test_shp = sf::read_sf("datapack_leeds_shp/datapack_leeds.shp")
#zip(zipfile = paste0(data_pack_basename, ".zip"),
#    files = paste0(data_pack_basename, "_shp"))
#piggyback::pb_upload(paste0(data_pack_basename, ".zip"))