##################################################################################
# This script contains parts of the workflow within create_data_packs.R
# Specifically, this script gets the LAD networks created within 
# create_LAD_networks.R, creates transport infrastructure data packs which are 
# then saved locally before being uploaded to releases with the piggyback 
# package.
#
################################################################################

# Key parameters  ---------------------------------------------------------

overwrite_network = FALSE             # Overwrite existing network geojsons? 
overwrite_datapack = FALSE            # Overwrite existing data pack geojsons? 
local_save_data_pack = TRUE           # Save data packs locally?  
piggyback_data_packs = TRUE           # Upload data packs to releases? 
save_formats = c(".geojson", ".gpkg") # Data pack file formats
release_tag = "0.4.2"                 # Releases tag for piggyback
creation_date = "31_08_2022"          # Date of download for england-latest.osm
lad_limit = 1:330                     # Limits number of LADs to be processed

# File path or URL to LAD bounding polygons
LAD_polygons_path = paste0("https://github.com/udsleeds/openinfra/raw/",
                           "main/data-small/lads_joined_2021.geojson")
# File path to store default OSM geojson networks of lines layer
lines_network_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra",
                           "/openinfra/data_pack_networks/lines/")
# File path to store default OSM geojson networks of points layer
points_network_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra",
                            "/openinfra/data_pack_networks/points/")
# File path to store lines data packs as geojson files
lines_data_pack_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                             "openinfra/data_packs/lines/")
# File path to store points data packs as geojson files
points_data_pack_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                              "openinfra/data_packs/points/")

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


# Create & save default OSM networks per LAD ------------------------------

# Specify default parsing requirements 
translate_options = c(
  "-nlt", "PROMOTE_TO_MULTI",       # Check this
  "-where", "highway IS NOT NULL")  # Highway cannot be NA

# Specify file path to save the default networks.
lines_network_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                           "openinfra/data_pack_networks/lines/", creation_date, "/")
points_network_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                            "openinfra/data_pack_networks/points/", creation_date, "/")



# Generate Lines layer infrastructure data packs -------------------------
message(c("Creating lines data packs now ", format(Sys.time(), "%a %b %d %X %Y")))

# Go through stored networks, apply Openinfra functions and create data packs. 
lines_network_files = list.files(lines_network_dir) # Load stored files
# Dir. to store data packs (Note we can just use piggyback if u don't want to 
#                           store these locally - maybe add boolean check ?)
lines_data_pack_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                             "openinfra/data_packs/lines/", creation_date, "/")
#network_files = network_files %>% dplyr::filter(! is.na(network_files))
# For all lines network files, generate a lines data pack.
for (network_filename in lines_network_files){
  # Check for current LAD lines network
  region_name = gsub(".geojson", "", network_filename)
  message(paste0("Checking file: ", network_filename, " @ ", 
                 format(Sys.time(), "%a %b %d %X %Y")))
  
  # Check if this data pack already exists
  if (network_filename %in% substring(gsub("_data_pack", "", list.files(lines_data_pack_dir)), 21 )){
    # In this instance the data pack already exists - what to do next... ?
    if (overwrite_datapack){
      # Delete data pack .geojson
      unlink(paste0(lines_data_pack_dir, creation_date,"_datapack_",network_filename)) 
      # Delete data pack .gpkg
      unlink(paste0(lines_data_pack_dir, creation_date,"_datapack_",
                    sub(".geojson", ".gpkg", network_filename)))
      # Code below will re-write new data pack.
    } else {
      # Don't overwrite the data pack, move onto the next region
      next
    }
  }
  message(paste(region_name, "lines data pack missing - creating now."))
  # Load the current LAD lines network
  lines_network = sf::read_sf(paste0(lines_network_dir, network_filename))
  
  # Apply all openinfra functions to network (create the data pack)
  lines_network_data_pack = oi_active_cycle(lines_network, remove = FALSE)
  lines_network_data_pack = oi_active_walk(lines_network_data_pack,
                                           remove = FALSE)
  lines_network_data_pack = oi_clean_maxspeed_uk(lines_network_data_pack,
                                                 no_NA = FALSE, del = FALSE)
  lines_network_data_pack = oi_im_flush_kerb(lines_network_data_pack)
  lines_network_data_pack = oi_im_pavement_width(lines_network_data_pack)
  lines_network_data_pack = oi_im_pedestrian_infra(lines_network_data_pack)
  lines_network_data_pack = oi_im_surfaces(lines_network_data_pack)
  lines_network_data_pack = oi_im_tactile_paving(lines_network_data_pack)
  
  lines_network_data_pack = oi_is_lit(lines_network_data_pack, remove = FALSE)
  lines_network_data_pack = oi_recode_road_class(lines_network_data_pack,
                                                 del = FALSE)
  lines_network_data_pack = oi_road_names(lines_network_data_pack)
  lines_network_data_pack = oi_bicycle_parking(lines_network_data_pack,
                                               remove = FALSE)
  lines_network_data_pack = oi_cycle_crossings(lines_network_data_pack, 
                                               remove=FALSE)
  
  # Select relevant columns for data packs
  lines_network_data_pack = lines_network_data_pack %>%
    select(osm_id, highway, matches(match = "openinfra_"))
  # Put geometry column at the end (good sf practice)
  lines_network_data_pack = sf::st_sf( lines_network_data_pack %>%
                                         sf::st_drop_geometry(),
                                       geometry = lines_network_data_pack$geometry)
  
  # Save lines data packs locally
  if (local_save_data_pack) {
    for (f in save_formats){
      #region_name
      data_pack_filename = paste0(creation_date, "_", "datapack_",
                                  gsub(".geojson", "", network_filename), f)
      message("Writing data pack for: ", region_name, " with format: ", f)
      sf::st_write(lines_network_data_pack,
                   paste0(lines_data_pack_dir,data_pack_filename),
                   append = FALSE) 
    }
  }
  
  # Upload lines data packs with piggyback 
  if (piggyback_data_packs){
    for (f in save_formats){
      data_pack_filename = paste0(creation_date, "_", "datapack_",
                                  gsub(".geojson", "", network_filename), f)
      message("Uploading data pack for: ", region_name, " with format: ", f)
      
      ############change file name for upload to releases#######################
      current_filename = paste0(lines_data_pack_dir, data_pack_filename)
      upload_filename = gsub("datapack", "lines_datapack", current_filename)
      file.rename(current_filename, upload_filename)
      ##########################################################################
      
      #piggyback::pb_upload(paste0(lines_data_pack_dir, data_pack_filename), 
      #                     tag = release_tag)
      piggyback::pb_upload(upload_filename, tag = release_tag)
      file.rename(upload_filename, current_filename) # Change filename back 
    }
  }
}



# Generate points layer infrastructure data packs -------------------------

message(paste0("Creating points layer data packs now ",
               format(Sys.time(), "%a %b %d %X %Y")))

# Go through stored networks, apply Openinfra functions and create data packs. 
points_network_files = list.files(points_network_dir) # Load stored files
# Dir. to store data packs (Note we can just use piggyback if u don't want to 
#                           store these locally - maybe add boolean check ?)
points_data_pack_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                              "openinfra/data_packs/points/", creation_date, "/")
#network_files = network_files %>% dplyr::filter(! is.na(network_files))
# For all points network files, generate a lines data pack.
for (network_filename in points_network_files){
  # Check for current LAD lines network
  region_name = gsub(".geojson", "", network_filename)
  message(paste0("Checking data pack: ", network_filename, " @ ", 
                 format(Sys.time(), "%a %b %d %X %Y")))
  
  # Check if this data pack already exists
  if (network_filename %in% substring(gsub("_data_pack", "", list.files(points_data_pack_dir)), 21 )){
    # In this instance the data pack already exists - what to do next... ?
    if (overwrite_datapack){
      # Delete data pack .geojson
      unlink(paste0(points_data_pack_dir, creation_date,"_datapack_",
                    network_filename)) 
      # Delete data pack .gpkg
      unlink(paste0(points_data_pack_dir, creation_date,"_datapack_",
                    sub(".geojson", ".gpkg", network_filename)))
      # Nothing else - code below will re-write new data pack.
    } else {
      # Don't overwrite the data pack, move onto the next region
      
      #message(paste0("Error:" , region_name, 
      #               " already has points a data pack, skipping region."))
      next
    }
  }
  message(paste(region_name, " points data pack missing - creating now."))
  # Load the current LAD lines network
  points_network = sf::read_sf(paste0(points_network_dir, network_filename))
  
  # Apply all openinfra functions to network (create the data pack)
  points_network_data_pack = oi_bicycle_parking(points_network, remove = FALSE)
  
  # Select relevant columns for data packs
  points_network_data_pack = points_network_data_pack %>%
    select(osm_id, highway, matches(match = "openinfra_|im_"))
  # Put geometry column at the end (good sf practice)
  points_network_data_pack = sf::st_sf(points_network_data_pack %>%
                                         sf::st_drop_geometry(),
                                       geometry = points_network_data_pack$geometry)
  
  # Save lines data packs locally
  if (local_save_data_pack) {
    for (f in save_formats){
      #region_name
      data_pack_filename = paste0(creation_date, "_", "datapack_",
                                  gsub(".geojson", "", network_filename), f)
      message("Writing points data pack for: ", region_name,
              " with format: ", f)
      sf::st_write(points_network_data_pack,
                   paste0(points_data_pack_dir, data_pack_filename),
                   append = FALSE) 
    }
  }
  
  # Upload points data packs with piggyback 
  if (piggyback_data_packs){
    for (f in save_formats){
      data_pack_filename = paste0(creation_date, "_", "datapack_",
                                  gsub(".geojson", "", network_filename), f)
      message("Uploading points data pack for: ", region_name,
              " with format: ", f)
      ############change file name for upload to releases#######################
      current_filename = paste0(points_data_pack_dir, data_pack_filename)
      upload_filename = gsub("datapack", "points_datapack", current_filename)
      file.rename(current_filename, upload_filename)
      ##########################################################################
      piggyback::pb_upload(upload_filename, tag = release_tag)
      file.rename(upload_filename, current_filename) # Change name back 
    }
  }
}

message("Finished")
