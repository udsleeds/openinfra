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
release_tag = "0.5"                   # Releases tag for piggyback
creation_date = "2022_09_21"          # Date of download for england-latest.osm
lad_limit = 1:330                     # Limits number of LADs to be processed
# Comment below if not using local dev package versions
devtools::load_all()

# File path or URL to LAD bounding polygons
LAD_polygons_path = paste0("https://github.com/udsleeds/openinfra/releases/",
                           "download/v0.1/Local_Authority_Districts_.",
                           "May_2022._UK_BSC.geojson")

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

# Check if osmextract dev is installed - install it if not
if (!("osmextract" %in% installed.packages())){
  remotes::install_github("ropensci/osmextract")
} 

# Check if openinfradev is installed - install it if not
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

# Create dirs at current date to store default OSM networks from eng-latest.osm
dir.create(paste0(lines_network_dir, creation_date))  # Lines dir
dir.create(paste0(points_network_dir, creation_date)) # Points dir

# Create dir for current date to store data packs created from default networks
dir.create(paste0(lines_data_pack_dir, creation_date))

dir.create(paste0(points_data_pack_dir, creation_date))

# Get required data -------------------------------------------------------

# Tags required to make data packs
required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                  "kerb", "footway", "sidewalk", "cycleway", "segregated",
                  "highway", "crossing", "lit", "tactile_paving", "surface",
                  "smoothness", "width", "est_width", "lit_by_led", "ref",
                  "amenity", "sidewalk", "sidewalk:left", "sidewalk:right",
                  "sidewalk:both", "source:maxspeed", "maxspeed:type",
                  "zone:maxspeed", "zone:traffic", "maxspeed", "HFCS", "rural",
                  "cycleway_left", "cycleway_right", "cycleway_both",
                  "separation")


# Download & save OSM data
options(timeout = 1000) # Un-comment if download fails due to timeout. 
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

# Get LAD Boundaries ------------------------------------------------------
# Download LAD boundary polygons
LADs = sf::read_sf(LAD_polygons_path)
LADs = sf::st_make_valid(LADs)

english_lads = LADs %>% dplyr::filter(stringr::str_detect(string = LAD22CD, pattern="E"))
welsh_lads = LADs %>% dplyr::filter(stringr::str_detect(string = LAD22CD, pattern = "W"))
scottish_lads = LADs %>% dplyr::filter(stringr::str_detect(string = LAD22CD, pattern = "S"))
ni_lads = LADs %>% dplyr::filter(stringr::str_detect(string = LAD22CD, pattern = "N"))

# Remove Scottish LADs
LADs = LADs %>%  dplyr::filter(! LAD22NM %in% scottish_lads$LAD22NM)
# Remove NI LADs
LADs = LADs %>% dplyr::filter(! LAD22NM %in% ni_lads$LAD22NM)
# Remove Welsh LADs
LADs = LADs %>% dplyr::filter(! LAD22NM %in% welsh_lads$LAD22NM)
# Remove NA LADs
LADs = LADs %>% dplyr::filter(! is.na(LAD22NM))

#LADs = LADs[lad_limit, ] # only use first 5 whilst testing. (comment out when done)
LAD_polys = LADs %>% dplyr::select(c("LAD22NM", "geometry"))

# Create & save default OSM networks per LAD -------------------------------

# Specify default parsing requirements 
translate_options = c(
  "-nlt", "PROMOTE_TO_MULTI",       # Check this
  "-where", "highway IS NOT NULL")  # Highway cannot be NA

# Specify file path to save the default networks.
lines_network_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                     "openinfra/data_pack_networks/lines/", creation_date, "/")
points_network_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                     "openinfra/data_pack_networks/points/", creation_date, "/")


# Create & save default OSM line networks per LAD -------------------------
# Iterates through all LADs, saving a geojson of each default OSM lines network. 
for (region_name in LADs$LAD22NM){
  # Generate file name
  filename = paste0(gsub(" ", "_", region_name), ".geojson")
  
  # Check if a network exists for current region: overwrite or not 
  if (filename %in% list.files(lines_network_dir)) {
    # In this instance the network exists in network_dir - what to do next...
    
    if (overwrite_network){
      # overwrite the existing network...
      message(region_name, " lines network found - overwrite is TRUE so re-writing")
      # (Do nothing here, network will be overwritten by code below)
    
      }else{
      # Don't overwrite network, move to the next region.
      next
    }
  }else{
    # The network geojson does not exist
    message(paste(region_name, " lines network not found - creating now."))
  }
  
  # Get current LAD polygon
  subset_poly = LAD_polys %>% 
    dplyr::filter(LAD_polys$LAD22NM == region_name) %>% 
    dplyr::select("geometry")
  # Check if polygon is invalid, fixes if it is.
  subet_poly = sf::st_make_valid(subset_poly)
  
  # Get network by sub setting England.osm.pbf, save output as geojson.
  message("Reading lines for: ", region_name)
  network_lines = oe_read(
    eng_latest_fp,
    vectortranslate_options = translate_options,
    layer = "lines",
    extra_tags = required_tags,
    boundary = subset_poly,
    boundary_type = "clipsrc",
    quiet = TRUE
  )
  
  # Save LAD network as geojson 
  sf::st_write(network_lines, paste0(lines_network_dir, filename),
               append = FALSE, quiet = TRUE)
  
  # Print status
  message(c(region_name, "lines network created @ ",
            format(Sys.time(), "%a %b %d %X %Y")))
}


# Create & save default OSM point networks per LAD -----------------------
# Iterates through all LADs, saving a geojson of each default OSM point network. 
for (region_name in LADs$LAD22NM){
  # Generate file name
  filename = paste0(gsub(" ", "_", region_name), ".geojson")
  
  # Check if a network exists for current region: overwrite or not 
  if (filename %in% list.files(points_network_dir)) {
    # In this instance the network exists in network_dir - what to do next...
    
    if (overwrite_network){
      # overwrite the existing network...
      message(region_name, " points network found - overwrite is TRUE so re-writing")
      # (Do nothing here, network will be overwritten by code below)
      
    } else{
      # Don't overwrite network, move to the next region.
      #message(paste("Skipping:", region_name,
      #              "as points network .geojson already exists"))
      next
    }
  } else {
    # The network geojson does not exist
    message(paste(region_name, " points network not found - creating now."))
  }
  
  # Get current LAD polygon
  subset_poly = LAD_polys %>% 
    dplyr::filter(LAD_polys$LAD22NM == region_name) %>% 
    dplyr::select("geometry")
  # Check if polygon is invalid, fixes if it is.
  # subet_poly = sf::st_make_valid(subset_poly) #(already done above I think)
  
  # Get network by sub setting England.osm.pbf, save output as geojson.
  message("Reading points for: ", region_name)
  network_points = oe_read(
    eng_latest_fp,
    vectortranslate_options = translate_options,
    layer = "points",
    extra_tags = required_tags,
    boundary = subset_poly,
    boundary_type = "clipsrc",
    quiet = TRUE
  )
  
  # Save LAD points network as geojson 
  sf::st_write(network_points, paste0(points_network_dir, filename),
               append = FALSE, quiet = TRUE)
  
  # Print status
  message(c(region_name, " points network created @ ",
            format(Sys.time(), "%a %b %d %X %Y")))
}


# Generate Lines layer infrastructure data packs -------------------------
message(c("Creating lines data packs now ", format(Sys.time(), "%a %b %d %X %Y")))

# Go through stored networks, apply Openinfra functions and create data packs. 
lines_network_files = list.files(lines_network_dir) # Load stored files
# Dir. to store data packs (Note we can just use piggyback if u don't want to 
#                           store these locally - maybe add boolean check ?)
lines_data_pack_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openinfra/",
                       "openinfra/data_packs/lines/", creation_date, "/")

# For all LAD lines network files, generate a lines data pack.
level = "lad"
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
      # Nothing else - code below will re-write new data pack.
    } else {
      # Don't overwrite the data pack, move onto the next region
      
      #message(paste0("Error:" , region_name, 
      #               " already has a data pack, skipping region."))
      next
    }
  }
  message(paste(region_name, "lines data pack missing - creating now."))
  # Load the current LAD lines network
  lines_network = sf::read_sf(paste0(lines_network_dir, network_filename))
  message("lines -- ", dim(lines_network))
  # Apply all openinfra functions to network (create the data pack)
  lines_network_data_pack = oi_active_cycle(lines_network, remove = FALSE)
  message("1st DP -- ", dim(lines_network_data_pack))
  lines_network_data_pack = oi_active_walk(lines_network_data_pack,
                                           remove = FALSE)
  lines_network_data_pack = oi_clean_maxspeed_uk(lines_network_data_pack,
                                                 no_NA = FALSE, del = FALSE)
  #lines_network_data_pack = oi_inclusive_mobility(lines_network_data_pack)
  
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
  message("2L DP -- ", dim(lines_network_data_pack))
  lines_network_data_pack = oi_cycle_separation(lines_network_data_pack,
                                                remove = FALSE)
  message("Final DP -- ", dim(lines_network_data_pack))
  force(lines_network_data_pack)
  message("1")
  
  # Select relevant columns for data packs
  lines_network_data_pack = lines_network_data_pack %>%
    select(osm_id, highway, matches(match = "openinfra_|im_"))
  message("2")
  # Put geometry column at the end (good sf practice)
  lines_network_data_pack = sf::st_sf( lines_network_data_pack %>%
                                   sf::st_drop_geometry(),
                                 geometry = lines_network_data_pack$geometry)
  
  # Save lines data packs locally
  if (local_save_data_pack) {
    for (f in save_formats){
      #region_name
      #data_pack_filename = paste0(creation_date, "_", "datapack_",
      #                            gsub(".geojson", "", network_filename), f)
      
      #filename format packname_level_regionname_yyyy-mm-dd.format
      #for an example: datapack_lad_adur_2022-08-25.geojson
      
      data_pack_filename = paste0("datapack_", level, "_", region_name,
                                  "_", creation_date, f)
      
      message("Writing data pack for: ", region_name, " with format: ", f)
      sf::st_write(lines_network_data_pack,
                   paste0(lines_data_pack_dir,data_pack_filename),
                   append = FALSE) 
    }
  }
  
  # Upload lines data packs with piggyback 
  if (piggyback_data_packs){
    for (f in save_formats){
      #data_pack_filename = paste0(creation_date, "_", "datapack_",
      #                            gsub(".geojson", "", network_filename), f)
      
      data_pack_filename = paste0("datapack_", level, "_", region_name,
                                  "_", creation_date, f)      
      
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

# For all LAD points network files, generate a lines data pack.
level = "lad"
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
      #unlink(paste0(points_data_pack_dir, creation_date,"_datapack_",
      #              network_filename))
      unlink(paste0(points_data_pack_dir, "datapack_", level, 
                    gsub(".geojson", "", network_filename), "_",
                    creation_date, ".geojson"))
      # Delete data pack .gpkg
      #unlink(paste0(points_data_pack_dir, creation_date,"_datapack_",
      #              sub(".geojson", ".gpkg", network_filename)))
      unlink(paste0(points_data_pack_dir, "datapack_", level, 
                    gsub(".geojson", "", network_filename), "_",
                    creation_date, ".gpkg"))
      
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
      #data_pack_filename = paste0(creation_date, "_", "datapack_",
      #                            gsub(".geojson", "", network_filename), f)
      data_pack_filename = paste0("datapack_", level, "_", 
                                  gsub(".geojson", "", network_filename), "_",
                                  creation_date, f)
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
      #data_pack_filename = paste0(creation_date, "_", "datapack_",
      #                            gsub(".geojson", "", network_filename), f)
      data_pack_filename = paste0("datapack_", level, "_", 
                                  gsub(".geojson", "", network_filename), "_",
                                  creation_date, f)
      
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


# Upload data packs only --------------------------------------------------
# Assuming data packs have already been created, simply upload the lines and 
# points data packs to gh releases using piggyback.

# Upload lines data packs:
#message("Uploading lines data packs now")
#for (lines_network_filename in lines_network_files){
#  region_name = gsub(".geojson", "", lines_network_filename)
#  
#  if (piggyback_data_packs){
#    for (f in save_formats){
#      data_pack_filename = paste0(creation_date, "_", "datapack_",
#                                  gsub(".geojson", "", lines_network_filename), f)
#      message("Uploading lines data pack for: ", region_name, " with format: ", f)
#      
#      ############change file name for upload to releases#######################
#      current_filename = paste0(lines_data_pack_dir, data_pack_filename)
#      upload_filename = gsub("datapack", "lines_datapack", current_filename)
#      file.rename(current_filename, upload_filename)
#      ##########################################################################
#      
#      #piggyback::pb_upload(paste0(lines_data_pack_dir, data_pack_filename), 
#      #                     tag = release_tag)
#      piggyback::pb_upload(upload_filename, tag = release_tag)
#      file.rename(upload_filename, current_filename) # Change filename back 
#    }
#  }
#}

# Upload points data packs
#message("Uploading points data packs now")
#for (points_network_filename in points_network_files){
#  region_name = gsub(".geojson", "", points_network_filename)
#  
#  if (piggyback_data_packs){
#    for (f in save_formats){
#      data_pack_filename = paste0(creation_date, "_", "datapack_",
#                                  gsub(".geojson", "", points_network_filename), f)
#      message("Uploading points data pack for: ", region_name,
#              " with format: ", f)
#      ############change file name for upload to releases#######################
#      current_filename = paste0(points_data_pack_dir, data_pack_filename)
#      #print(current_filename)
#      upload_filename = gsub("datapack", "points_datapack", current_filename)
#      #print(upload_filename)
#      file.rename(current_filename, upload_filename)
#      ##########################################################################
#      piggyback::pb_upload(upload_filename, tag = release_tag)
#      file.rename(upload_filename, current_filename) # Change name back 
#    }
#  }
#}



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