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

# This script contains a part of the workflow from create_data_packs. 
# Specifically, this script creates the network files which are then used 
# to create the data packs for each LAD within create_data_packs.

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
                  "smoothness", "width", "est_width", "lit_by_led", "boundary",
                  "admin_level", "name", "ref", "amenity")


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

Scotland_LADs = c("South Ayrshire", "South Lanarkshire", "Scottish Borders",
                  "East Ayrshire", "East Lothian", "Midlothian",
                  "City of Edinburgh", "West Lothian", "North Lanarkshire",
                  "Glasgow City", "East Renfrewshire", "North Ayrshire",
                  "Argyll and Bute", "Renfrewshire", "Inverclyde", "Fife",
                  "West Dunbartonshire", "East Dunbartonshire", "Stirling",
                  "Falkirk", "Clackmannanshire", "Perth and Kinross",
                  "Dundee City", "Na h-Eileanan Siar", "Highland",
                  "Shetland Islands", "Orkney Islands", "Moray", "Aberdeenshire",
                  "Aberdeen City", "Angus")

NI_LADs = c("Causeway Coast and Glens", "Derry City and Strabane", "Belfast",
            "Fermanagh and Omagh", "Mid Ulster", "Mid and East Antrim", 
            "Antrim and Newtownabbey", "Armagh City, Banbridge and Craigavon",
            "Newry, Mourne and Down", "Lisburn and Castlereagh",
            "Ards and North Down", "Causeway Coast and Glens")

Welsh_LADs = c("Isle of Anglesey", "Flintshire", "Denbighshire", "Conwy", 
               "Gwynedd", " Powys", "Ceredigion", "Pembrokeshire", 
               "Carmarthenshire", "Monmouthshire", "Newport", "Torfaen", 
               "Blaenau Gwent", "Merthyr Tydfil", "Rhondda Cynon Taf", "Cardiff",
               "Vale of Glamorgan", "Bridgend", "Neath Port Talbot", "Swansea",
               "Caerphilly", "Wrexham"
)

#aLADs = LADs
#aLADs = aLADs %>% dplyr::filter(! LAD21NM %in% Scotland_LADs)
#aLADs = aLADs %>% dplyr::filter(! LAD21NM %in% NI_LADs)
# Remove Scottish LADs
LADs = LADs %>%  dplyr::filter(! LAD21NM %in% Scotland_LADs)
# Remove NI LADs
LADs = LADs %>% dplyr::filter(! LAD21NM %in% NI_LADs)
# Remove Welsh LADs
LADs = LADs %>% dplyr::filter(! LAD21NM %in% Welsh_LADs)

LADs = LADs %>% dplyr::filter(! is.na(LAD21NM))

#LADs = LADs[lad_limit, ] # only use first 5 whilst testing. (comment out when done)
LAD_polys = LADs %>% dplyr::select(c("LAD21NM", "geometry"))

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
for (region_name in LADs$LAD21NM){
  # Generate file name
  filename = paste0(gsub(" ", "_", region_name), ".geojson")
  
  # Check if a network exists for current region: overwrite or not 
  if (filename %in% list.files(lines_network_dir)) {
    # In this instance the network exists in network_dir - what to do next...
    
    if (overwrite_network){
      # overwrite the existing network...
      message(region_name, " lines network found - overwrite is TRUE so re-writing")
      # (Do nothing here, network will be overwritten by code below)
      
    } else{
      # Don't overwrite network, move to the next region.
      #message(paste("Skipping:", region_name,
      #              "as network .geojson already exists"))
      next
    }
  } else {
    # The network geojson does not exist
    message(paste(region_name, " lines network not found - creating now."))
  }
  
  # Get current LAD polygon
  subset_poly = LAD_polys %>% 
    dplyr::filter(LAD_polys$LAD21NM == region_name) %>% 
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
for (region_name in LADs$LAD21NM){
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
    dplyr::filter(LAD_polys$LAD21NM == region_name) %>% 
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


message("Finished")
