# Get bicycle parking


# Install & Load Packages--------------------------------------------------

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

#________Imports below___________________________
pkgs = c(pkgs, "osmextract", "openinfra")
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]
rm(pkgs, pkg)

# Set up buffer -----------------------------------------------------------
leeds_centre_point = sf::st_sfc(sf::st_point(c(-1.549, 53.801)), crs = "EPSG:4326")
leeds_buffer = sf::st_buffer(leeds_centre_point, dist = 5000)


# Download Data -----------------------------------------------------------
all_extra_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                   "kerb", "footway", "sidewalk", "cycleway", "segregated", "highway",
                   "crossing", "lit", "tactile_paving", "surface", "smoothness", "width",
                   "est_width", "lit_by_led", "ref", "amenity")

# Download leeds ways
leeds_lines = oe_get(
  place = "leeds",
  layer = "lines",
  extra_tags = all_extra_tags
  
)
leeds_lines = leeds_lines[leeds_buffer, op = st_within]

# Download leeds nodes
leeds_points = oe_get(
  place = "leeds",
  layer = "points",
  extra_tags = all_extra_tags
)
leeds_points = leeds_points[leeds_buffer, op=st_within]





# Process Data ------------------------------------------------------------
# Remove different columns 
leeds_lines = within(leeds_lines, rm("z_order", "waterway", "aerialway"))
leeds_points = within(leeds_points, rm("place", "is_in", "address"))

# Combine ways and nodes
leeds_combined = rbind(leeds_points, leeds_lines)

# Subset the data to 5km circular buffer & select bicycle parking
bike_storage = leeds_combined %>% dplyr::filter(amenity == "bicycle_parking")


# Visualise Data ----------------------------------------------------------
# Visualise bike parking in Leeds
#tmap::qtm(bike_storage)


# Data Analysis -----------------------------------------------------------
ways_bikes = leeds_lines %>% dplyr::filter(amenity == "bicycle_parking") 
nodes_bikes = leeds_points %>% dplyr::filter(amenity == "bicycle_parking")



# Testing --------------------------------------------------------------
# Download just Leeds .osm.pbf
dl_leeds = oe_get(
  place = "Leeds",
  download_only = TRUE,
  force_download = TRUE,
  download_directory = oe_download_directory()
)

fp = paste0(oe_download_directory(), "/bbbike_Leeds.osm.pbf")

leeds_lines = oe_read(
  file_path = fp,
  never_skip_vectortranslate = TRUE,
  layer = "lines",
  extra_tags = c("amenity")
)
leeds_lines = leeds_lines[leeds_buffer, op = st_within]

leeds_points = oe_read(
  file_path = fp,
  never_skip_vectortranslate = TRUE,
  layer = "points",
  extra_tags = c("amenity")
)
leeds_points = leeds_points[leeds_buffer, op = st_within]

# Remove different columns 
leeds_lines = within(leeds_lines, rm("z_order", "waterway", "aerialway"))
leeds_points = within(leeds_points, rm("place", "is_in", "address", "ref"))

leeds_combi = rbind(leeds_points, leeds_lines)


bike_storage_in_leeds = leeds_combi %>% dplyr::filter(amenity == "bicycle_parking")


# it does not matter if we download and parse ways and points with seperate 
# oe_get commands or a single oe_download followed by two oe_reads (one for 
# nodes and one for points). Thus, in create_data_packs.R we should get combined
# data from reading england-latest twice with oe_read, one for nodes and one for
# ways before combining into a single network using rbind().