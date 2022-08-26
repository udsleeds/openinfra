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


# Comments ----------------------------------------------------------------


# it does not matter if we download and parse ways and points with seperate 
# oe_get commands or a single oe_download followed by two oe_reads (one for 
# nodes and one for points). Thus, in create_data_packs.R we should get combined
# data from reading england-latest twice with oe_read, one for nodes and one for
# ways before combining into a single network using rbind().


# Proposed function -------------------------------------------------------
# To assess bicycle parking anywhere in the uk we require OSM node data. Thus, 
# we must read te "points" layer of the downloaded .gpkg file. So for creating
# data packs we need to get lines & nodes and combine them into a single network
# by using rbind(). 
# 
# To do this we can use a funtion oi_get(), a wrapper for 
# oe_get to download and read "lines" and "points" from the file. 
#
# oe_get: https://github.com/ropensci/osmextract/blob/master/R/get.R
#
# Next, a function oi_bicycle_parking than analyses the combined network for the
# amenity == "bicycle_parking" tag to find bicycle parking infrastructure. 


# oi_get() function proposal ----------------------------------------------

# General comment - this is getting a bit messy and is not really a priority - 
# for now we can just called oe_read twice within create_data_packs rather than#
# creating this warapper function.

# Def oi_get() - a wrapper for oe_get()
oi_get = function(
    palce,
    layer = "both",
    ...,
    provider = "geofabrik",
    match_by = "name",
    max_string_dist = 1,
    level = NULL,
    force_download = FALSE,
    max_file_size = 5e+8,
    vectortranslate_options = NULL,
    osmconf_ini = NULL,
    extra_tags = NULL,
    force_vectortranslate = FALSE,
    boundary = NULL,
    boundary_type = NULL,
    download_only = FALSE,
    skip_vectortranslate = FALSE,
    never_skip_vectortranslate = FALSE,
    quiet = FALSE
) {
  
  # 0: check osmextract is installed
  if (! ("osmextract" %in% installed.packages())){
    message("Error: the osmextract package is required to obtain OSM data. ",
            "Please install with `install.packages('osmextract')`, or for the ",
            "dev version use `remotes::install_github('ropensci/osmextract')`")
    break
  }
  
  # 1: Download place data
  osmextract::oe_get(
    palce = place,
    download_only = TRUE,
    force_download = force_download,
  )
  
  # 2: Read "lines" layer
  place_lines = oe_get(
    palce = place, 
    layer = "lines",
    extra_tags = extra_tags
  )
  
  # 3: read "points" layer
  
  # 4: combine layers & return
  
  
}



# oi_bicycle_parking function proposal ------------------------------------
# Function that takes an osm_sf and adds an additional column oi_cycle_parking
# containing values "yes" or NA based on whether or not the node in question is
# tagged with the amenity=="bicycle_parking" tag

oi_bicycle_parking = function(osm_sf, remove = FALSE){
  osm_sf_recat = osm_sf %>% 
    dplyr::mutate(oi_cycle_parking = dplyr::case_when(
      amenity == "bicycle_parking" ~ "yes"
    ))
  
  if (remove){
    osm_sf_recat = osm_sf_recat %>% dplyr::filter(! is.na(oi_cycle_parking))
  }
  
  return(osm_sf_recat)
} 

# Testing the function
leeds_combined_test = oi_bicycle_parking(leeds_combined)

# View value counts of oi_cycle_parking (yes == 291, so the function works)
vc = as.data.frame(table(leeds_combined_test$oi_cycle_parking))
