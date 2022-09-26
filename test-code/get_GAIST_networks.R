
# Library imports ---------------------------------------------------------
pkgs = c("od", "tmaptools", "stplanr", "tidyverse", "pct", "dplyr", "tmap",
         "sf")
#install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]

# Function to find best matched LAN --> RN --------------------------------

get_region_name = function(LAN, auto_match=FALSE){
  matches = pct::pct_regions_lookup %>% dplyr::filter(lad16nm == LAN)
  
  # No matches found
  if(nrow(matches) == 0){
    # No exact match found, try find best match. 
    message(paste0("No exact Local Authority match found for: ", LAN, 
                   " so searching for a match now..."))
    
    # Score matches against provided LAN
    matches = utils::adist(pct_regions_lookup$lad16nm, LAN)
    # Identify most similar place ID against LAN provided
    best_match_id  = which(matches == min(matches, na.rm = TRUE))
    # Identify best matched place using place ID
    best_matched_place = pct_regions_lookup[best_match_id, ]
    # Extract region_name from best matched place
    message("Best matched place is: ", best_matched_place$lad16nm, ", ",
            best_matched_place$region_name)
    # Get region name field from lookup table
    RN = best_matched_place$region_name
    
    # Should we promt the user to accept the best match? 
    if(! auto_match){
      # Ask user if they want to use the best match
      continue = utils::menu(
        choices = c("Yes", "No"),
        title = "Would you like to obtain data for the best matched region?"
      )
      
      # Assess users response
      if(continue != 1){
        stop(paste0("User aborted match. To see supported authorities use ",
                    "`View(pct_regions_lookup)`"))
        return(NULL)
      }else{
        message("Using best matched local authority ", LAN, " with",
                " region_name: ", RN)
        return(RN)
      }
    }else{
      message("Using best matched local authority ", best_matched_place$lad16nm,
              " with region_name: ", RN)
      return(RN)
    }

  
  # A perfect match has been found, return matching region_name
  }else{
    RN = matches$region_name[[1]]
    message(paste0("Local authority ", LAN, "has been exactly matched to",
                   " region-name: ", RN))
    return(RN)
  }
}


# Testing  ----------------------------------------------------------------

# LAN = "Peterborough"
# matches = utils::adist(pct_regions_lookup$lad16nm, LAN)
# best_match_id  = which(matches == min(matches, na.rm = TRUE))
# 
# if (length(best_match_id) > 1L) {
#   best_match_id = best_match_id[1L]
# }
# best_match_id
# 
# best_matched_place = pct_regions_lookup[best_match_id, ]
# message("Best matched place is: ", best_matched_place$lad16nm, ", ",
#         best_matched_place$region_name)
# RN = best_matched_place$region_name


# LIDA Example test -------------------------------------------------------

# lida_point = tmaptools::geocode_OSM("Worsley Building, Leeds")
# leeds_point = tmaptools::geocode_OSM("leeds")
# 
# c_m_coordiantes = rbind(lida_point$coords, leeds_point$coords)
# c_m_od = od::points_to_od(p = c_m_coordiantes, interzone_only = TRUE)
# c_m_desire_line = od::odc_to_sf(c_m_od[-(1:2)])[1, ]
# lida_buffer = stplanr::geo_buffer(c_m_desire_line, dist = 2000)


# Leicestershire test -----------------------------------------------------

############# Param setup 

region_name = "leicestershire"
radius = 150 # metres
osm_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
             "kerb", "footway", "sidewalk", "cycleway", "segregated", 
             "highway", "crossing", "lit", "tactile_paving", "surface",
             "smoothness", "width", "est_width", "lit_by_led", "ref",
             "amenity", "cycleway_left", "cycleway_right", 
             "cycleway_both", "separation")

# Define translate options (from osmextract docs I think)
translate_options = c(
  "-nlt", "PROMOTE_TO_MULTI",       # Check this
  "-where", "highway IS NOT NULL") 

# Info for locally stored england-latest.osm.pbf
creation_date = "14_09_2022"
GAIST_eng_latest_dir = '/home/james/Desktop/LIDA_OSM_Project/openinfra/GAIST_eng_latest/'

############# Get PCT Routes data 

############### INVESTIGATE PCT LAYERS _ DELETE AFTER ##########################
a_rnet_pct = pct::get_pct_rnet(region_name)
a_fast_pct = pct::get_pct_routes_fast(region_name)
a_quiet_pct = pct::get_pct_routes_quiet(region_name)

plot(a_fast_pct$geometry, col = "red")
plot(a_quiet_pct$geometry, col= "blue", add = TRUE)
plot(a_rnet_pct$geometry, col = "grey", add = TRUE)

tmap::tmap_mode("view")

plot = tmap::tm_shape(a_fast_pct) + 
  tmap::tm_lines(palette = c("red")) +
tmap::tm_shape(a_quiet_pct) + 
  tmap::tm_lines(palette = c("blue")) + 
tmap::tm_shape(a_rnet_pct) + 
  tmap::tm_lines(palette = c("blue"))

tmap::tmap_save(plot, "/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/test_pct_plot.html")

# Remove geometries
a_rnet_pct_no_geom = sf::st_drop_geometry(a_rnet_pct)
a_fast_pct_no_geom = sf::st_drop_geometry(a_fast_pct)
a_quiet_pct_no_geom = sf::st_drop_geometry(a_quiet_pct)

a_look = dplyr::inner_join(a_fast_pct_no_geom, a_quiet_pct_no_geom)

# Test overlaps
library(dplyr)
#a_fast_quiet_look = dplyr::inner_join(a_fast_pct, a_quiet_pct)
a_fast_quiet_look = sf::st_join(a_fast_pct, a_quiet_pct, join=st_disjoint)
################################################################################

# Get default, fast, AND quiet PCT Routes
rnet_pct = pct::get_pct_rnet(region_name)
rnet_pct_fast = pct::get_pct_routes_fast(region_name)
rnet_pct_quiet = pct::get_pct_routes_quiet(region_name)

# Get top 10% of default, fast, AND quiet routes
rnet_pct_top10 = rnet_pct |>
  top_frac(n = 0.1, wt = rnet_pct$govtarget_slc)
rnet_pct_fast_top10 = rnet_pct_fast |>
  top_frac(n = 0.1, wt = rnet_pct_fast$govtarget_slc)
# CANNOT BE DONE FOR QUIET AS NO GOVTARGET_SLC COLUMN
#rnet_pct_quiet_top10 = rnet_pct_quiet |>
#  top_frac(n = 0.1, wt = rnet_pct_quiet$govtarget_slc)

# Visualise entire network & top 10%
plot(rnet_pct$geometry, col = "grey")
plot(rnet_pct_top10$geometry, col= "red", add = TRUE)

# Visualise entire fast network & top 10%
plot(rnet_pct_fast$geometry, col = "grey")
plot(rnet_pct_fast_top10$geometry, col= "red", add = TRUE)

############# Apply spatial buffer around PCT routes

# Create buffer around top 10% routes (150 metres)
test_buffer = stplanr::geo_buffer(rnet_pct_top10, dist = radius)
# Combine unique polygon buffers
test_union = test_buffer %>% summarise(geometry = sf::st_union(geometry))
# Visualise the MultiPolygon union
plot(test_union)

# For the fast networks
fast_buffer = stplanr::geo_buffer(rnet_pct_fast_top10, dist = radius)
fast_union = fast_buffer %>% summarise(geometry = sf::st_union(geometry))
plot(fast_union)

############# Use spatial buffer to get OSM data (w osmextract)

# Get filepath for locally stored englanf-latest.osm.pbf file
fp = paste0(GAIST_eng_latest_dir, creation_date, "/")
eng_latest_fp = paste0(fp, "geofabrik_england-latest.osm.pbf")


# Acquire data for buffered regions
oe_osm_data = osmextract::oe_read(
  file_path = eng_latest_fp,
  layer = "lines",
  vectortranslate_options = translate_options,
  never_skip_vectortranslate = TRUE,
  force_vectortranslate = TRUE,
  extra_tags = osm_tags,
  boundary = test_union,
  boundary_type = "spat", 
  quiet = FALSE
)

# For fast buffer
oe_osm_data_fast = osmextract::oe_read(
  file_path = eng_latest_fp,
  layer = "lines",
  vectortranslate_options = translate_options,
  never_skip_vectortranslate = TRUE,
  force_vectortranslate = TRUE,
  extra_tags = osm_tags,
  boundary = fast_union,
  boundary_type = "spat", 
  quiet = FALSE
)


# Apply spatial buffer ("spat" in somextract not the best... but it works...)
oe_osm_data_buffed = oe_osm_data[test_union, ]

# Apply openinfra function, leaving only cycling infra related data behind
oe_osm_data_cycling = openinfra::oi_cycle_separation(oe_osm_data_buffed, remove = TRUE)

############# Visualise the outputs (different subsetting ops)

tmap::tm_shape(test_union) + 
  tmap::tm_polygons(alpha = 0.45) + 
tmap::tm_shape(oe_osm_data_cycling) + 
  tmap::tm_lines(col = "openinfra_cycle_infra")

clipped_data_within = oe_osm_data_cycling[test_union, , op=st_within]

tmap::tm_shape(test_union) + 
  tmap::tm_polygons(alpha = 0.45) + 
tmap::tm_shape(clipped_data_within) + 
  tmap::tm_lines(col = "openinfra_cycle_infra")

clipped_data_intersect = oe_osm_data_cycling[test_union, , op=st_intersects]
clipped_data_intersect = clipped_data_intersect %>% dplyr::filter(! is.na(openinfra_cycle_infra))

map = tmap::tm_shape(test_union) + 
  tmap::tm_polygons(alpha = 0.45) + 
tmap::tm_shape(clipped_data_intersect) + 
  tmap::tm_lines(col = "openinfra_cycle_infra")

# comment after saving
tmap::tmap_save(map, '/home/james/Desktop/LIDA_OSM_Project/openinfra/Openinfra htmls/email_GAIST_example.html')

++# Set-up ------------------------------------------------------------------


# Get all Local Authority Names
places = pct_regions_lookup %>% dplyr::filter(! is.na(region_name))
LANs = places$lad16nm
LANs = LANs[1:5] # Subset of LANs for testing purposes
RN = get_region_name("Leiester")

# Network directory to store geosjons or plots (static & htmls?)
GAIST_pct_network_dir = paste0("/home/james/Desktop/LIDA_OSM_Project/openin",
                               "fra/GAIST_PCT_Networks/")

# Get data & create plots.  -----------------------------------------------

# for(LAN in LANs){
#   message("Working on: ", LAN)
#   
#   # Get region_name to acquire data
#   RN = get_region_name(LAN, auto_match = TRUE)
#   
#   # Get pct route network
#   rnet_pct = pct::get_pct_rnet(RN)
#   
#   # Get top 10% of routes, scaled by the government uptake targets (SLC)
#   rnet_top_10 = rnet_pct |> 
#     top_frac(n = 0.1, wt = govtarget_slc)
#   
#   # Plot routes with highest potential, overlayed with top 10% of routes
#   tmap::tmap_mode("view")
#   
#   tmap::tm_shape(rnet_pct) + 
#     tmap::tm_lines(col = "black") + 
#     tmap::tm_shape(rnet_top_10) + 
#     tmap::tm_lines(col = "red") + 
#     tmap::tm_layout(title = paste0("Local Autority: ", LAN, 
#                                    ", Region Name: ", RN),
#                     legend.show = TRUE)
#   
#   
#   #plot(rnet_pct$geometry, col = "grey")
#   #plot(rnet_top_10$geometry, add = TRUE)
# }


# Iterate over region names | Create & save interactive plots of PCT routes----

# Get unique, non-NA region names only
RNs = pct_regions_lookup %>% dplyr::filter(! is.na(region_name))
RNs = unique(RNs$region_name)

for(RN in RNs){

  #TODO: Add check for existing files, don't want to save too many. 
  
  message("Loading region name: ", RN)

  # Get pct route network
  rnet_pct = pct::get_pct_rnet(RN)
  rnet_pct = rnet_pct %>% dplyr::mutate(desc = "Default routes")
  
  # Get top 10% of routes, scaled by the government uptake targets (SLC)
  rnet_top_10 = rnet_pct |> 
    top_frac(n = 0.1, wt = govtarget_slc)
  
  # Remove and replace desc column for legend
  rnet_top_10 = subset(rnet_top_10, select = -c(desc) )
  rnet_top_10 = rnet_top_10 %>% dplyr::mutate(desc ="90% percentile routes")
  
  # Add fast & quiet routes too!
  
  
  # Plot routes with highest cycling potential, overlayed with top 10% of routes
  RegionMap = tmap::tm_shape(rnet_pct) + 
    tmap::tm_lines(col="desc", palette="black", title.col="Routes type") + 
  tmap::tm_shape(rnet_top_10) + 
    tmap::tm_lines(col="desc", palette="red", title.col="Routes type") + 
    tmap::tm_layout(main.title = paste0("Region Name: ", str_to_title(RN)),
                    #main.title.position = "centre",
                    title = paste0("Region Name: ", RN),
                    legend.bg.color = "grey",
                    legend.bg.alpha = .4
                    ) 

  # Save the map
  filepath = paste0(GAIST_pct_network_dir, RN, ".html")
  tmap::tmap_save(RegionMap, filepath)
} 



# Get cycling infrastructure with oi_cycle_separation ---------------------

regions = pct::pct_regions
r_names = regions$region_name
r_geoms = regions$geometry	

# Get england-latest data (to be subset for each region)
creation_date = "14_09_2022"
GAIST_eng_latest_dir = '/home/james/Desktop/LIDA_OSM_Project/openinfra/GAIST_eng_latest/'
fp = paste0(GAIST_eng_latest_dir, creation_date, "/")

# Create DL directory if not already present
dir.create(fp)

eng_dl = osmextract::oe_get(
  place = "England",
  extra_tags = required_tags,
  download_only = TRUE,
  skip_vectortranslate = TRUE, 
  download_directory = fp)
  
eng_latest_fp = paste0(fp, "geofabrik_england-latest.osm.pbf")
  
  
for(i in 1:nrow(regions)){
    
  RN = r_names[i] # Region Name
  RG = r_geoms[i] # Region Geometry
  
  #TODO: Add check for existing files, don't want to save too many. 
    
  # Get OSM data using region geometry
  message("Getting ", RN, " region data.")
    
  translate_options = c(
    "-nlt", "PROMOTE_TO_MULTI",       # Check this
    "-where", "highway IS NOT NULL")   # Highway cannot be NA
    
  required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                    "kerb", "footway", "sidewalk", "cycleway", "segregated", 
                    "highway", "crossing", "lit", "tactile_paving", "surface",
                    "smoothness", "width", "est_width", "lit_by_led", "ref",
                    "amenity", "cycleway_left", "cycleway_right", 
                    "cycleway_both", "separation"
                   )
  
  # Get region data  
  r_data = osmextract::oe_read(
    eng_latest_fp,
    vectortranslate_options = translate_options,
    layer = "lines",
    extra_tags = required_tags,
    boundary = RG,
    boundary_type = "clipsrc",
    quiet = TRUE
  )
    
  # Create cycle infra network
  cycle_infra_network = openinfra::oi_cycle_separation(r_data, remove=TRUE)
    
    
  # Save the network as a geojson?
  GAIST_fp = '/home/james/Desktop/LIDA_OSM_Project/openinfra/GAIST_cycle_infra_networks/'
  file_name = paste0(RN, "_cycle_infra_network.geojson")
    
  sf::st_write(cycle_infra_network, paste0(GAIST_fp, file_name))
    
}

# Testing visualisation ----------------------------------------------------


url = "https://github.com/udsleeds/openinfraresults/releases/download/cycle_infra/leicestershire_cycle_infra_network.geojson"
test_network = sf::read_sf(url)
test_network = test_network %>% dplyr::filter(! is.na(openinfra_cycle_infra))
tmap::tmap_mode("view")

tmap::qtm(test_network)
# Create a buffer with high cycling potential -----------------------------

#rnet_pct = pct::get_pct_rnet(RN)

# Get top 10% of routes, scaled by the government uptake targets (SLC)
#rnet_top_10 = rnet_pct |> 
#  top_frac(n = 0.1, wt = govtarget_slc)

# Plot routes with highest potential, overlayed with top 10% of routes
#plot(rnet_pct$geometry, col = "grey")
#plot(rnet_top_10$geometry, add = TRUE)