
# Library imports ---------------------------------------------------------
pkgs = c("od", "tmaptools", "stplanr", "tidyverse", "pct", "dplyr", "tmap", 
         "sf", "openinfra", "osmextract")

# install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]

# Function to find best matched LAN --> RN --------------------------------
# for the pct tool. 
get_region_name = function(LAN, auto_match=FALSE){
  matches = pct::pct_regions_lookup %>% dplyr::filter(lad16nm == LAN)
  
  # No matches found
  if(nrow(matches) == 0){
    # No exact match found, try find best match. 
    message(paste0("No exact Local Authority match found for: ", LAN, 
                   " so searching for a match now..."))
    
    # Score matches against provided LAN
    matches = utils::adist(pct::pct_regions_lookup$lad16nm, LAN)
    # Identify most similar place ID against LAN provided
    best_match_id  = which(matches == min(matches, na.rm = TRUE))
    # Identify best matched place using place ID
    best_matched_place = pct::pct_regions_lookup[best_match_id, ]
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
    message(paste0("Local authority ", LAN, " has been exactly matched to",
                   " region-name: ", RN))
    return(RN)
  }
}

#TODO: Investigate the coverage of national cycling routes, following on from Martin's comments on GitHub. 

# Workflow below:

# Setup params ------------------------------------------------------------

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
creation_date = "2022_10_04"
GAIST_eng_latest_dir = 'GAIST_eng_latest/'


# Generate GAIST networks below -------------------------------------------

# Local Authority name
LA_name = "Coventry"
# Gets region name that matches a pct region name from LA_name
region_name = get_region_name(LA_name, auto_match = TRUE)

# Use city centre coords for 2 km network buffer 
hackney_point = tmaptools::geocode_OSM("Hackney, London")
leicester_point = tmaptools::geocode_OSM("Leicester, Leicestershire")
leeds_point = tmaptools::geocode_OSM("Leeds, West Yorkshire") 
conventry_point = tmaptools::geocode_OSM("Coventry")
bristol_point = tmaptools::geocode_OSM("Bristol")

# Define spatial buffer (2km around city centre)
region_centre_point = sf::st_sfc(sf::st_point(conventry_point[["coords"]]), 
                                 crs = "EPSG:4326")
region_buffer = stplanr::geo_buffer(region_centre_point, dist = 2000) #2km radius


# Get pct routes
rnet_pct = pct::get_pct_rnet(region_name)
rnet_pct_fast = pct::get_pct_routes_fast(region_name)
rnet_pct_quiet = pct::get_pct_routes_quiet(region_name)

# Join the govtarget_slc from fast routes to quiet (NOT SURE THIS IS OKAY? THINK IT IS)
rnet_pct_quiet_joined = dplyr::left_join(sf::st_drop_geometry(rnet_pct_quiet), sf::st_drop_geometry(rnet_pct_fast), by="id", all.x = TRUE)

# Re-apply geometry column
rnet_pct_quiet_joined = sf::st_sf(rnet_pct_quiet_joined, geometry = rnet_pct_quiet$geometry)

# Get top 10% of respective routes: ---------------------------------------

# Default Routes
rnet_pct_top10 = rnet_pct %>% 
  top_frac(n = 0.1, wt = rnet_pct$govtarget_slc)
# Fast Routes
rnet_pct_fast_top10 = rnet_pct_fast %>% 
  top_frac(n = 0.1, wt = rnet_pct_fast$govtarget_slc)
# Quiet Routes
rnet_pct_quiet_top10 = rnet_pct_quiet_joined %>% 
  top_frac(n = 0.1, wt= rnet_pct_quiet_joined$govtarget_slc)



# Apply region buffer to pct networks.  -----------------------------------
# Fast routes
rnet_pct_fast_network = rnet_pct_fast[region_buffer, op=st_within]
rnet_pct_fast_network_top10 = rnet_pct_fast_top10[region_buffer, op=st_within]
# Quiet routes
rnet_pct_quiet_network = rnet_pct_quiet_joined[region_buffer, op=st_within]
rnet_pct_quiet_network_top10 = rnet_pct_quiet_top10[region_buffer, op=st_within]
# Default routes
rnet_pct_default_network = rnet_pct[region_buffer, op=st_within]
rnet_pct_default_network_top10 = rnet_pct_top10[region_buffer, op=st_within]


# Save PCT routes. --------------------------------------------------------
GAIST_network_dir = 'GAIST_Networks/'
region_filename = paste0(LA_name, "_central_", region_name,"/")
base_dir = paste0(GAIST_network_dir, region_filename)
dir.create(base_dir, showWarnings = FALSE)

# Save the buffered (2km pct routes for default, fast & quiet)
sf::st_write(rnet_pct_fast_network, paste0(base_dir, "pct_routes_fast.geojson"), append = FALSE)
sf::st_write(rnet_pct_fast_network_top10, paste0(base_dir, "pct_routes_fast_top_10%.geojson"), append = FALSE)

sf::st_write(rnet_pct_quiet_network, paste0(base_dir, "pct_routes_quiet.geojson"), append = FALSE)
sf::st_write(rnet_pct_quiet_network_top10, paste0(base_dir, "pct_routes_quiet_top_10%.geojson"), append = FALSE)

sf::st_write(rnet_pct_default_network, paste0(base_dir, "pct_routes_default.geojson"), append = FALSE)
sf::st_write(rnet_pct_default_network_top10, paste0(base_dir, "pct_routes_default_top_10%.geojson"), append = FALSE)


# Get and visualise top 10% of default, fast & quiet pct routes.  -----------------

rnet_pct_fast_network_top10 = rnet_pct_fast_network_top10 %>%
  dplyr::mutate(top_10_percent = "pct fast")
rnet_pct_quiet_network_top10 = rnet_pct_quiet_network_top10 %>%
  dplyr::mutate(top_10_percent = "pct quiet")
rnet_pct_default_network_top10 = rnet_pct_default_network_top10 %>%
  dplyr::mutate(top_10_percent = "pct default")

# Visualise

# tmap::tm_shape(rnet_pct_fast_network_top10) + 
#   tmap::tm_lines(col = "top_10_percent", palette = "red") + 
# tmap::tm_shape(rnet_pct_quiet_network_top10) + 
#   tmap::tm_lines(col = "top_10_percent", palette = "blue") + 
# tmap::tm_shape(rnet_pct_default_network_top10) + 
#   tmap::tm_lines(col = "top_10_percent", palette = "green") +
# tmap::tm_shape(region_buffer) + 
#   tmap::tm_polygons(alpha =0.45)

# Apply spatial buffer to pct routes --------------------------------------

# pct_routes are combined linestrings from the top 10% of all route types
# Select or remove routes from rbind below based on desired inclusion
pct_routes = rbind(rnet_pct_fast_network_top10[c("geometry", "top_10_percent")],
                   rnet_pct_quiet_network_top10[c("geometry", "top_10_percent")],
                   rnet_pct_default_network_top10[c("geometry", "top_10_percent")]) 

# Apply boundary to pct routes
pct_routes_buffer= stplanr::geo_buffer(pct_routes, dist=80) #m radius

# Don't use s2 spherical geometries, not quite working as expected (claimed
# that could not plot pct_routes_union due to invalid geometry, but all polygon
# geometries within pct_routes_buffer are VALID and sf::st_make_valid would fail
# to fix pct_routes_union when using s2 geometry)
sf::sf_use_s2(FALSE)

# Collect polygons into single MultiPolygon
pct_routes_union = pct_routes_buffer["geometry"] %>% sf::st_union()

# Acquire OSM data for given region ---------------------------------------

# Create DL directory if not already present & get england_latest file
fp = paste0(GAIST_eng_latest_dir, creation_date, "/")
dir.create(fp, showWarnings = FALSE)

# Get filepath for locally stored england-latest.osm.pbf file
eng_latest_fp = paste0(fp, "geofabrik_england-latest.osm.pbf")
check = file.exists(eng_latest_fp)

if ( ! check){
  # England-latest does not exist, so download.
  eng_dl = osmextract::oe_get(
    place = "England",
    extra_tags = required_tags,
    download_only = TRUE,
    skip_vectortranslate = TRUE,
    download_directory = fp)
}else{
  message("England-latest.osm.pbf file already downloaded")
}


# Acquire data for buffered regions
osm_region_data = osmextract::oe_read(
  file_path = eng_latest_fp,
  layer = "lines",
  vectortranslate_options = translate_options,
  never_skip_vectortranslate = TRUE,
  force_vectortranslate = TRUE,
  extra_tags = osm_tags,
  boundary = region_buffer,
  boundary_type = "clipsrc", 
  quiet = FALSE
)

# Apply oi_cycle_separation to get cycle specific infra
osm_region_cycle_infra = openinfra::oi_cycle_separation(osm_region_data, 
                                                        remove = TRUE)
# Visualise above

# tmap::tm_shape(osm_region_cycle_infra) +
#   tmap::tm_lines(col = "openinfra_cycle_infra") +
# tmap::tm_shape(pct_routes_union) +
#   tmap::tm_polygons(alpha = 0.45)

# When saving OSM networks, save them based on LTN protection level. That is:
# A] Protected space for cyclists (Kerb segregated, stepped track, ligh segregation)
# B] ON CARRAIGEWAY Cycle lanes (protected & mandatory)
# C] Mixed traffic (no dedicated cycle infrastructure, mixed with on road traffic)
#[A]
protected_cycle_infra = osm_region_cycle_infra %>% 
  dplyr::filter(openinfra_cycle_infra %in% c("road segregated - cycleway/track", 
                                             "road segregated - shared use cycleway",
                                             "appropriately smooth road segregated path"))
#[B]
cycle_lane_infra = osm_region_cycle_infra %>% 
  dplyr::filter(openinfra_cycle_infra %in% c("on road cycle lane",
                                             "on road cycle crossing"))
#[C]
mixed_traffic_infra = osm_region_cycle_infra %>% 
  dplyr::filter(openinfra_cycle_infra %in% c("On road mixed traffic @ 20 mph",
                                             "On road mixed traffic @ < 20 mph",
                                             "On road mixed traffic - NA maxspeed",
                                             "On road shared bus/taxi lane"))

# Combine the openinfra_OSM layers: 
combined_openinfra_OSM = rbind(protected_cycle_infra[c("geometry", "openinfra_cycle_infra")],
                               cycle_lane_infra[c("geometry", "openinfra_cycle_infra")],
                               mixed_traffic_infra[c("geometry", "openinfra_cycle_infra")])

# Save openinfra OSM cycle infrastructure within 2km city buffer:
# Protected
sf::st_write(protected_cycle_infra, paste0(base_dir, "openinfra_OSM_protected_cycle_infra.geojson"), append = FALSE)
# Cycle Lanes
sf::st_write(cycle_lane_infra, paste0(base_dir, "openinfra_OSM_cycle_lane_infra.geojson"), append = FALSE)
# Mixed Traffic
sf::st_write(mixed_traffic_infra, paste0(base_dir, "openinfra_OSM_mixed_traffic_cycle_infra.geojson"), append = FALSE)
#Combined layers
sf::st_write(combined_openinfra_OSM, paste0(base_dir, "openinfra_OSM_combined_layer.geojson"), append = FALSE)


# OSM openinfra cycle infra that intersects pct union polygon -------------

# (pct prioritised cycle infra selection)
pct_optimised_region_cycle_infra = osm_region_cycle_infra[pct_routes_union, ]

# Visualise the above
# tmap::tm_shape(pct_optimised_region_cycle_infra) +
#   tmap::tm_lines(col = "openinfra_cycle_infra") +
#   tmap::tm_shape(pct_routes_union) +
#   tmap::tm_polygons(alpha = 0.45)

#[A]
buffed_protected_cycle_infra = pct_optimised_region_cycle_infra %>% 
  dplyr::filter(openinfra_cycle_infra %in% c("road segregated - cycleway/track", 
                                             "road segregated - shared use cycleway",
                                             "appropriately smooth road segregated path"))
#[B]
buffed_cycle_lane_infra = pct_optimised_region_cycle_infra %>% 
  dplyr::filter(openinfra_cycle_infra %in% c("on road cycle lane",
                                             "on road cycle crossing"))
#[C]
buffed_mixed_traffic_infra = pct_optimised_region_cycle_infra %>% 
  dplyr::filter(openinfra_cycle_infra %in% c("On road mixed traffic @ 20 mph",
                                             "On road mixed traffic @ < 20 mph",
                                             "On road mixed traffic - NA maxspeed",
                                             "On road shared bus/taxi lane"))

# Combine the pct_optimised layers: 
combined_pct_optimised_openinfra_OSM = rbind(buffed_protected_cycle_infra[c("geometry", "openinfra_cycle_infra")],
                                             buffed_cycle_lane_infra[c("geometry", "openinfra_cycle_infra")],
                                             buffed_mixed_traffic_infra[c("geometry", "openinfra_cycle_infra")])


# Save pct optimised openinfra OSM cycle infrastructure within 2km city buffer
# Protected
sf::st_write(buffed_protected_cycle_infra, paste0(base_dir, "pct_optimised_openinfra_OSM_protected_cycle_infra.geojson"), append = FALSE)
# Cycle Lanes
sf::st_write(buffed_cycle_lane_infra, paste0(base_dir, "pct_optimised_openinfra_OSM_cycle_lane_infra.geojson"), append = FALSE)
# Mixed Traffic
sf::st_write(buffed_mixed_traffic_infra, paste0(base_dir, "pct_optimised_openinfra_OSM_mixed_traffic_cycle_infra.geojson"), append = FALSE)
# Combined Layers
sf::st_write(combined_pct_optimised_openinfra_OSM, paste0(base_dir, "pct_optimised_openinfra_OSM_combined_layer.geojson"), append = FALSE)


# diff = nrow(combined_openinfra_OSM)- nrow(combined_pct_optimised_openinfra_OSM)
# message("Applying the pct_optimised subsetting has remvoed ", diff, " features")

################################################################################

# Get cycle separation outputs for each pct_region ------------------------

# Get cycling infrastructure with oi_cycle_separation ---------------------
# 
##########################################################################
# General Workflow:
# 1 - Download England-latest.osm.pbf
# 2 - For each `area` in `list of areas`, get the OSM data for each area.
# 3 - Apply the oi_cycle_separation() function to get only cycle infra.
# 4 - Save each area as a .geojson, upload to GAIST releases ?
#
# If some form of prioritisation is required, utilise pct routes for this.
#    should this be done, need to make clear limitations of data
#    (2011 census & work commute data)
###########################################################################

# # Uncomment below
# 
# # regions = pct::pct_regions
# # r_names = regions$region_name
# # r_geoms = regions$geometry	
# # 
# # # Get england-latest data (to be subset for each region)
# # creation_date = "14_09_2022"
# # GAIST_eng_latest_dir = '/home/james/Desktop/LIDA_OSM_Project/openinfra/GAIST_eng_latest/'
# # fp = paste0(GAIST_eng_latest_dir, creation_date, "/")
# # 
# # # Create DL directory if not already present
# # dir.create(fp)
# # 
# # eng_dl = osmextract::oe_get(
# #   place = "England",
# #   extra_tags = required_tags,
# #   download_only = TRUE,
# #   skip_vectortranslate = TRUE, 
# #   download_directory = fp)
# # 
# # # Filepath for england_latest
# # eng_latest_fp = paste0(fp, "geofabrik_england-latest.osm.pbf")
# #   
# #   
# # for(i in 1:nrow(regions)){
# #   
# #   RN = r_names[i] # Region Name
# #   RG = r_geoms[i] # Region Geometry
# #   
# #   #TODO: Add check for existing files, don't want to save too many. 
# #   
# #   # Get OSM data using region geometry
# #   message("Getting ", RN, " region data.")
# #     
# #   translate_options = c(
# #     "-nlt", "PROMOTE_TO_MULTI",       # Check this
# #     "-where", "highway IS NOT NULL")   # Highway cannot be NA
# #     
# #   required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
# #                     "kerb", "footway", "sidewalk", "cycleway", "segregated", 
# #                     "highway", "crossing", "lit", "tactile_paving", "surface",
# #                     "smoothness", "width", "est_width", "lit_by_led", "ref",
# #                     "amenity", "cycleway_left", "cycleway_right", 
# #                     "cycleway_both", "separation"
# #                    )
# #   
# #   # Get region data  
# #   r_data = osmextract::oe_read(
# #     eng_latest_fp,
# #     vectortranslate_options = translate_options,
# #     layer = "lines",
# #     extra_tags = required_tags,
# #     boundary = RG,
# #     boundary_type = "clipsrc",
# #     quiet = TRUE
# #   )
# #     
# #   # Create cycle infra network
# #   cycle_infra_network = openinfra::oi_cycle_separation(r_data, remove=TRUE)
# #     
# #   # Save the network as a geojson?
# #   GAIST_fp = '/home/james/Desktop/LIDA_OSM_Project/openinfra/GAIST_cycle_infra_networks/'
# #   file_name = paste0(RN, "_cycle_infra_network.geojson")
# #     
# #   sf::st_write(cycle_infra_network, paste0(GAIST_fp, file_name))
# # }
# 
# # Visualisation of above networks ---------------------------------------
# 
# url = "https://github.com/udsleeds/openinfraresults/releases/download/cycle_infra/leicestershire_cycle_infra_network.geojson"
# test_network = sf::read_sf(url)
# test_network = test_network %>% dplyr::filter(! is.na(openinfra_cycle_infra))
# tmap::tmap_mode("view")
# 
# tmap::qtm(test_network)
# 
# tmap::tm_shape(test_network) +
#   tmap::tm_lines(col = "openinfra_cycle_infra")
