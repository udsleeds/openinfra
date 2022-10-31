# TfL CiD Comparison Script. 

# Load libraries.  --------------------------------------------------------

pkgs = c("osmextract",
         "openinfra",
         "tmap",
         "sf",
         "dplyr"
         )

lapply(pkgs, library, character.only=TRUE)[length(pkgs)]

# Load CycleInfraLdn (Caroline Tait PhD project)
devtools::install_github("PublicHealthDataGeek/CycleInfraLnd")
library("CycleInfraLnd")

# Play with CycleInfraLnd -------------------------------------------------

# Test function, prints "Hello, World!"
hello_world = CycleInfraLnd::hello()

# Gets CiD linestring assets: type = c("advanced_stop_line", "crossing",
#                                      "cycle_lane_track", "restricted_route")
?CycleInfraLnd::get_cid_lines()

# Gets CiD node assets: type = c("signal", "cycle_parking", "restricted_point",
#                                "signage", "traffic_calming")
?CycleInfraLnd::get_cid_points()

################################################################################
# CiD has two data types for assets - linestrings and nodes.                   #
#                                                                              #
# - NODES                                                                      #
#    - Signals,                                                                #
#      Cycle Parking, [oi_cycle_parking]                                       #
#      Restricted Point(steps & lifts),                                        #
#      Signage,                                                                #
#      Traffic Calming Features.                                               #
#                                                                              #
# - LINESTRINGS                                                                #
#    - Advanced Stop Lines (ASL),                                              #
#      Crossings,                                                              #
#      Cycle Lane/Track, [oi_cycle_infra]                                      #
#      Restricted Routes (Stairs, Lifts, Pedestrian only ways etc.)            #
#                                                                              #
# After laoding required data, investigate the coverage of each asset type     #
#   where possible.                                                            #
################################################################################

# Load data ---------------------------------------------------------------
# Load CiD - Greater London polygon, OSM data for Greater London

# Greater London Polygon
regions_2022 = sf::read_sf(paste0("https://github.com/udsleeds/openinfra/",
                                  "releases/download/0.4.2/transport_",
                                  "authorities_2022.geojson"))

greater_london = regions_2022 %>% dplyr::filter(Name == "Greater London") %>% 
                    dplyr::select(geometry)

## Load CiD data ##

# CiD Linestrings
CiD_ASL = CycleInfraLnd::get_cid_lines(type = "advanced_stop_line")
CiD_crossings = CycleInfraLnd::get_cid_lines(type = "crossing")
CiD_cycle_infra = CycleInfraLnd::get_cid_lines(type = "cycle_lane_track")
CiD_restricted_lines = CycleInfraLnd::get_cid_lines(type = "restricted_route")

# CiD Nodes
CiD_signal = CycleInfraLnd::get_cid_points(type = "signal")
CiD_parking = CycleInfraLnd::get_cid_points(type = "cycle_parking")
CiD_restricted_points = CycleInfraLnd::get_cid_points(type = "restricted_point")
CiD_signs = CycleInfraLnd::get_cid_points(type = "signage")
CiD_calming = CycleInfraLnd::get_cid_points(type = "traffic_calming")

# Get OSM data
OSM_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                   "kerb", "footway", "sidewalk", "cycleway", "segregated",
                   "highway", "crossing", "lit", "tactile_paving", "surface",
                   "smoothness", "width", "est_width", "lit_by_led", "ref",
                   "amenity", "sidewalk", "sidewalk:left", "sidewalk:right",
                   "sidewalk:both", "source:maxspeed", "maxspeed:type",
                   "zone:maxspeed", "zone:traffic", "maxspeed", "HFCS", "rural",
                   "cycleway_left", "cycleway_right", "cycleway_both",
                   "separation", "lcn", "lcn_ref", "ncn", "ncn_ref", "type",
                   "route", "network", "cycle_network"
)

OSM_lines_data = osmextract::oe_get(
  place = "Greater London",
  layer = "lines",
  extra_tags = OSM_tags,
  boundary = greater_london,
  boundary_type = "clipsrc",
  quiet = FALSE
)
# Remove NULL highways: 
OSM_lines_data = OSM_lines_data %>% dplyr::filter(! is.na(highway))


OSM_point_data = osmextract::oe_get(
  place = "Greater London",
  layer = "points",
  extra_tags = OSM_tags, 
  boundary = greater_london,
  boundary_type = "clipsrc", 
  quiet = FALSE
)

# Test visualisation
tmap::tmap_mode("view")
tmap::tm_shape(OSM_lines_data) + 
  tmap::tm_lines(col = "highway") +
  tmap::tm_layout(title = "Test Visualisation") + 
tmap::tm_shape(greater_london) + 
  tmap::tm_polygons(alpha = .45)


# Analysis Below:  --------------------------------------------------------


# ASL Analysis ------------------------------------------------------------
# https://bikedata.cyclestreets.net/tflcid/conversion/#advanced_stop_line
# ASL within TfLCiD are Linestrings, but within OSM they are Nodes

# Additionally, OSM ASLs make no distinction on type of ASL, whereas TfLCiD 
# contains a number of ASLs (Feeder Lane, Left Feeder, Right Feeder, Centre 
#                            Feeder, Lane Colour & Shared (becomes the road) 
#                            hence no proper protection.)
# We must compare all OSM ASLs to ALL TfLCiD ASLs, currently there is no 
# distinction on ASL type in OSM, so treat all as one. 

# Get all TfLCiD ASL types (Feeder & No Feeder Lanes). 
OSM_ASL = OSM_point_data %>% dplyr::filter(cycleway == "asl") 

# Feeder lane / No feeder lane breakdown (if needed...)
################################################################################
CiD_ASL_feeders = CiD_ASL %>% dplyr::filter(ASL_FDR == TRUE |
                                            ASL_FDRLFT == TRUE |
                                            ASL_FDCENT == TRUE |
                                            ASL_FDRIGH == TRUE |
                                            ASL_SHARED == TRUE )

CiD_ASL_no_feeder = CiD_ASL %>% dplyr::filter(ASL_FDR == FALSE & 
                                          ASL_FDRLFT == FALSE &
                                          ASL_FDCENT == FALSE &
                                          ASL_FDRIGH == FALSE &
                                          ASL_SHARED == FALSE )
################################################################################


# Test visualisations
tmap::tm_shape(CiD_ASL) + 
  tmap::tm_lines(col = "ASL_COLOUR", palette = "red") + 
tmap::tm_shape(OSM_ASL) + 
  tmap::tm_dots(col = "cycleway", alpha = 0.35)


# IDEA - Create an n meter buffer around each OSM ASL node, then count how many 
# CiD ASL linestrings are entirely within or intersect this -  these are features
# mapped in both instaces, the st_disjoin are features present in only one dataset.

# In other words, they are complimentary datasets. 


# Cycle Infra  ------------------------------------------------------------

OSM_Cycleinfra = openinfra::oi_cycle_separation(OSM_lines_data, remove = TRUE)

nrow(OSM_Cycleinfra)
nrow(CiD_cycle_infra)

tmap::tm_shape(OSM_Cycleinfra) + 
  tmap::tm_lines(col = "highway", palette = "blue") + 
tmap::tm_shape(CiD_cycle_infra) + 
  tmap::tm_lines(col = "CLT_CARR", palette = "red")

difference = sf::st_difference(OSM_Cycleinfra, CiD_cycle_infra)
