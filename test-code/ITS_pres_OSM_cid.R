# OSM CID SCRIPT

# Install CycleInfraLdn to obtain CiD data. 
devtools::install_github("PublicHealthDataGeek/CycleInfraLnd")

library(CycleInfraLnd)
library(osmextract)
library(openinfra)


# CycleInfraLnd Functions -------------------------------------------------

CiD_line_types = c("advanced_stop_line",
                   "crossing",
                   "cycle_lane_track",
                   "restricted_route")
# Get CiD data on linestrings
CycleInfraLnd::get_cid_lines()

CiD_point_types = c("signal",
                    "restricted_parking",
                    "signage",
                    "traffic_calming")

# Get CiD data on points
CycleInfraLnd::get_cid_points()

# Prints "Hello, world!"
CycleInfraLnd::hello()



# Get CiD data with CycleInfraLnd -----------------------------------------

# Linestrings first
CiD_asl = get_cid_lines(type = "advanced_stop_line")
CiD_crossing = get_cid_lines(type = "crossing")
CiD_cycle_lane_track = get_cid_lines(type = "cycle_lane_track")
CiD_restricted_route = get_cid_lines(type = "restricted_route")

# Points next
CiD_signal = get_cid_points(type = "signal")
CiD_restricted_route = get_cid_points(type = "restricted_point")
CiD_signage = get_cid_points(type = "signage")
CiD_traffic_calming = get_cid_points(type = "traffic_calming")


# Get Greater London polygon for osmextract query -------------------------

lads = sf::read_sf("data-small/Local_Authority_Districts_(May_2022)_UK_BSC.geojson")
lad_ta_region_lookup = read_csv("data-small/lad_ta_region_lookup_atf3.csv")

ta_regions_2022 = sf::read_sf("https://github.com/udsleeds/openinfra/releases/download/0.4.2/transport_authorities_2022.geojson")
greater_ldn_boundary = ta_regions_2022 %>% dplyr::filter(Name == "Greater London")

# Compare CiD_cycle_lane_track to Greater London area ---------------------

tmap::tm_shape(CiD_cycle_lane_track) + 
  tmap::tm_lines(palette = c("red")) + 
tmap::tm_shape(greater_ldn_boundary) + 
  tmap::tm_polygons(alpha = 0.45)

# Visualising the above we can see that all CiD data is contained within the 
# greater_ldn polygon. So, we can confidently use greater_ldn as a boundary 
# option within oe_get() to clip data to this specific region. 


# Get OSM data with osmextract --------------------------------------------

# Required tags
required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                  "kerb", "footway", "sidewalk", "cycleway", "segregated",
                  "highway", "crossing", "lit", "tactile_paving", "surface",
                  "smoothness", "width", "est_width", "lit_by_led", "ref",
                  "amenity", "sidewalk", "sidewalk:left", "sidewalk:right",
                  "sidewalk:both", "source:maxspeed", "maxspeed:type",
                  "zone:maxspeed", "zone:traffic", "maxspeed", "HFCS", "rural",
                  "cycleway_left", "cycleway_right", "cycleway_both",
                  "separation", "lcn", "lcn_ref", "ncn", "ncn_ref", "type",
                  "route", "network", "cycle_network")

# Download data
osm_ldn_lines = osmextract::oe_get(
  place = greater_ldn_boundary,
  layer = "lines",
  extra_tags = required_tags,
  boundary = greater_ldn_boundary,
  boundary_type = "clipsrc",
  quiet = FALSE
)

osm_ldn_points = osmextract::oe_get(
  place = greater_ldn_boundary,
  layer = "points",
  extra_tags = required_tags,
  boundary = greater_ldn_boundary,
  boundary_type = "clipsrc",
  quiet = FALSE
)



# Compare Data ------------------------------------------------------------

# Advanced Stop Line Fields (https://bikedata.cyclestreets.net/tflcid/conversion/#advanced_stop_line)

# Feeder Lane - asl_fdr - OSM treats ASLs as a node, but the CID has them as a line.
#                         No tag for feeder lane exists yet, other than putting the lane on the adjacent highway. 
# feeder lane - asl_fdr : Use nodes, cycleway=asl for assessment of numbers. 

# Feeder Lane in Centre - asl_fdcent - No such tag exists, so proposed tag here, and not necessary of significant use.
# Investigate

ldn_point_asl = osm_ldn_points %>% dplyr::filter(cycleway == "asl")

# Visualise OSM Nodes
tmap::tm_shape(CiD_asl) + 
  tmap::tm_lines() + 
tmap::tm_shape(ldn_point_asl) +
  tmap::tm_dots( alpha = 0.25)


view = CiD_asl %>% dplyr::filter(FEATURE_ID == "RWG150449")

n_CiD_asl = nrow(CiD_asl)
n_osm_point_asl = nrow(ldn_point_asl)

message("Greater London area asl points - CiD: ",n_CiD_asl," & OSM: ",n_osm_point_asl  )
message("A difference of ", (n_CiD_asl-n_osm_point_asl), " asl features" )


# Visual comparison of ASL from CiD & OSM:
# file:///home/james/Pictures/Screenshots/Screenshot%20from%202022-10-12%2015-25-43.png



# Cycle Lnae/Tacks

tmap::qtm(CiD_cycle_lane_track)
