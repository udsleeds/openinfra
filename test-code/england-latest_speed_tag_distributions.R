

# Library set-up ----------------------------------------------------------

pkgs = c("sf",
         "osmextract",
         "tidyverse",
         "tmap",
         "openinfra"
        )
# Load packages
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]


# Parameters --------------------------------------------------------------

required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
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

lit_way_vals = c("05:00-00:00", "05:00-dawn;dusk-24:00", "24/7", "automatic",
                 "both", "dusk-dawn", "sunset-00:00,05:00-sunrise UTC", "yes"
                )

region_name = "England"


# Get data ----------------------------------------------------------------

england_osm = osmextract::oe_get(
  place = region_name,
  extra_tags = required_tags,
  max_file_size = 9e+09,
  layer = "lines"
) 

england_osm_lines = england_osm 

england_osm_line_rels = osmextract::oe_get(
  place = region_name,
  extra_tags = required_tags,
  max_file_size = 9e+09,
  layer = "multilinestrings"
)

#england_osm_poly_rels = osmextract::oe_get(
#  place = region_name,
#  extra_tags = required_tags,
#  max_file_size = 9e+09,
#  layer = "multipolygons"
#)

england_osm_other_rels = osmextract::oe_get(
  place = region_name,
  extra_tags = required_tags,
  max_file_size = 9e+09,
  layer = "other_relations"
)


# investigate lcn, ncn & _ref tags ----------------------------------------

# These will be the common values to use as names, or to be parsed before being
# used as a route name. 
ways_lcn_vc = as.data.frame(table(england_osm_lines$lcn))
ways_lcn_ref_vc = as.data.frame(table(england_osm_lines$lcn_ref))
rels_lcn_vc = as.data.frame(table(england_osm_line_rels$lcn))
rels_lcn_ref_vc = as.data.frame(table(england_osm_line_rels$lcn_ref))

ways_ncn_vc = as.data.frame(table(england_osm_lines$ncn))
ways_ncn_ref_vc = as.data.frame(table(england_osm_lines$ncn_ref))
rels_ncn_vc = as.data.frame(table(england_osm_line_rels$ncn))
rels_ncn_ref_vc = as.data.frame(table(england_osm_line_rels$ncn_ref))

test = england_osm_lines %>% dplyr::filter(lcn == "cambscc_wisbech_rural")

# NCN and LCN routes from relations layer.
ncn_yes_rels = england_osm_line_rels %>% dplyr::filter(ncn == "yes")
lcn_yes_rels = england_osm_line_rels %>% dplyr::filter(lcn == "yes")

lcn_rels_noNA = england_osm_line_rels %>% dplyr::filter(!is.na(lcn))

# Get each of "lines" lcn & ncn not NA routes, AND each of "mutlilinestrings" lcn & ncn routes,
# Then rbind all four of them to make a totoal one. 

way_lcns = england_osm_lines %>% dplyr::filter(! is.na(lcn))
way_ncns = england_osm_lines %>% dplyr::filter(! is.na(ncn))

relation_lcns = england_osm_line_rels %>% dplyr::filter(! is.na(lcn))
relation_ncns = england_osm_line_rels %>% dplyr::filter(! is.na(ncn))


# Value counts for lcn, ncn, lcn_ref, ncn_ref, name & cycle_network --------

# Value counts for lcn, ncn, lcn_ref & ncn_ref values from "ways" layer
ncn_way_vcs = as.data.frame(table(way_ncns$ncn))
lcn_way_vcs = as.data.frame(table(way_lcns$lcn))

ncn_ref_way_vcs = as.data.frame(table(way_lcns$ncn_ref))
lcn_ref_way_vcs = as.data.frame(table(way_lcns$lcn_ref))

# Value counts for lcn & ncn values from "relations"
ncn_rel_vcs = as.data.frame(table(relation_ncns$ncn))
lcn_rel_vcs = as.data.frame(table(relation_lcns$lcn))

ncn_ref_rel_vcs = as.data.frame(table(relation_ncns$ncn_ref))
lcn_ref_rel_vcs = as.data.frame(table(relation_lcns$lcn_ref))

ncn_name_vcs = as.data.frame(table(relation_ncns$name))
lcn_name_vcs = as.data.frame(table(relation_lcns$name))

ncn_cycle_network_vcs = as.data.frame(table(relation_ncns$cycle_network))
lcn_cycle_network_vcs = as.data.frame(table(relation_lcns$cycle_network))


# rbind the ways & relation layers ----------------------------------------
# rbind all four, selectuing only relevnt columns.

ways = rbind(way_lcns, way_ncns)
relations = rbind(relation_lcns, relation_ncns)

rows = c("osm_id", "lcn", "ncn", "lcn_ref", "ncn_ref", "name", "geometry")
routes = rbind(ways %>% dplyr::select(rows), relations %>% dplyr::select(rows))

# Gets results tagged as both LCN & NCN
lcn_ncn_yes = routes %>% dplyr::filter(!is.na(lcn) & !is.na(ncn))
ncn_route1 = routes %>% dplyr::filter(ncn_ref == "(1)")

ncn_other_rels = england_osm_other_rels %>% dplyr::filter(!is.na(ncn))
ncn_ref_other_rels = england_osm_other_rels %>% dplyr::filter(!is.na(ncn_ref))
other_rel_type_vc = as.data.frame(table(england_osm_other_rels$type))

other_rel_superroutes = england_osm_other_rels %>% dplyr::filter(type == "superroute")

# Remove waterways & aerial ways ------------------------------------------
# Below looks at the lit tags for highway="residential"
message(nrow(england_osm))
england_osm = england_osm %>% dplyr::filter(! is.na(highway))
message(nrow(england_osm))

# All highway=residential ways in England
eng_res_highway = england_osm %>% dplyr::filter(highway == "residential")
# All highway=residential ways in England that ARE lit
lit_eng_res_highway = eng_res_highway %>% dplyr::filter(lit %in% lit_way_vals)

# Remove residential ways with NA maxspeed (unknown condition)
eng_res_highway_parsed = eng_res_highway %>% dplyr::filter(! is.na(maxspeed))
lit_eng_res_highway_parsed = lit_eng_res_highway %>% dplyr::filter(! is.na(maxspeed))

# Parse maxspeed column as number
eng_res_highway_parsed[["maxspeed"]] = eng_res_highway_parsed[["maxspeed"]] %>% parse_number()
lit_eng_res_highway_parsed[["maxspeed"]] = lit_eng_res_highway_parsed[["maxspeed"]] %>% parse_number()

# Get value count of all maxspeeds (Visualisation)
eng_res_highway_speeds = as.data.frame(table(eng_res_highway_parsed$maxspeed))
lit_eng_res_highway_speeds = as.data.frame(table(lit_eng_res_highway_parsed$maxspeed))

# Find highway="residential" maxspeed combinations
good_res = eng_res_highway_parsed %>% dplyr::filter(maxspeed <= 30)
too_fast_res = eng_res_highway_parsed %>% dplyr::filter(maxspeed > 30)
too_fast_res_lit = lit_eng_res_highway_parsed %>% dplyr::filter(maxspeed > 30)
good_res_lit = lit_eng_res_highway_parsed %>% dplyr::filter(maxspeed <= 30)
  
# We can see that there are 2,610 ways tagged highway="residential" in England that
# have a maxspeed value exceeding 30 mph. Thus, it is probably not a safe assumption
# to automatically assign highway=residential as 30 mph, though this is expected given 
# the extensive range of highway=residential across urban and rural areas

# However, a safer assumption may be the tagging of highway="residential" ways that
# are lit. By definition, any residential street that is lit should have a maxspeed of
# 30 mph, unless changed by a local order. 

# From `too_fast_res_lit`, we observe the number of highway=residential ways that 
# do have lighting and a maxspeed exceeding 30 mph. Specifically 40, 50 and 60. The
# likely result of this are new residential builds springing up off of the side of
# primary roads possessing that speed limit. 
# For an example, please see https://user-images.githubusercontent.com/58815827/194595370-049dcb4c-e220-49e0-ab7a-9e693407b41a.png

# Given discussions within #147 (udsleeds/openinfra), it is reasonable to make the 
# assumption that cases described in the previous paragraph will have correct
# maxspeed values entered to reflect their exception from the norm, that is that
# a 30 mph speed limit should apply. 

# Noting the obvious exception that similar areas to those described two paragraphs
# above WILL exist, and WILL NOT have been mapped with an appropriate, extraordinary
# maxspeed value, at least yet..., it would be reasonable to infer a maximum speed of
# 30 mph on ways tagged highway=residential, lit=yes & maxspeed=NA to be added as a 
# SEPERATE column to the data packs (clearly stating this is an inferred speed, not input).

# As time passes, OSM will be updated with correct maxspeed values as these areas 
# become mapped, at which point the maxspeed==NA condition (to infer a maxspeed)
# will not be met, and the OSM entered maxspeed value will instead prevail and no
# maxspeed needs to be inferred.


# Proposed test case for England-latest -----------------------------------

lit_eng_res_highway = lit_eng_res_highway %>% 
  dplyr::mutate(inferred_maxspeed = dplyr::case_when(
    # as we are using `lit_eng_res_highway` we have already met the condition
    # of highway=residential & highway is lit... need maxspeed is NA
    is.na(maxspeed) ~ "i30 mph"
  ))

# i30 mph noting this speed is inferred
message("# inferred speeds:", nrow(lit_eng_res_highway %>% dplyr::filter(inferred_maxspeed == "i30 mph")))

# Visualise the inferred 30 mph maxspeeds
tmap::tmap_mode("view")

tmap::tm_shape(lit_eng_res_highway %>% dplyr::filter(inferred_maxspeed == "i30 mph")) + 
  tmap::tm_lines(col = "inferred_maxspeed")




# Look at coverage of flush kerb & tactile paving  ------------------------

england_osm_im = england_osm #%>% dplyr::select(kerb, tactile_paving)

eng_osm_im_noNA = england_osm_im %>% dplyr::filter(! is.na(kerb) & ! is.na(tactile_paving))

eng_fully_im_kerbs = eng_osm_im_noNA %>% dplyr::filter(kerb %in% c("flush", "no"))
eng_fully_im_kerbs = eng_fully_im_kerbs %>% dplyr::filter(tactile_paving == "yes")

leeds_centre_point = sf::st_sfc(sf::st_point(c(-1.549, 53.801)), 
                                crs = "EPSG:4326")
leeds_buffer = sf::st_buffer(leeds_centre_point, dist = 2000) #2000 metre radius

leeds = eng_fully_im_kerbs[leeds_buffer, ]


tact_paving_vc = as.data.frame(table(eng_osm_im_noNA$tactile_paving))



test_output = openinfra::oi_im_flush_kerb(openinfra::example_data)
test_output = test_output %>% dplyr::filter(openinfra_im_tactile_paving == "yes")
test_output = test_output %>% dplyr::filter(openinfra_im_flush_kerb == "yes")
