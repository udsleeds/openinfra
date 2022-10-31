# Test script to demo final version of oi_cycle_routes. 

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

region_name = "England"

# Get data ----------------------------------------------------------------

options(timeout = 1000) # Un-comment if download fails due to timeout. 

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

england_osm_lines_other_rels = osmextract::oe_get(
  place = region_name,
  extra_tags = required_tags,
  max_file_size = 9e+09,
  layer = "other_relations"
)

# Combine "multilinestring" & "other_relations" layers
england_relations = rbind(england_osm_line_rels,
                          england_osm_lines_other_rels)


# Load and test function. -------------------------------------------------

devtools::load_all()

#cycle_routes_output = oi_cycle_routes(england_osm_lines, england_relations, remove=FALSE)

clean_cycle_routes_output = oi_cycle_routes(england_osm_lines, england_relations, remove=TRUE)

tmap::tmap_mode("view")
map = tmap::tm_shape(clean_cycle_routes_output) + 
  tmap::tm_lines(col = "openinfra_cycle_routes")
tmap::tmap_save(map, "~final_test_map.html")

#tmap::qtm(clean_cycle_routes_output)
