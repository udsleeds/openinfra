# Generate package test-data. 
library("osmextract")
library("tmap")
library("dplyr")
library("sf")

devtools::load_all()


# All extra tags - by requesting all of these extra_tags, you can
# later specify your own network ("cycling", "walking", "driving")
# as all necessary tags are within all_extra_tags
all_extra_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
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
#_______________________________________________

# Set place name
place_name = "Leeds"

# Checks for best provider given place
place_match = oe_match(place_name)

# Detects perfect match from a provider and sets provider=perfect_match
if (exists("place_match")) {
  if (grepl("bbbike", place_match[1])) {
    provider = "bbbike"
  } else if (grepl("geofabrik", place_match[1])) {
    provider = "geofabrik"
  }
} else {
  print("Exact match not found with providers")
}
print(c(place_name,"provider is:",  provider))


# (Long, Lat) coords of desired buffer place (Leeds City Centre)
coords = c(-1.548567, 53.801277)
# Specify Buffer Radius
radius = 2500 # <- meters - 5km

# Define spatial buffer (2km around city centre)
region_centre_point = sf::st_sfc(sf::st_point(coords), 
                                 crs = "EPSG:4326")
region_buffer = stplanr::geo_buffer(region_centre_point, dist = radius) 


# Total dataset for leeds - no travel mode specified
download_data = osmextract::oe_get(
  place = place_name,
  provider = provider,
  boundary = region_buffer,
  boundary_type = "clipsrc",
  force_download = TRUE,
  quiet = FALSE,
  download_only = TRUE,
  download_directory = "../"
)


# Read each data layer  ---------------------------------------------------

total_place_lines = osmextract::oe_get(
  place = place_name,
  provider = provider,
  layer = "lines",
  extra_tags = all_extra_tags,
  never_skip_vectortranslate = TRUE,
  boundary = region_buffer,
  boundary_type = "clipsrc",
  quiet = FALSE
)


total_place_nodes = osmextract::oe_get(
  place = place_name,
  provider = provider,
  layer = "points",
  extra_tags = all_extra_tags,
  never_skip_vectortranslate = TRUE,
  boundary = region_buffer,
  boundary_type = "clipsrc",
  quiet = FALSE
)

total_place_rels = osmextract::oe_get(
  place = place_name,
  provider = provider,
  layer = "multilinestrings",
  extra_tags = all_extra_tags,
  never_skip_vectortranslate = TRUE,
  boundary = region_buffer,
  boundary_type = "clipsrc",
  quiet = FALSE
)

total_place_other_rels = osmextract::oe_get(
  place = place_name,
  provider = provider,
  layer = "other_relations",
  extra_tags = all_extra_tags,
  never_skip_vectortranslate = TRUE,
  boundary = region_buffer,
  boundary_type = "clipsrc",
  quiet = FALSE
)


# Data cleaning -----------------------------------------------------------


# Remove NA highway values
total_place_lines = total_place_lines %>% dplyr::filter(! is.na(highway))


# Lines visualisation
tmap::tm_shape(osm_sf_lines) + 
  tmap::tm_lines() + 
tmap::tm_shape(region_buffer) + 
  tmap::tm_polygons(alpha=.35)


# Save package data -------------------------------------------------------
# Openinfra uses "example_*_data" for package data so rename. 

example_data = total_place_lines
example_data_pois = total_place_nodes 
example_data_rels = total_place_rels
example_data_other_rels = total_place_other_rels

usethis::use_data(example_data, overwrite = TRUE)
usethis::use_data(example_data_pois, overwrite = TRUE)
usethis::use_data(example_data_rels, overwrite = TRUE)
usethis::use_data(example_data_other_rels, overwrite = TRUE)
