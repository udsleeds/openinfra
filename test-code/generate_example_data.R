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
download_osm_data = osmextract::oe_get(
  place = place_name,
  provider = provider,
  boundary = circle_buffer,
  boundary_type = "clipsrc",
  never_skip_vectortranslate = TRUE,
  force_download = TRUE,
  quiet = FALSE,
  download_only = TRUE,
  download_directory = "../"
)

total_place_lines = osmextract::oe_read(
  file_path = "../",
  layer = "lines",
  boundary = circle_buffer,
  boundary_type = "clipsrc",
  extra_tags = all_extra_tags
)

total_place_nodes = osmextract::oe_read(
  file_path = "../",
  layer = "points",
  boundary = circle_buffer,
  boundary_type = "clipsrc",
  extra_tags = all_extra_tags
)

total_place_rels = osmextract::oe_read(
  file_path = "../",
  layer = "multilinestrings",
  boundary = circle_buffer,
  boundary_type = "clipsrc",
  extra_tags = all_extra_tags
)

total_place_other_rels = osmextract::oe_read(
  file_path = "../",
  layer = "other_relations",
  boundary = circle_buffer,
  boundary_type = "clipsrc",
  extra_tags = all_extra_tags
)

# total_place = osmextract::oe_get(
#   place = place_name,
#   provider = provider,
#   boundary = circle_buffer,
#   boundary_type = "clipsrc",
#   layer = "lines",
#   never_skip_vectortranslate = TRUE,
#   force_download = TRUE,
#   quiet = FALSE,
#   extra_tags = all_extra_tags
# )

# total_place_downloaded_lines = osmextract::oe_read(
#   file_path = "/home/james/Desktop/LIDA_OSM_Project/osm_pbf_files/england-latest.osm.pbf",
#   layer = "lines",
#   boundary = circle_buffer,
#   boundary_type = "clipsrc",
#   extra_tags = all_extra_tags
# )

# total_place_nodes = osmextract::oe_read(
#   file_path = "/home/james/Desktop/LIDA_OSM_Project/osm_pbf_files/england-latest.osm.pbf",
#   layer = "points",
#   boundary = circle_buffer,
#   boundary_type = "clipsrc",
#   extra_tags = all_extra_tags
# )
# 
# total_place_relations = osmextract::oe_read(
#   file_path = "/home/james/Desktop/LIDA_OSM_Project/osm_pbf_files/england-latest.osm.pbf",
#   layer = "multilinestrings",
#   boundary = circle_buffer,
#   boundary_type = "clipsrc",
#   extra_tags = all_extra_tags
# )
#   
# total_place_other_relations = osmextract::oe_read(
#   file_path = "/home/james/Desktop/LIDA_OSM_Project/osm_pbf_files/england-latest.osm.pbf",
#   layer = "other_relations",
#   boundary = circle_buffer,
#   boundary_type = "clipsrc",
#   extra_tags = all_extra_tags
# )

total_place_lines = total_place_lines %>% dplyr::filter(! is.na(highway))


# Re-apply spatial buffers 
osm_sf_nodes = total_place_nodes[circle_buffer, ]
osm_sf_lines = total_place_lines[circle_buffer, ]
osm_sf_rels = total_place_rels[circle_buffer, ]
osm_sf_other_rels = total_place_rels[circle_buffer, ]


#__________________FUNCTION___
# This script may actually want deleting now - package data will be made using 
# the generate_package_data.R script rather than this. 
#_____________________________

# SOTM2022 Plots # 

# UNCOMMENT BELOW TO SAVE NEW EXAMPLE_DATA
#usethis::use_data(example_data)


# Create data packs -------------------------------------------------------

osm_sf_road_recoded = oi_road_names(osm_sf_lines) # recode_road_class(osm_sf) #openinfra::recode_road_class(osm_sf)
data_pack = oi_clean_maxspeed_uk(osm_sf_road_recoded) #openinfra::oi_clean_maxspeed_uk(osm_sf_road_recoded)
data_pack_lit = oi_is_lit(data_pack)


data_pack = oi_road_names(osm_sf_lines)
data_pack_road_name = oi_road_names(osm_sf_lines)
data_pack_cycle = oi_active_cycle(osm_sf_lines)
data_pack_walk = oi_active_walk(osm_sf_lines)
data_pack_maxspeed = oi_clean_maxspeed_uk(osm_sf_lines)
#data_pack_IM = oi_inclusive_mobility(osm_sf_lines)
data_pack_im_flush_kerb = oi_im_flush_kerb(osm_sf_lines)
data_pack_im_pavement_width = oi_im_pavement_width(osm_sf_lines)
data_pack_im_ped_infra = oi_im_pedestrian_infra(osm_sf_lines)
data_pack_im_surfaces = oi_im_surfaces(osm_sf_lines)
data_pack_im_tactile_paving = oi_im_tactile_paving(osm_sf_lines)
data_pack_lit = oi_is_lit(osm_sf_lines)
data_pack_road_desc = oi_recode_road_class(osm_sf_lines)

# New additions
data_pack_bicycle_parking = oi_bicycle_parking(osm_sf_nodes)
data_pack_cycle_separation = oi_cycle_separation(osm_sf_lines)



# Remove NA values (Missing)
data_pack_road_desc = data_pack_road_desc %>% dplyr::filter(! is.na(openinfra_road_desc))
data_pack_maxspeed = data_pack_maxspeed %>% dplyr::filter(! is.na(openinfra_maxspeed))
data_pack_bicycle_parking = data_pack_bicycle_parking %>% dplyr::filter(!is.na(openinfra_cycle_parking))
data_pack_cycle_separation = data_pack_cycle_separation %>% dplyr::filter(!is.na(openinfra_cycle_infra))



# Create SOTM plots -------------------------------------------------------
tmap_mode("plot")
#osm_sf = osm_sf %>% dplyr::filter(! is.na(highway)) # COMMENT OUT AFTER

tmap::tm_shape(data_pack_lit |> dplyr::select(c("openinfra_is_lit"))) +
  tmap::tm_lines(col = "openinfra_is_lit", title.col = "Lighting presence") +
  tmap::tm_layout(title = "Presence of lighting on ways within 5mk of Leeds City Centre", legend.bg.color = "white")
# _________________________________________________________________________
# Re-coded Road Descriptions / Class
fig_road_desc = tmap::tm_shape(data_pack_road_desc %>% dplyr::select("openinfra_road_desc")) + 
  tmap::tm_lines(col = "openinfra_road_desc", title.col = "Recoded Road Descriptions") + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_road_desc, "fig_road_desc.jpg", dpi = 700)

# OSM Highway Values
fig_norm_highway = tmap::tm_shape(data_pack %>% dplyr::select("highway")) + 
  tmap::tm_lines(col = "highway", title.col = "OSM Highway Values") + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_norm_highway, "fig_norm_highway.jpg", dpi = 700)

# oi_active_cycle() oi_cycle
fig_active_cycle = tmap::tm_shape(data_pack_cycle %>% dplyr::select("openinfra_cycle")) + 
  tmap::tm_lines(col = "openinfra_cycle", title.col = "Cyclable Ways", palette = c("sienna2", "darkseagreen3")) + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_active_cycle, "fig_active_cycle.jpg", dpi = 700)

# oi_active_walk() oi_walk
fig_active_walk = tmap::tm_shape(data_pack_walk %>% dplyr::select("openinfra_walk")) + 
  tmap::tm_lines(col = "openinfra_walk", title.col = "Walkable Ways", palette = c("sienna2", "darkseagreen3")) + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_active_walk, "fig_active_walk.jpg", dpi = 700)

# oi_clean_maxspeed_uk() oi_maxspeed
fig_maxspeed = tmap::tm_shape(data_pack_maxspeed %>% dplyr::select("openinfra_maxspeed")) + 
  tmap::tm_lines(col = "openinfra_maxspeed", title.col = "Recategorised Maxspeed") + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_maxspeed, "fig_maxspeed.jpg", dpi = 700)

# oi_is_lit() oi_is_lit
fig_is_lit = tmap::tm_shape(data_pack_lit %>% dplyr::select("openinfra_is_lit")) + 
  tmap::tm_lines(col = "openinfra_is_lit", title.col = "Presence of Lighting", palette = c("darkslategray3", "sienna2", "darkorchid3")) + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_is_lit, "fig_is_lit.jpg", dpi = 700)

# oi_road_names() openinfra_road_name
fig_road_names = tmap::tm_shape(data_pack_road_name %>% dplyr::select("openinfra_road_name")) + 
  tmap::tm_lines(col = "openinfra_road_name", title.col = "OSM Road Names") + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_road_names, "fig_road_names.jpg", dpi = 700)

# oi_bicycle_parking() openinfra_cycle_parking
fig_bicycle_parking = tmap::tm_shape(data_pack_bicycle_parking %>% dplyr::select("openinfra_cycle_parking")) + 
  tmap::tm_dots(col = "openinfra_cycle_parking", ) + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_bicycle_parking, "fig_bicycle_parking.jpg", dpi = 700)

# oi_cycle_separation() openinfra_cycle_separation
fig_cycle_separation = tmap::tm_shape(data_pack_cycle_separation %>% dplyr::select("openinfra_cycle_infra")) +
  tmap::tm_lines(col = "openinfra_cycle_infra", title.col = "Dedicated Cycling Infra") + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white")
tmap::tmap_save(fig_cycle_separation, "fig_cycle_separation.jpg", dpi = 700) 

# Load figures into R
fig_road_desc
fig_norm_highway
fig_active_cycle
fig_active_walk
fig_maxspeed
fig_is_lit
fig_road_names
fig_bicycle_parking
fig_cycle_separation

# Create two value count tables to show the effect of clean_maxspeed_uk
uncleaned_speeds = as.data.frame(table(total_place_downloaded$maxspeed))
cleaned_speeds = as.data.frame(table(data_pack_maxspeed$oi_maxspeed))

