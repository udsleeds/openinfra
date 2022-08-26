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
                   "kerb", "footway", "sidewalk", "cycleway", "segregated", "highway",
                   "crossing", "lit", "tactile_paving", "surface", "smoothness", "width",
                   "est_width", "lit_by_led", "ref", "amenity")

# Set place name
place_name = "Leeds"

# Specify Buffer Radius
radius = 2500 #(metres)

# Specify Buffer Centre & crs
coords = c(-1.549, 53.801)
crs = "EPSG:4326"

#_______________________________________________


# Check Provider Match -----------------------------------------------------

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


# Buffer Setup ------------------------------------------------------------

# Setting up the circular buffer around specified (long, lat) coord
place_centre_point = sf::st_sfc(sf::st_point(coords), crs = crs)
circle_buffer = sf::st_buffer(place_centre_point, dist = radius)



# Get OSM Data ------------------------------------------------------------

# Download Data
total_place = osmextract::oe_get(
  place = place_name, 
  download_only = TRUE,
  force_download = TRUE,
  skip_vectortranslate = TRUE,
  never_skip_vectortranslate = FALSE,
  download_directory = osmextract::oe_download_directory()
)

# File path of downloaded file
fp = paste0(osmextract::oe_download_directory(), "/bbbike_Leeds.osm.pbf")

# Read place "lines" layer
example_data = osmextract::oe_read(
  file_path = fp, 
  never_skip_vectortranslate = TRUE, 
  layer = "lines",
  extra_tags = all_extra_tags,
  boundary = circle_buffer,
  boundary_type = "clipsrc"
)

# Read place "points" layer
example_data_pois = osmextract::oe_read(
  file_path = fp, 
  never_skip_vectortranslate = TRUE,
  layer = "points",
  extra_tags = all_extra_tags,
  boundary = circle_buffer,
  boundary_type = "clipsrc"
)

# Remove columns with different names
to_combine_pois = within(example_data_pois, rm("address", "is_in", "place"))
to_combine_ways = within(example_data, rm("waterway", "aerialway", "z_order"))

# Combine place layers
example_data_combined = rbind(to_combine_pois, to_combine_ways)


# Save package data -------------------------------------------------------
# Un-comment the below to save created package data
usethis::use_data(example_data, overwrite = TRUE)
usethis::use_data(example_data_pois, overwrite = TRUE)

# Create Data Pack Visuals ------------------------------------------------

data_pack = oi_road_names(osm_sf)
data_pack_road_name = oi_road_names(osm_sf)
data_pack_cycle = oi_active_cycle(osm_sf)
data_pack_walk = oi_active_walk(osm_sf)
data_pack_maxspeed = oi_clean_maxspeed_uk(osm_sf)
data_pack_IM = oi_inclusive_mobility(osm_sf)
data_pack_lit = oi_is_lit(osm_sf)
data_pack_road_desc = oi_recode_road_class(osm_sf)



# Generate Data Pack Interactive htmls ------------------------------------
tmap::tmap_mode("view")

# Re-coded Road Descriptions / Class
map_road_desc = tmap::tm_shape(data_pack_road_desc %>% dplyr::select("oi_road_desc")) + 
  tmap::tm_lines(col = "oi_road_desc", title.col = "Recoded Road Descriptions") + 
  tmap::tm_layout(title = "Recoded Road Descriptions - 2.5km Buffer at Leeds City Centre", legend.bg.alpha = 0.5, legend.bg.color = "white" )
#tmap::tmap_save(map_road_desc, "2500m_LCC_map_road_desc.html")

# OSM Highway Values
map_norm_highway = tmap::tm_shape(data_pack %>% dplyr::select("highway")) + 
  tmap::tm_lines(col = "highway", title.col = "OSM Highway Values") + 
  tmap::tm_layout(title = "Default OSM Highway Values - 2.5km Buffer at Leeds City Centre ", legend.bg.alpha = 0.5, legend.bg.color = "white" )
#tmap::tmap_save(map_norm_highway, "2500m_LCC_map_norm_highway.html")

# oi_active_cycle() oi_cycle
map_active_cycle = tmap::tm_shape(data_pack_cycle %>% dplyr::select("oi_cycle")) + 
  tmap::tm_lines(col = "oi_cycle", title.col = "Cyclable Ways", palette = c("red", "green")) + 
  tmap::tm_layout(title = "Cyclable OSM Infrastructure - 2.5km Buffer at Leeds City Centre", legend.bg.alpha = 0.5, legend.bg.color = "white" )
#tmap::tmap_save(map_active_cycle, "2500m_LCC_map_active_cycle.html")

# oi_active_walk() oi_walk
map_active_walk = tmap::tm_shape(data_pack_walk %>% dplyr::select("oi_walk")) + 
  tmap::tm_lines(col = "oi_walk", title.col = "Walkable Ways", palette = c("red", "green")) + 
  tmap::tm_layout(title = "Walkable OSM Infrastructure - 2.5km Buffer at Leeds City Centre", legend.bg.alpha = 0.5, legend.bg.color = "white" )
#tmap::tmap_save(map_active_walk, "2500m_LCC_map_active_walk.html")

# oi_clean_maxspeed_uk() oi_maxspeed
map_maxspeed = tmap::tm_shape(data_pack_maxspeed %>% dplyr::select("oi_maxspeed")) + 
  tmap::tm_lines(col = "oi_maxspeed", title.col = "Recategorised Maxspeed") + 
  tmap::tm_layout(title = "Cleaned UK Maxspeed Values - 2.5km Buffer at Leeds City Centre", legend.bg.alpha = 0.5, legend.bg.color = "white" )
#tmap::tmap_save(map_maxspeed, "2500m_LCC_fig_maxspeed.html")

# oi_is_lit() oi_is_lit
map_is_lit = tmap::tm_shape(data_pack_lit %>% dplyr::select("oi_is_lit")) + 
  tmap::tm_lines(col = "oi_is_lit", title.col = "Presence of Lighting") + 
  tmap::tm_layout(title = "Presence of Lighting on OSM Ways - 2.5km Buffer at Leeds City Centre", legend.bg.alpha = 0.5, legend.bg.color = "white" )
#tmap::tmap_save(map_is_lit, "2500m_LCC_fig_is_lit.html")

# oi_road_names() oi_name
map_road_names = tmap::tm_shape(data_pack_road_name %>% dplyr::select("oi_name")) + 
  tmap::tm_lines(col = "oi_name", title.col = "OSM Road Names") + 
  tmap::tm_layout(title = "Recoded Road Names - 2.5km Buffer at Leeds City Centre", legend.bg.alpha = 0.5, legend.bg.color = "white" )
#tmap::tmap_save(map_road_names, "2500m_LCC_fig_road_names.html")


# SOTM 2022 Presentation Figures ------------------------------------------
tmap_mode("plot")

# Re-coded Road Descriptions / Class
fig_road_desc = tmap::tm_shape(data_pack_road_desc %>% dplyr::select("oi_road_desc")) + 
  tmap::tm_lines(col = "oi_road_desc", title.col = "Recoded Road Descriptions") + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_road_desc, "fig_road_desc.jpg")

# OSM Highway Values
fig_norm_highway = tmap::tm_shape(data_pack %>% dplyr::select("highway")) + 
  tmap::tm_lines(col = "highway", title.col = "OSM Highway Values") + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_norm_highway, "fig_norm_highway.jpg")

# oi_active_cycle() oi_cycle
fig_active_cycle = tmap::tm_shape(data_pack_cycle %>% dplyr::select("oi_cycle")) + 
  tmap::tm_lines(col = "oi_cycle", title.col = "Cyclable Ways", palette = c("red", "green")) + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_active_cycle, "fig_active_cycle.jpg")

# oi_active_walk() oi_walk
fig_active_walk = tmap::tm_shape(data_pack_walk %>% dplyr::select("oi_walk")) + 
  tmap::tm_lines(col = "oi_walk", title.col = "Walkable Ways", palette = c("red", "green")) + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_active_walk, "fig_active_walk.jpg")

# oi_clean_maxspeed_uk() oi_maxspeed
fig_maxspeed = tmap::tm_shape(data_pack_maxspeed %>% dplyr::select("oi_maxspeed")) + 
  tmap::tm_lines(col = "oi_maxspeed", title.col = "Recategorised Maxspeed") + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_maxspeed, "fig_maxspeed.jpg")

# oi_is_lit() oi_is_lit
fig_is_lit = tmap::tm_shape(data_pack_lit %>% dplyr::select("oi_is_lit")) + 
  tmap::tm_lines(col = "oi_is_lit", title.col = "Presence of Lighting") + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_is_lit, "fig_is_lit.jpg")

# oi_road_names() oi_name
fig_road_names = tmap::tm_shape(data_pack_road_name %>% dplyr::select("oi_name")) + 
  tmap::tm_lines(col = "oi_name", title.col = "OSM Road Names") + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_road_names, "fig_road_names.jpg")