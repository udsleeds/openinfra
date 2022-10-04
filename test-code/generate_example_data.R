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
                   "est_width", "lit_by_led", "ref")

# Set place name
place_name = "Leeds"

# Specify Buffer Radius
radius = 5000 # <- meters - 5km

# (Long, Lat) coords of desired buffer place (Leeds City Centre)
coords = c(-1.548567, 53.801277)

# Specify CRS
crs = "WGS84"
#_______________________________________________

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


# Setting up the circular buffer around specified (long, lat) coord
crs = crs
place_point = coords
# Desired (m) radius around desired point
radius = radius
# Converts point coord into a sf object (so we can use st_buffer)
point_table <- data.frame(place=("Location"), lon=(place_point[1]), lat=(place_point[2]))
point_sf = st_as_sf(point_table, coords=c("lon", "lat"), crs=crs)
# Define the circle buffer around our desired location
circle_buffer = sf::st_buffer(point_sf, dist = radius)


# Total dataset for leeds - no travel mode specified
total_place = osmextract::oe_get(
  place = place_name,
  provider = provider,
  boundary = circle_buffer,
  boundary_type = "clipsrc",
  layer = "lines",
  never_skip_vectortranslate = TRUE,
  force_download = TRUE,
  quiet = FALSE,
  extra_tags = all_extra_tags
)

total_place_downloaded = osmextract::oe_read(
  file_path = "/home/james/Desktop/LIDA_OSM_Project/osm_pbf_files/england-latest.osm.pbf",
  layer = "lines",
  boundary = circle_buffer,
  boundary_type = "clipsrc",
  extra_tags = all_extra_tags
)

total_place_downloaded = total_place_downloaded %>% dplyr::filter(! is.na(highway))
total_place = total_place %>% dplyr::filter(! is.na(highway))

#__________________FUNCTION___
# This script may actually want deleting now - package data will be made using 
# the generate_package_data.R script rather than this. 
#_____________________________


osm_sf= total_place[circle_buffer, ]
# UNCOMMENT BELOW TO SAVE NEW EXAMPLE_DATA
#usethis::use_data(example_data)


# SOTM2022 Plots
osm_sf = total_place
osm_sf = total_place_downloaded

osm_sf_road_recoded = oi_road_names(osm_sf) # recode_road_class(osm_sf) #openinfra::recode_road_class(osm_sf)
data_pack = oi_clean_maxspeed_uk(osm_sf_road_recoded) #openinfra::oi_clean_maxspeed_uk(osm_sf_road_recoded)
data_pack_IM = oi_inclusive_mobility(data_pack)
data_pack_lit = oi_is_lit(data_pack)
#data_pack_short = data_pack %>% dplyr::select(c("osm_id", "highway", "road_desc", "oi_maxspeed"))
data_pack_IM = data_pack_IM %>% dplyr::select(c("highway", "im_footway", "im_footpath", "im_tactile"))

data_pack = oi_road_names(osm_sf)
data_pack_road_name = oi_road_names(osm_sf)
data_pack_cycle = oi_active_cycle(osm_sf)
data_pack_walk = oi_active_walk(osm_sf)
data_pack_maxspeed = oi_clean_maxspeed_uk(osm_sf)
data_pack_IM = oi_inclusive_mobility(osm_sf)
data_pack_lit = oi_is_lit(osm_sf)
data_pack_road_desc = oi_recode_road_class(osm_sf)



tmap_mode("plot")

#osm_sf = osm_sf %>% dplyr::filter(! is.na(highway)) # COMMENT OUT AFTER

tmap::tm_shape(data_pack_lit |> dplyr::select(c("oi_is_lit"))) +
  tmap::tm_lines(col = "oi_is_lit", title.col = "Lighting presence") +
  tmap::tm_layout(title = "Presence of lighting on ways within 5mk of Leeds City Centre", legend.bg.color = "white")
#'---------------------------------------______________________________________
# Re-coded Road Descriptions / Class
fig_road_desc = tmap::tm_shape(data_pack_road_desc %>% dplyr::select("oi_road_desc")) + 
  tmap::tm_lines(col = "oi_road_desc", title.col = "Recoded Road Descriptions") + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_road_desc, "fig_road_desc.jpg", dpi = 700)

# OSM Highway Values
fig_norm_highway = tmap::tm_shape(data_pack %>% dplyr::select("highway")) + 
  tmap::tm_lines(col = "highway", title.col = "OSM Highway Values") + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_norm_highway, "fig_norm_highway.jpg", dpi = 700)

# oi_active_cycle() oi_cycle
fig_active_cycle = tmap::tm_shape(data_pack_cycle %>% dplyr::select("oi_cycle")) + 
  tmap::tm_lines(col = "oi_cycle", title.col = "Cyclable Ways", palette = c("sienna2", "darkseagreen3")) + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_active_cycle, "fig_active_cycle.jpg", dpi = 700)

# oi_active_walk() oi_walk
fig_active_walk = tmap::tm_shape(data_pack_walk %>% dplyr::select("oi_walk")) + 
  tmap::tm_lines(col = "oi_walk", title.col = "Walkable Ways", palette = c("sienna2", "darkseagreen3")) + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_active_walk, "fig_active_walk.jpg", dpi = 700)

# oi_clean_maxspeed_uk() oi_maxspeed
fig_maxspeed = tmap::tm_shape(data_pack_maxspeed %>% dplyr::select("oi_maxspeed")) + 
  tmap::tm_lines(col = "oi_maxspeed", title.col = "Recategorised Maxspeed") + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_maxspeed, "fig_maxspeed.jpg", dpi = 700)

# oi_is_lit() oi_is_lit
fig_is_lit = tmap::tm_shape(data_pack_lit %>% dplyr::select("oi_is_lit")) + 
  tmap::tm_lines(col = "oi_is_lit", title.col = "Presence of Lighting", palette = c("darkslategray3", "sienna2", "darkorchid3")) + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_is_lit, "fig_is_lit.jpg", dpi = 700)

# oi_road_names() oi_name
fig_road_names = tmap::tm_shape(data_pack_road_name %>% dplyr::select("oi_name")) + 
  tmap::tm_lines(col = "oi_name", title.col = "OSM Road Names") + 
  tmap::tm_layout(title = "", title.bg.color = "white", title.bg.alpha = 0.7, legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_road_names, "fig_road_names.jpg", dpi = 700)

fig_road_desc
fig_norm_highway
fig_active_cycle
fig_active_walk
fig_maxspeed
fig_is_lit
fig_road_names


# Create two value count tables to show the effect of clean_maxspeed_uk

#vc = as.data.frame( table(df$col_name) )

uncleaned_speeds = as.data.frame(table(total_place_downloaded$maxspeed))

cleaned_speeds = as.data.frame(table(data_pack_maxspeed$oi_maxspeed))


