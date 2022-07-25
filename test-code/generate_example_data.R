# Generate package test-data. 
#library("openinfra")
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
                   "est_width", "lit_by_led")

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
  layer = "lines",
  never_skip_vectortranslate = TRUE,
  force_download = TRUE,
  quiet = FALSE,
  extra_tags = all_extra_tags
)


#__________________FUNCTION___


#_____________________________


total_place_buffered = total_place[circle_buffer, ]

example_data = total_place_buffered

# UNCOMMENT BELOW TO SAVE NEW EXAMPLE_DATA
#usethis::use_data(example_data)

osm_sf = total_place_buffered

osm_sf_road_recoded = recode_road_class(osm_sf) #openinfra::recode_road_class(osm_sf)

data_pack = oi_clean_maxspeed_uk(osm_sf_road_recoded) #openinfra::oi_clean_maxspeed_uk(osm_sf_road_recoded)

data_pack_IM = oi_inclusive_mobility(data_pack)
data_pack_lit = oi_is_lit(data_pack)

data_pack_short = data_pack %>% dplyr::select(c("osm_id", "highway", "road_desc", "oi_maxspeed"))
data_pack_IM = data_pack_IM %>% dplyr::select(c("highway", "im_footway", "im_footpath", "im_tactile"))

tmap_mode("view")

#osm_sf = osm_sf %>% dplyr::filter(! is.na(highway)) # COMMENT OUT AFTER

tmap::tm_shape(data_pack_lit |> dplyr::select(c("oi_is_lit"))) +
  tmap::tm_lines(col = "oi_is_lit", title.col = "Lighting presence") +
  tmap::tm_layout(title = "Presence of lighting on ways within 5mk of Leeds City Centre", legend.bg.color = "white")

#map = tmap::tm_shape(data_pack_IM |> dplyr::select(c("im_footway", "im_footpath", "im_tactile"))) +
#  tmap::tm_lines(col = "highway", title.col = "OSM highways") +
#  tmap::tm_layout(title = "OSM highways within 5mk of Leeds City Centre", legend.bg.color = "white")

#map

tmap_save(map, "5km_Leeds_OSM_highways.html")

#sf::st_write(data_pack, "500m_LCC_data_pack.geojson")

