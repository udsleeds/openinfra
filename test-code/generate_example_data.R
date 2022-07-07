# Generate package test-data. 

library("osmextract")
library("tmap")
library("dplyr")
library("sf")


# All extra tags - by requesting all of these extra_tags, you can
# later specify your own network ("cycling", "walking", "driving")
# as all necessary tags are within all_extra_tags
all_extra_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                   "kerb", "footway", "sidewalk", "cycleway", "segregated", "highway",
                   "crossing", "lit", "tactile_paving", "surface", "smoothness", "width",
                   "est_width")

# Set place name
place_name = "Leeds"

# Specify Buffer Radius
radius = 6500 # <- meters - 6.5km

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


total_place_buffered = total_place[circle_buffer, ]


extended_data = total_place_buffered
