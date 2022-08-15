#SOTM2022 Presentation Figures
# Generate package test-data. 
library("osmextract")
library("tmap")
library("dplyr")
library("sf")


# Paramater Setup ---------------------------------------------------------
# osmextract params.
place_name = "Leeds" # <- place query for osmextract

# NA highway filtering
remove_NA_highawy = TRUE

# Buffer params.
radius = 2500       # <- meters - 2.5km
crs = "WGS84"
coords = c(-1.548567, 53.801277)

# All extra tags - by requesting all of these extra_tags, you can
# later specify your own network ("cycling", "walking", "driving")
# as all necessary tags are within all_extra_tags
all_extra_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                   "kerb", "footway", "sidewalk", "cycleway", "segregated", "highway",
                   "crossing", "lit", "tactile_paving", "surface", "smoothness", "width",
                   "est_width", "lit_by_led", "ref")

# Save data params.
save_data = FALSE
filename = "sotm_data.geojson"

# Package Data.
package_data = FALSE

# Get Provider ------------------------------------------------------------
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


# Circular Buffer ---------------------------------------------------------
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


# Download Data -----------------------------------------------------------

sotm_data = osmextract::oe_get(
  place = place_name,
  provider = provider,
  layer = "lines",
  extra_tags = required_tags,
  boundary = circle_buffer,
  boundary_type = "clipsrc",
  never_skip_vectortranslate = TRUE,
  force_download = TRUE,
  quiet = FALSE
)

# Remove highway == NA
if (remove_NA_highawy){
  sotm_data = sotm_data %>% dplyr::filter(! is.na(highway))
}

# Save data?
if (save_data){
  sf::st_write(sotm_data, filename)
}

# Package data? 
if (package_data){
  usethis::use_r(sotm_data)
}
