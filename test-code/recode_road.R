# recode_road_class function. 
# Takes an OSM sf dataframe/datatable and removes features 
#_________________________________________________________________________
# create a fucntion or filter, that removes highways from a sf data frame unless 
# they have the highway = allowed_highways above, then either re name or add an
# additional columun called road_class being one of the 0-(6/7) road classes available

library("osmextract")
library("tmap")
library("dplyr")
library("sf")
devtools::load_all() # Loads recode_road_class

# All extra tags - by requesting all of these extra_tags, you can
# later specify your own network ("cycling", "walking", "driving")
# as all necessary tags are within all_extra_tags
all_extra_tags = c("access", "service", "bicycle", "foot")

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


# Setting up the circular buffer around specified (long, lat) coord
crs = "WGS84"
# (Long, Lat) coords of desired place (Leeds City Centre)
place_point = c(-1.548567, 53.801277)
# Desired (m) radius around desired point
radius = 5000 #(5km)
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
  extra_tags = c(all_extra_tags, "oneway", "maxspeed") # Add in "oneway" for recode_road_class
)

#repro_example = "example url here"

# Apply function to example Leeds dataset.
re_classified = recode_road_class(total_place)

# Apply the created circular buffer to our re-cat network
place_subset = re_classified[circle_buffer, ]

tmap_mode("view")

# To save this plot, simply assign to a variable with <- and use tmap::tm_save()
tmap::tm_shape(place_subset %>% dplyr::select(road_desc)) +
  tmap::tm_lines("road_desc") +
  tmap::tm_layout(title = "Road Descriptions within 5km of Leeds City Centre")
