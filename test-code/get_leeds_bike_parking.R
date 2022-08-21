library(osmextract)
library(sf)
library(tmap)

leeds_centre_point = sf::st_sfc(sf::st_point(c(-1.549, 53.801)), crs = "EPSG:4326")
leeds_buffer = sf::st_buffer(leeds_centre_point, dist = 5000)

all_extra_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                   "kerb", "footway", "sidewalk", "cycleway", "segregated", "highway",
                   "crossing", "lit", "tactile_paving", "surface", "smoothness", "width",
                   "est_width", "lit_by_led", "ref", "amenity")

# Download leeds ways
leeds_lines = oe_get(
  place = "leeds",
  layer = "lines",
  extra_tags = all_extra_tags
  
)

# Download leeds nodes
leeds_points = oe_get(
  place = "leeds",
  layer = "points",
  extra_tags = all_extra_tags
)

# Remove different columns 
leeds_lines = within(leeds_lines, rm("z_order", "waterway", "aerialway"))
leeds_points = within(leeds_points, rm("place", "is_in", "address"))

# Combine ways and nodes
leeds_combined = rbind(leeds_points, leeds_lines)

# Subset the data to 5km circular buffer & select bicycle parking
leeds_combined = leeds_combined[leeds_buffer , op = st_within]
bike_storage = bike_storage %>% dplyr::filter(amenity == "bicycle_parking")

# Visualise bike parking in Leeds
tmap::qtm(bike_storage)
