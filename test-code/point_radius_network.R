library(osmextract)
library(tmap)
library(sf)
library(data.table)

# This script demonstrates sub-setting geospatial data. 
# In this instances we sub-set a a 1km radius circle centred at Leeds City Centre (LCC)
# out of the entire Leeds City network. 
# Network data has been obtained using the package osmextract.

tmap_mode("view")
crs = "WGS84"

# Long, Lat coords of desired place
place_point = c(-1.548567, 53.801277)
# Desired (m) radius around desired point
radius = 1000 #(1km)

# Converts point coord into a sf object (so we can use st_buffer)
point_table <- data.table(place=("Location"), lon=(place_point[1]), lat=(place_point[2]))
point_sf = st_as_sf(point_table, coords=c("lon", "lat"), crs=crs)

# Define the circle buffer around our desired location
circle_buffer = sf::st_buffer(point_sf, dist = radius)

# Defines the total network, of which a subset will be made.
leeds_total = sf::read_sf("https://github.com/udsleeds/openinfra/releases/download/v0.1/leeds.geojson")

# Creates subset of total network using our defined buffer
leeds_sub = leeds_total[circle_buffer, ]

#View the subset
qtm(leeds_sub,
    lines.col = "red",
    title = "1km subset of LCC from toal Leeds City network.")
