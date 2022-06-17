library(osmextract)
library(tmap)
library(dplyr)
library(sf)

tmap_mode("view")
crs = "WGS84"

"Editable Variables here"
# Long, Lat coords of desired place
# edi = Edinburgh
edi = c(55.94625165186419, -3.1844093528268886)
place_point = c(-3.1844093528268886, 55.94625165186419)
# Desired (m) radius around desired point
radius = 1000 #(1km)

# Converts point coord into a sf object (so we can use st_buffer)
point_table <- data.table(place=("Location"), lon=(place_point[1]), lat=(place_point[2]))
point_sf = st_as_sf(point_table, coords=c("lon", "lat"), crs=crs)

# Define the circle buffer around our desired location
circle_buffer = sf::st_buffer(point_sf, dist = radius)

# Now, set up three cycling networks from osmextract: 
#   1 - Default oe_get_network("cycling")
#   2 - Default "cycling" SQL filter 
#   3 - My proposed SQL filter 

osmextract_cycle_norm = osmextract::oe_get_network(
  place = "Edinburgh",
  mode = "cycling",
  provider = "bbbike",
  force_download = TRUE,
  force_vectortranslate = TRUE
)

osmextract_cycle_norm_SQL = osmextract::oe_get(
  place = "Edinbutgh",
  layer = "lines",
  provider = "bbbike",
  never_skip_vectortranslate = TRUE,
  force_download = TRUE,
  extra_tags = c("access", "bicycle", "service"),
  vectortranslate_options = c(
    "-where",
    "(highway IS NOT NULL)
    AND
    (highway NOT IN (
    'abandoned', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'raceway', 'steps'
    ))
    AND
    (highway NOT IN ('motorway', 'motorway_link', 'footway', 'bridleway',
    'pedestrian') OR bicycle IN ('yes', 'designated', 'permissive', 'destination')
    )
    AND
    (access NOT IN ('private', 'no'))
    AND
    (bicycle NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    AND
    (service NOT ILIKE 'private%')
    "
    ),
  quiet = TRUE
)

edinburgh_cycle = osmextract::oe_get(
  place = "Edinburgh",
  provider = "bbbike",
  layer = "lines",
  never_skip_vectortranslate = TRUE,
  force_download = TRUE,
  extra_tags =  c("bicycle", "access", "service"),
  vectortranslate_options = c(
    "-where",
    "(highway IS NOT NULL)
    AND
    (highway NOT IN (
    'abandoned', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'raceway', 'steps', 'motor'
    ))
    AND
    (highway NOT IN ('motorway', 'motorway_link', 'footway', 'bridleway',
    'pedestrian') OR bicycle IN ('yes', 'designated', 'permissive', 'destination')
    )
    AND
    (access NOT IN ('private', 'no'))
    AND
    (bicycle NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    AND
    (service NOT ILIKE 'private%')
    "
    )
)


edinburgh_cycle = osmextract::oe_get(
  place = "Edinburgh",
  provider = "bbbike",
  layer = "lines",
  extra_tags = c("bicycle", "access", "service"),
  never_skip_vectortranslate = TRUE,
  force_download = TRUE,
  vectortranslate_options = c(
    "-where",
    "(highway IS NOT NULL)
    AND
    (highway NOT IN (
    'abandoned', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'raceway', 'steps', 'motor'
    ))
    AND
    (highway NOT IN ('motorway', 'motorway_link', 'footway', 'bridleway',
    'pedestrian') OR bicycle IN ('yes', 'designated', 'permissive', 'destination')
    )
    AND
    (access NOT IN ('private', 'no'))
    AND
    (bicycle NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    AND
    (service NOT ILIKE 'private%')
    "
  ),
)

subset_edi = edinburgh_cycle[circle_buffer, ]
qtm(subset_edi,
    lines.col = "red",
    title = "Cycling network within 1km of Royal College of Surgeons")

#point (55.94625165186419, -3.1844093528268886)

# Defines the total network, of which a subset will be made.
#leeds_total = sf::read_sf("https://github.com/udsleeds/openinfra/releases/download/v0.1/leeds.geojson")

# Creates subset of total network using our defined buffer
#leeds_sub = buffer[leeds_total, ]

#View the subset
#qtm(leeds_sub)