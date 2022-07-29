# Script analysing the new io_active_wlking function. 
library(osmextract)
# Load all openinfra functions (locally - use library(openinfra) otherwise)
devtools::load_all()

# New function (demo - code is the same as official fucntion.)
oi_active_walk_demo = function(osm_sf, remove = FALSE){
  #browser() #<-- Uncomment to debug function.
  osm_sf_walking = osm_sf %>% dplyr::mutate(oi_walk = dplyr::case_when(
    
    # Highway tag cannot be NA
    is.na(highway) ~ "no",
    
    # Highway tag cannot have un-walkable values
    highway %in% c('abandoned', 'bus_guideway', 'byway', 'construction', 
                   'corridor', 'elevator', 'fixme', 'escalator', 'gallop', 
                   'historic', 'no', 'planned', 'platform', 'proposed',
                   'raceway', 'motorway', 'motorway_link') 
    ~ "no",
    
    # Hihgway tag cannot be a cycleway UNLESS walking is permitted
    ((highway == "cycleway") & (foot %in% c('yes', 'designated', 'permissive', 'destination'))) ~ "yes",
    # Below catches highway == cycleway and foot != values below INCLUDING NAs. 
    ((highway == "cycleway") & (! foot %in% c('yes', 'designated', 'permissive', 'destination'))) ~ "no",
    
    (highway %in% c("footway", "pedestrian", "path")) & (! foot %in% c("no", "private")) & (! access %in% c("private", "no")) ~ "yes",
    
    # Access cannot be restricted
    ((access %in% c('private', 'no')) & (foot == "yes")) ~ "yes",
    ((access %in% c('private', 'no'))) ~ "no",
    
    # Foot usage must be permitted,
    foot %in% c('private', 'no', 'use_sidepath', 'restricted') ~ "no",
    
    # Service value does not contain "private"
    ((grepl("private", service)) & (foot == "yes")) ~ "yes",
    ((grepl("private", service)) & (foot != "yes")) ~ "no",
    
  ))
  
  # find features that do not contain "no" and set as "yes"
  osm_sf_walking$oi_walk[is.na(osm_sf_walking$oi_walk)] = "yes"
  
  # If remove = TRUE, filter out features that have oi_walking == "no"
  if (remove){
    osm_sf_walking = osm_sf_walking %>% dplyr::filter(osm_sf_walking$oi_walk == "yes")
  }
  
  return(osm_sf_walking)
}


# Specify Buffer Radius
radius = 5000 # <- meters - 5km
# (Long, Lat) coords of desired buffer place (Leeds City Centre)
coords = c(-1.548567, 53.801277)
# Specify CRS
crs = "WGS84"
# Setting up the circular buffer around specified (long, lat) coord
crs = crs
place_point = coords
# Desired (m) radius around desired point
radius = radius
# Converts point coord into a sf object (so we can use st_buffer)
point_table <- data.frame(place=("Location"), lon=(place_point[1]), lat=(place_point[2]))
point_sf = sf::st_as_sf(point_table, coords=c("lon", "lat"), crs=crs)
# Define the circle buffer around our desired location
circle_buffer = sf::st_buffer(point_sf, dist = radius)



total_place_full = osmextract::oe_get(
  place = "Leeds",
  provider = "bbbike",
  layer = "lines",
  extra_tags = c("access", "service", "foot"),
  force_download = TRUE,
  force_vectortranslate = TRUE,
  quiet = FALSE
)
# Filter out any NA highways (train tracks, water ways etc.)
total_place_full = total_place_full %>% dplyr::filter(! is.na(highway))

osmextract_walking_full = osmextract::oe_get_network(
  place = "Leeds",
  mode = "walking",
  provider = "bbbike",
  extra_tags = c("access", "service", "foot"),
  force_download = TRUE,
  force_vectortranslate = TRUE,
  quiet = FALSE 
)

# Now apply the circular buffer to networks so visualisation and processing is
# faster than the entire WY network. 

total_place = total_place_full[circle_buffer, ]
osmextract_walking = osmextract_walking_full[circle_buffer, ]

total_place_walking_all = oi_active_walk_demo(total_place, remove = FALSE)
total_place_walking = oi_active_walk_demo(total_place, remove = TRUE)

mine_not_in_osmextract = dplyr::anti_join(as.data.frame(total_place_walking), as.data.frame(osmextract_walking), by = "osm_id")
# (74 x 14)

tmap::tmap_mode("view")
tmap::qtm(sf::st_as_sf(mine_not_in_osmextract), line.col = "red")


mine_not_in_osmextract = mine_not_in_osmextract %>% dplyr::mutate(key = "oi_walk")
osmextract_walking = osmextract_walking %>% dplyr::mutate(key = "osmextract")
mine_not_in_osmextract = within(mine_not_in_osmextract, rm(oi_walk))
combined = rbind(mine_not_in_osmextract, osmextract_walking)

# Below creates a map comparing my case_when filtering, and osmextract filtering. 
compare = tmap::tm_shape(sf::st_as_sf(combined) %>% dplyr::select(c("key"))) +
  tmap::tm_lines(col = "key", title.col = "filter type", palette = c("red", "blue")) +
  tmap::tm_layout(title = "Walking filtering analysis", legend.bg.color = "white")
tmap::tmap_save(compare, "comparing_proposed_oi_walk.html")

my_map = tmap::tm_shape(sf::st_as_sf(mine_not_in_osmextract) %>% dplyr::select(c("key"))) +
  tmap::tm_lines(col = "key", title.col = "filter type", palette = c("red")) +
  tmap::tm_layout(title = "openinfra walking filtering analysis", legend.bg.color = "white")
tmap::tmap_save(my_map, "oi_walk_additional_features.html")

extract_map = tmap::tm_shape(sf::st_as_sf(osmextract_walking) %>% dplyr::select(c("key"))) +
  tmap::tm_lines(col = "key", title.col = "filter type", palette = c("blue")) +
  tmap::tm_layout(title = "osmextract walking filtering analysis", legend.bg.color = "white")
tmap::tmap_save(extract_map, "osmextract_walking_features.html")