library(dplyr)
library(osmextract)

oi_active_cycle = function(osm_sf, remove=FALSE){
  #browser() # Uncomment to debug
  osm_sf_cycle = osm_sf %>% dplyr::mutate(oi_cycle = dplyr::case_when(
    
    # Highway cannot be NA
    is.na(highway) ~ "no",
    
    # Highway values not appropriate for cycling
    highway %in% c('abandoned', 'bus_guideway', 'byway', 'construction', 
                   'corridor', 'elevator', 'fixme', 'escalator', 'gallop', 
                   'historic', 'no', 'planned', 'platform', 'proposed', 
                   'raceway', 'steps') 
    ~ "no",
    
    # Create two cases to catch features typically not allowed for cyclists, but have been flagged appropriate:
    # 1 - If highway = "bad highway values" BUT bicycle = "good bicycle values" then assign ~ "yes",
    (highway %in% c('motorway', 'motorway_link', 'footway', 'bridleway','pedestrian') & bicycle %in% c('yes', 'designated', 'permissive', 'destination')) ~ "yes",
    ((highway == "footway") & (! bicycle %in% c("no", "dismount", "private"))) & (! access %in% c("private", "no", "permit")) ~ "yes",
    
    # 2 - Assign highway = "bad highway values" ONLY as ~ "no". Ways that are appropriate for cyclists will already be assigned "yes" from above
    highway %in% c('motorway', 'motorway_link', 'footway', 'bridleway','pedestrian') ~ "no",
    
    # Way must have access rights
    access %in% c('private', 'no') ~ "no",
    
    # Way must not bar cyclists
    bicycle %in% c('no', 'private', 'ue_sidepath', 'restricted') ~ "no", 
    
    # Way must not contain "private" within the service tag
    grepl("private", service) ~ "no",
    
  ))
  
  # Case_when above should have added "no" to all inappropriate features, now 
  # find features that do not contain "no" and set as "yes"
  osm_sf_cycle$oi_cycle[is.na(osm_sf_cycle$oi_cycle)] = "yes"
  
  # If remove = TRUE, filter out features that have oi_cycle == "no"
  if (remove){
    osm_sf_cycle = osm_sf_cycle %>% dplyr::filter(osm_sf_cycle$oi_cycle == "yes")
  }
  return(osm_sf_cycle)
}
# devtools function is the same as the above. 
#devtools::load_all()

get_cycling = oe_get_network(
  place = "Leeds",
  provider = "bbbike",
  extra_tags = c("access", "bicycle", "service"),
  mode = "cycling",
  force_download = TRUE,
  never_skip_vectortranslate = TRUE,
  force_vectortranslate = TRUE
)

data = oe_get(
  place= "Leeds",
  extra_tags = c("foot", "access", "service", "bicycle", "footway"),
  layer = "lines",
  provider = "bbbike",
  force_download = TRUE,
  never_skip_vectortranslate = TRUE,
  force_vectortranslate = TRUE
)
# Remove train tracks & waterways etc
data = data %>% dplyr::filter(! is.na(highway))

# Specify circular buffer Radius
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

# Apply 5km buffer and add keys to indicate where the features came from
get_cycling_small = get_cycling[circle_buffer, ] %>% dplyr::mutate(key = "Origional ways")
data_small = data[circle_buffer, ] %>% dplyr::mutate(key = "Additional ways")

# Apply the function to the general data with the buffer applied! 
oi_output_full = oi_active_cycle(data_small, remove=FALSE)
oi_output_removed = oi_active_cycle(data_small, remove=TRUE)

# Determine the additional ways returned by my function not included by osmextract
additional_ways = dplyr::anti_join(as.data.frame(oi_output_removed), as.data.frame(get_cycling_small), by = "osm_id")

# Remove additional columns so that rbind() can be applied
oi_output_removed = within(oi_output_removed, rm("oi_cycle", "footway", "foot"))
analyse = rbind(oi_output_removed, get_cycling_small)

# Below creates maps comparing my case_when filtering, and osmextract filtering. 
osmextract_map = tmap::tm_shape(get_cycling_small |> dplyr::select(c("key"))) +
  tmap::tm_lines(col = "blue", title.col = "osmextract Cycling network") +
  tmap::tm_layout(title = "osmextract analysis", legend.bg.color = "white")
#tmap::tmap_save(osmextract_map, "osmextract_cycling_mode_map.html")

additional_ways = sf::st_as_sf(additional_ways)
additional_map = tmap::tm_shape(additional_ways |> dplyr::select(c("key"))) +
  tmap::tm_lines(col = "red", title.col = "Additiona") +
  tmap::tm_layout(title = "Additional ways found with my filtering", legend.bg.color = "white")
#tmap::tmap_save(additional_map, "additional_ways_from_function.html")

analyse_map = tmap::tm_shape(analyse |> dplyr::select(c("key"))) +
  tmap::tm_lines(col = "key", title.col = "Filter type", palette = c("red", "blue")) +
  tmap::tm_layout(title = "Filtering analysis", legend.bg.color = "white")
#tmap::tmap_save(analyse_map, "Comparing_osmextract_cycle_to_proposed_update.html")


