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
                   "est_width")

# Set place name
place_name = "Leeds"

# Specify Buffer Radius
radius = 5000 # <- meters - 6.5km

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
oi_inclusive_mobility = function(osm_sf) {
  
  #print(dim(osm_sf))
  #browser()
  
  osm_sf_im = osm_sf %>% 
    # Assesses whether a kerb is flush or not
    dplyr::mutate(im_kerb = dplyr::if_else(kerb == "flush" | kerb == "no", "flush", "other")) %>% 
    
    # Assesses footway - a ‘pavement’ adjacent to a road
    dplyr::mutate(im_footway = dplyr::case_when(
      footway %in% c("left", "right", "both", "sidewalk") |
        sidewalk %in% c("left", "right", "both", "yes", "separate") |
        # trying to capture footways shared with cyclists
        !is.na(cycleway) & # map cycling infrastructure that is an inherent part of the road
        foot %in% c("yes", "designated") |
        segregated %in% "yes"
      ~ "yes",
      TRUE ~ "no" 
    ) 
    ) %>% 
    # Assesses footpath - any other right of way for pedestrians, that does not run adjacent to a road.
    dplyr::mutate(im_footpath = dplyr::case_when(
      highway %in% "footway" & 
        im_footway %in% "no" | 
        # not (always) an inherent part of the road
        highway %in% c("cycleway", "bridleway", "path") & # foot = "designated" is implied
        im_footway %in% "no" &
        ! foot %in% c("no", "private") | 
        ! access %in% c("no", "private") &
        segregated %in% "no" # shared space
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>%
    
    # presence of a crossing: give-way, signal controlled, none, or yes (but the type is unknown)
    dplyr::mutate(im_crossing = dplyr::case_when(
      stringr::str_detect(crossing, "zebra|uncontr|marked")~ "give-way",
      stringr::str_detect(crossing, "toucan|pedex|puffin|equestrian|light|signal")~ "signal-controlled",
      highway %in% "crossing" | footway  %in% "crossing" | !is.na(crossing) ~ "yes",
      TRUE ~ "no"
    )) %>% 
    
    # implied footways but there's a lack of data to verify
    dplyr::mutate(im_footway_imp = dplyr::case_when(
      im_footway %in% "no" &
        im_footpath %in% "no" &
        im_crossing %in% "no"
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>% 
    # Assesses whether the way is lit or not
    dplyr::mutate(im_light = dplyr::case_when( 
      # highway %in% "street_lamp" |
      ! lit %in% c("no", "disused") & ! is.na(lit)
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>% 
    
    # Assesses the presence of tactile paving - either yes, no.
    dplyr::mutate(im_tactile = dplyr::case_when(
      ! tactile_paving %in% c("no", "incorrect", "bad") & ! is.na(tactile_paving) 
      ~ "yes",
      ! is.na(tactile_paving)
      ~ "no"
    )
    ) %>%
    
    # Assesses whether surface is paved, unpaved, or other
    dplyr::mutate(
      im_surface_paved = dplyr::case_when(
        highway %in% "cycleway"
        ~ "paved",
        
        stringr::str_detect(surface,
                            "pav|asph|chipseal|concrete|paving|sett|cobble|metal|wood|stepping")
        ~ "paved",
        highway %in% c("footway", "bridleway") & # highway = footway implied surface value is unpaved
          ! surface %in% stringr::str_detect(surface, "pav|asph|chipseal|concrete|paving|sett|cobble|metal|wood|stepping")
        ~ "unpaved",
        stringr::str_detect(surface, "unpav|compact|gravel|rock|pebble|ground|dirt|grass|mud|sand|woodchips|snow|ice|salt")
        ~ "unpaved",
        TRUE & !is.na(surface) ~ "other"
      )
    ) %>% 
    # Assesses whether surface is even or uneven
    dplyr::mutate(im_surface = dplyr::case_when(
      stringr::str_detect(surface, "asph|concrete")
      ~ "even",
      
      im_surface_paved %in% "paved" &
        smoothness %in% c("excellent", "good")
      ~ "even",
      ! is.na(im_surface_paved) 
      ~ "uneven"
    )
    ) %>% 
    # Assesses way width - either under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      im_width =  width %>% 
        readr::parse_number(),
      im_width = dplyr::case_when(
        im_width > 0 & im_width < 1.5 ~ " < 1.5",
        im_width <= 1.5 & im_width <= 2 ~ "1.5 - 2",
        im_width > 2 ~ "> 2"
      )
    ) %>% 
    # Assesses estimated way width - either under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      im_width_est = est_width %>% 
        readr::parse_number(),
      im_width_est = dplyr::case_when(
        im_width_est > 0 & im_width_est < 1.5 ~ "< 1.5",
        im_width_est <= 1.5 & im_width_est <= 2 ~ "1.5 - 2",
        im_width_est > 2 ~ "> 2"
      )
    )
  #print(dim(osm_sf_im))
  #structure(osm_sf_im)
  return(osm_sf_im)
}

#_____________________________


total_place_buffered = total_place[circle_buffer, ]
osm_sf = total_place_buffered

osm_sf_road_recoded = openinfra::recode_road_class(osm_sf)

data_pack = openinfra::oi_clean_maxspeed_uk(osm_sf_road_recoded)

data_pack_IM = oi_inclusive_mobility(data_pack)

data_pack_short = data_pack %>% dplyr::select(c("osm_id", "highway", "road_desc", "oi_maxspeed"))
data_pack_IM = data_pack_IM %>% dplyr::select(c("im_footway", "im_footpath", "im_tactile"))

tmap_mode("view")

#osm_sf = osm_sf %>% dplyr::filter(! is.na(highway)) # COMMENT OUT AFTER

map = tmap::tm_shape(data_pack_IM |> dplyr::select(c("im_footway", "im_footpath", "im_tactile"))) +
  tmap::tm_lines(col = "highway", title.col = "OSM highways") +
  tmap::tm_layout(title = "OSM highways within 5mk of Leeds City Centre", legend.bg.color = "white")

map

tmap_save(map, "5km_Leeds_OSM_highways.html")

#sf::st_write(data_pack, "500m_LCC_data_pack.geojson")

