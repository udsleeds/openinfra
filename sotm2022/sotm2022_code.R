# load packages
library(tidyverse)
library(sf)
library(mapview)
library(tmap)
library(osmextract)

# read OSM data (might time some time)
## downloaded 2022-05-21
wy = sf::st_read("https://github.com/udsleeds/openinfra/releases/download/v0.1/wy-2022-05-21.geojson")
## code example
# tags_needed = c("cycleway",
#                 "bicycle",
#                 "wheelchair",
#                 "kerb",
#                 "disabled",
#                 "mobility_scooter",
#                 "handicap",
#                 "foot",
#                 "lit",
#                 "access",
#                 "sidewalk",
#                 "footway",
#                 "incline",
#                 "smoothness",
#                 "est_width",
#                 "width",
#                 "ramp",
#                 "sidewalk_left",
#                 "sidewalk_right",
#                 "ramp_wheelchair",
#                 "footway_left",
#                 "footway_right",
#                 "footway_surface",
#                 "priority",
#                 "sidewalk_both_surface",
#                 "sidewalk_both_width",
#                 "path",
#                 "pedestrian",
#                 "sidewalk_left_width",
#                 "sidewalk_right_width",
#                 "sidewalk_right_surface",
#                 "sidewalk_left_surface",
#                 "maxspeed",
#                 "segregated",
#                 "sloped_curb",
#                 "surface",
#                 "tactile_paving",
#                 "crossing"
# )

# osmextract::oe_match_pattern("west yorkshire")
# region_wy = "West Yorkshire"
# wy = osmextract::oe_get(place = region_wy,
#                         layer = "lines",
#                         force_download = TRUE,
#                         force_vectortranslate = TRUE,
#                         extra_tags = tags_needed)

# sf::st_write(wy,
#              "wy-2022-05-21.geojson")

# Inclusive Mobility function

inclusive_mobility_get = function(osm_sf) {
  osm_sf_im = osm_sf %>% 
    # kerb: flush or not
    dplyr::mutate(im_kerb = dplyr::if_else(kerb == "flush" | kerb == "no", "flush", "other")) %>% 
    # footway is a ‘pavement’ adjacent to a road
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
    # footpath is any other right of way for pedestrians, that does not run adjacent to a road.
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
    # lighting: yes or no
    dplyr::mutate(im_light = dplyr::case_when( 
      # highway %in% "street_lamp" |
      ! lit %in% c("no", "disused") & ! is.na(lit)
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>% 
    # recategorize speed
    dplyr::mutate(im_maxspeed = maxspeed %>% 
                    parse_number(),
                  im_maxspeed =  dplyr::case_when(
                    im_maxspeed > 1 & im_maxspeed <= 20 ~ "1-20", # up to 20 mph
                    im_maxspeed > 20 & im_maxspeed <= 40 ~ "21-40", # 21 - 40 mph
                    im_maxspeed > 40 & im_maxspeed <= 60 ~ "41-60", # 41 - 60 mph
                    im_maxspeed > 60 ~ "61" # over 60 mph
                  )
    ) %>% 
    # tactile paving: yes, no
    dplyr::mutate(im_tactile = dplyr::case_when(
      ! tactile_paving %in% c("no", "incorrect", "bad") & ! is.na(tactile_paving) 
      ~ "yes",
      ! is.na(tactile_paving)
      ~ "no"
    )
    ) %>% 
    # surface: paved, unpaved, or other
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
    # surface: even  or not
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
    # width: under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      im_width =  width %>% 
        parse_number(),
      im_width = case_when(
        im_width > 0 & im_width < 1.5 ~ " 1.5",
        im_width <= 1.5 & im_width <= 2 ~ "1.5 - 2",
        im_width > 2 ~ "2"
      )
    ) %>% 
    # estimated width: under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      im_width_est = est_width %>% 
        parse_number(),
      im_width_est = case_when(
        im_width_est > 0 & im_width_est < 1.5 ~ " 1.5",
        im_width_est <= 1.5 & im_width_est <= 2 ~ "1.5 - 2",
        im_width_est > 2 ~ "2"
      )
    )
}


# filter out motorways + apply IM function
wy_im = wy %>% filter(!is.na(highway) & highway != "motorway" & highway != "motorway_link") %>%  inclusive_mobility_get() 

# proportions
## to see code behind proportions (referenced as Timaite et al 2022 in the abstract), see 
## code for GISRUK (GISRUK.R) here:
## https://github.com/udsleeds/openinfra/tree/main/GISRUK_paper

## Below I demonstrate how to find out and plot the proportions of tags created by the IM function, 
## yet this is not discussed in the submitted abstract. Code can be adjusted to return
## proportions for any tag

## proportion function
im_proportion_yes = function(osm_im, im_col){
  (osm_im %>% filter(im_col == "yes") %>% nrow()) / osm_im %>% nrow()
}

footpath_prop = im_proportion_yes(wy_im, wy_im$im_footpath) %>% 
  cbind("footpath") %>% 
  as.data.frame() %>% 
  setNames(c("prop", "value"))

footway_prop = im_proportion_yes(wy_im, wy_im$im_footway) %>%
  cbind("footway") %>% 
  as.data.frame() %>% 
  setNames(c("prop", "value"))

joined = rbind(footpath_prop,
               footway_prop) %>% 
  as.data.frame() 

## visualizations
### plot a histogram
ggplot2::ggplot(joined,
                aes(x = value,
                    y = prop))+
  geom_bar(stat = "identity")

### plot an interactive
tmap::tmap_mode("view")
tmap::tm_shape(wy_im %>% filter(im_footpath == "yes"))+
  tmap::tm_lines(col = "red")+
  tmap::tm_shape(wy_im %>% filter(im_footway == "yes"))+
  tmap::tm_lines(col = "blue")

# get Leeds Central Council footfall data
## LCC data is open and can be downloaded from the CDRC website (registration may be needed): 
## https://data.cdrc.ac.uk/dataset/leeds-city-council-footfall-camera-aggregated-data
## LCC data has also been uploaded to the project's GH repository to improve 
## accessibility and reproducibility of this code. 

footfall = read.csv(url("https://github.com/udsleeds/openinfra/releases/download/v0.1/lcc_footfall.csv"))

# a character vector of unique locations
location_vec = footfall %>% 
  select(Location) %>% 
  unique() %>% 
  unlist() %>% 
  as.vector()

### excluding Dortmund Square as it's not a highway
### also aggregating multiple footfall count locations on the same street 
### as the focus in on streets
location_list = c("Albion Street",
                  "Briggate",
                  "Commercial Street",
                  "The Headrow",
                  "Park Row")

## filter OSM data based on location_list
osm_locations = wy_im %>%
  filter(name %in% location_list)
osm_locations %>% nrow()

# plotting these locations based on OSM name tag
tmap::tmap_mode("view")
osm_locations %>% tmap::qtm()

### defining central Leeds as that's the area we need
### there are sterets with the names in `osm_locations` which are not in Leeds. We need Leeds only to match footfall data
## define a box (central Leeds)
# map_locations = mapview::mapview(osm_locations)
# map_leeds_locations = mapedit::editMap(map_locations)

## create a bounding box
# bbox = sf::st_bbox(map_leeds_locations$drawn$geometry)

## crop the df based on bbox
lcc = sf::st_crop(wy_im,
                   xmin = -1.552045,
                   ymin = 53.794796,
                   xmax = -1.536274,
                   ymax = 53.801433)

lcc_locations = lcc %>% 
  filter(name %in% location_list)
lcc_locations %>% tmap::qtm()

## check if the streets have data needed for IM and if so, what
lcc_locations %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(!is.na(im_surface)) %>% 
  dplyr::select(im_surface) %>% 
  table()

lcc_locations %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(!is.na(im_tactile)) %>% 
  dplyr::select(im_tactile) %>% 
  table()

tmap::tm_shape(lcc_locations)+
  tmap::tm_lines('im_surface',
                 palette = "Dark2",
                 colorNA = "black",
                 lwd = 2)

tmap::tm_shape(lcc_locations)+
  tmap::tm_lines('im_surface_paved',
                 palette = "Dark2",
                 colorNA = "black",
                 lwd = 2)
tmap::tm_shape(lcc_locations)+
  tmap::tm_lines('im_tactile')

# create a basic index

lcc_locations_index = lcc_locations %>% 
  # sf::st_drop_geometry() %>% 
  dplyr::mutate(
    foot_index = dplyr::case_when(
      im_footway == "yes" |
        im_footpath == "yes" |
        im_footway_imp == "yes"
      ~ 1,
      TRUE~ 0
    ),
    kerb_index = dplyr::case_when(
      im_kerb == "flush"
      ~ 1,
      TRUE ~ 0
      ),
    surface_index = dplyr::case_when(
      im_surface == "even" 
      ~ 1,
      TRUE ~ 0
    ),
    tactile_index = dplyr::case_when(
      im_tactile == "yes"
      ~ 1,
      TRUE ~ 0
    ),
    width_index = dplyr::case_when(
      !is.na(im_width) |
        !is.na(im_width_est) ~ 1,
      TRUE ~ 0 
    )
      )  %>% 
  dplyr::select(dplyr::contains("_index")) %>% 
  dplyr::mutate(index = rowSums(.[,1:5, drop = T])) # issue that helped me here: https://github.com/r-spatial/sf/issues/497

lcc_locations_index %>% dplyr::pull(index) %>% table()

## mapping index (interactive)
tmap::tm_shape(lcc_locations_index)+
  tm_lines("index",
           palette = "Dark2",
           as.count = TRUE,
           lwd = 2)

# a visualisation for Figure 1 in the abstract
## first create a new sf object with column values as referring to linestring type (will be helpful in creating a legend later)
lcc_new = lcc |> 
  dplyr::mutate(footpaths = dplyr::case_when(im_footpath == "yes" ~ "footpath",
                                             TRUE ~ "NA"),
                footways = dplyr::case_when(im_footway == "yes" ~ "footway",
                                            TRUE ~ "NA"),
                footways_implied = dplyr::case_when(im_footway_imp == "yes" ~ "implied footway",
                                                    TRUE ~ "NA")) 
## create subsets that will later be joined by row. This will create a column that can be mapped and colored by its values 
## (values were defined in the code chunk above)
lcc_footway = lcc_new |>
  dplyr::filter(footways == "footway") |> 
  dplyr::select(footways) |> 
  dplyr::rename("Highway type" = footways)
lcc_footpath = lcc_new |>
  dplyr::filter(footpaths == "footpath") |> 
  dplyr::select(footpaths) |> 
  dplyr::rename("Highway type" = footpaths)
lcc_footway_imp = lcc_new |>
  dplyr::filter(footways_implied == "implied footway") |> 
  dplyr::select(footways_implied) |> 
  dplyr::rename("Highway type" = footways_implied)

## join by row
lcc_joined = rbind(lcc_footway, lcc_footpath, lcc_footway_imp)

# plot
tmap::tmap_mode("plot")
cl_map = tmap::tm_shape(lcc_joined) +
    tmap::tm_lines(col = "Highway type",
                   palette = "Dark2") +
  
  tmap::tm_shape(lcc |> filter(!is.na(im_kerb)) |> dplyr::rename("Kerb" = im_kerb)) +
    tmap::tm_dots("Kerb",
                palette = "magma",
                shape = 4,
                size = 0.5) +
  
  #tmap::tm_shape(lcc |> filter(!is.na(im_crossing)) |> dplyr::rename("Crossings" = im_crossing)) +
  tmap::tm_shape(sf::st_as_sf(lcc) |> dplyr::filter(!is.na(im_crossing)) |> dplyr::rename("Crossings" = im_crossing) |> dplyr::select("Crossings")) +   
    tmap::tm_symbols(palette = "red",
                     shape = 3,
                     size = 0.45) +
  #tmap::tm_dots(col = "Crossings",
    #              palette = "red",
    #              shape = 3,
    #              size = 0.45) + 
  
    tmap::tm_layout(legend.position = c("left", "bottom"),
                  legend.bg.color = "white",
                  frame = FALSE)

#tmap::tmap_save(tm = cl_map,
#                filename = "sotm2022/somt2022_figure",
#                units = 150)


# ------------------------SOTM2022 P Figures--------------------------------------
tmap::tmap_mode("plot")

crossings_df = lcc %>% dplyr::filter(! im_crossing %in%  c("no")) #dplyr::filter(! is.na(im_crossing))#

  # Plots highway types
tmap::tm_shape(lcc_joined) +
    tmap::tm_lines(col = "Highway type",
                   palette = "Dark2") +
  # Plots IM_Kerbs
  tmap::tm_shape(lcc %>% filter(!is.na(im_kerb)) %>% rename("Kerb" = im_kerb)) +
    tmap::tm_dots("Kerb",
                palette = "magma",
                shape = 4,
                size = 0.5) +
  
  tmap::tm_shape(crossings_df %>% rename("Crossings" = im_crossing) %>% select("Crossings")) + 
    tmap::tm_symbols(col = "Crossings",
                     palette = c("red", "blue", "green"),
                     shape = 1, 
                     size = 0.5) + 
  
  tmap::tm_layout(legend.position = c("left", "bottom"),
                  legend.bg.color = "white",
                  frame = FALSE)

# James' figs. presentation -----------------------------------------------

# Setting up the circular buffer around specified (long, lat) coord
crs = "WGS84"
place_point = c(-1.548567, 53.801277)
# Desired (m) radius around desired point
radius = 5000 # <- meters - 5km
# Converts point coord into a sf object (so we can use st_buffer)
point_table <- data.frame(place=("Location"), lon=(place_point[1]), lat=(place_point[2]))
point_sf = st_as_sf(point_table, coords=c("lon", "lat"), crs=crs)
# Define the circle buffer around our desired location
circle_buffer = sf::st_buffer(point_sf, dist = radius)


# Get data ----------------------------------------------------------------
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

# Total dataset for leeds - no travel mode specified
total_place = osmextract::oe_get(
  place = place_name,
  provider = provider,
  boundary = circle_buffer,
  boundary_type = "clipsrc",
  layer = "lines",
  never_skip_vectortranslate = TRUE,
  force_download = TRUE,
  quiet = FALSE,
  extra_tags = all_extra_tags
)

total_place = total_place %>% dplyr::filter(! is.na(highway))

# Recategorise data and plot figs -----------------------------------------

# SOTM2022 Plots
osm_sf = total_place

osm_sf_road_recoded = oi_road_names(osm_sf) # recode_road_class(osm_sf) #openinfra::recode_road_class(osm_sf)
data_pack = oi_clean_maxspeed_uk(osm_sf_road_recoded) #openinfra::oi_clean_maxspeed_uk(osm_sf_road_recoded)
data_pack_IM = oi_inclusive_mobility(data_pack)
data_pack_lit = oi_is_lit(data_pack)
#data_pack_short = data_pack %>% dplyr::select(c("osm_id", "highway", "road_desc", "oi_maxspeed"))
data_pack_IM = data_pack_IM %>% dplyr::select(c("highway", "im_footway", "im_footpath", "im_tactile"))

data_pack = oi_road_names(osm_sf)
data_pack_road_name = oi_road_names(osm_sf)
data_pack_cycle = oi_active_cycle(osm_sf)
data_pack_walk = oi_active_walk(osm_sf)
data_pack_maxspeed = oi_clean_maxspeed_uk(osm_sf)
data_pack_IM = oi_inclusive_mobility(osm_sf)
data_pack_lit = oi_is_lit(osm_sf)
data_pack_road_desc = oi_recode_road_class(osm_sf)



tmap_mode("plot")

#osm_sf = osm_sf %>% dplyr::filter(! is.na(highway)) # COMMENT OUT AFTER

tmap::tm_shape(data_pack_lit |> dplyr::select(c("oi_is_lit"))) +
  tmap::tm_lines(col = "oi_is_lit", title.col = "Lighting presence") +
  tmap::tm_layout(title = "Presence of lighting on ways within 5mk of Leeds City Centre", legend.bg.color = "white")
#'---------------------------------------______________________________________
# Re-coded Road Descriptions / Class
fig_road_desc = tmap::tm_shape(data_pack_road_desc %>% dplyr::select("oi_road_desc")) + 
  tmap::tm_lines(col = "oi_road_desc", title.col = "Recoded Road Descriptions") + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_road_desc, "fig_road_desc.jpg")

# OSM Highway Values
fig_norm_highway = tmap::tm_shape(data_pack %>% dplyr::select("highway")) + 
  tmap::tm_lines(col = "highway", title.col = "OSM Highway Values") + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_norm_highway, "fig_norm_highway.jpg")

# oi_active_cycle() oi_cycle
fig_active_cycle = tmap::tm_shape(data_pack_cycle %>% dplyr::select("oi_cycle")) + 
  tmap::tm_lines(col = "oi_cycle", title.col = "Cyclable Ways", palette = c("red", "green")) + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_active_cycle, "fig_active_cycle.jpg")

# oi_active_walk() oi_walk
fig_active_walk = tmap::tm_shape(data_pack_walk %>% dplyr::select("oi_walk")) + 
  tmap::tm_lines(col = "oi_walk", title.col = "Walkable Ways", palette = c("red", "green")) + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_active_walk, "fig_active_walk.jpg")

# oi_clean_maxspeed_uk() oi_maxspeed
fig_maxspeed = tmap::tm_shape(data_pack_maxspeed %>% dplyr::select("oi_maxspeed")) + 
  tmap::tm_lines(col = "oi_maxspeed", title.col = "Recategorised Maxspeed") + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_maxspeed, "fig_maxspeed.jpg")

# oi_is_lit() oi_is_lit
fig_is_lit = tmap::tm_shape(data_pack_lit %>% dplyr::select("oi_is_lit")) + 
  tmap::tm_lines(col = "oi_is_lit", title.col = "Presence of Lighting") + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_is_lit, "fig_is_lit.jpg")

# oi_road_names() oi_name
fig_road_names = tmap::tm_shape(data_pack_road_name %>% dplyr::select("oi_name")) + 
  tmap::tm_lines(col = "oi_name", title.col = "OSM Road Names") + 
  tmap::tm_layout( legend.bg.alpha = 0.5, legend.bg.color = "white" )
tmap::tmap_save(fig_road_names, "fig_road_names.jpg")

