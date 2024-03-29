# Aim: create maps of Manchester city centre for walking and cycling Innovations
library(tidyverse)
library(stplanr)
library(osmextract)
library(tmap)
library(maptiles)

# route_mcr = route_google(from = "Manchester Piccadily", to = "Manchester Conference Centre", mode = "walking")
# sf::write_sf(route_mcr, "data-small/route_mcr.geojson")
route_mcr = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/route_mcr.geojson")

# m = mapview::mapview(route_mcr) # looks good!
# m
# Create custom bounding box:
# map_edited = mapedit::editMap(m)
# bbox = sf::st_bbox(map_edited$drawn$geometry)
# bbox
# xmin      ymin      xmax      ymax 
# -2.236786 53.474959 -2.229480 53.477265 
bbox = c(-2.236786, 53.474959, -2.229480, 53.477265)

# get satellite data
# bbox_projected = sf::st_transform(map_edited$drawn$geometry, "EPSG:3857")
# Get tiles: https://github.com/riatelab/maptiles/
# tiles = get_tiles(bbox_projected, crop = TRUE)
# get OSM data
et = c("maxspeed")
osm_highways_mcr = oe_get_network(place = "greater manchester", mode = "walking")
osm_tags = osmextract::oe_get_keys(osm_highways_mcr)
# 300+ keys, starting with:
# [1] "maxspeed"                              "surface"                              
# [3] "oneway"                                "source:name"                          
# [5] "lit"                                   "bicycle"   
et = c("maxspeed", "bicycle", "cycleway", "lit", "cycleway:left", "wheelchair", "foot")
osm_highways_mcr = oe_get_network(place = "greater manchester", mode = "walking", extra_tags = et)
table(osm_highways_mcr$cycleway_left)
table(osm_highways_mcr$pedestrian)
osm_df = osm_highways_mcr %>% sf::st_drop_geometry()
osm_highways_tagged = rbind(
  # osm_df %>% filter(!is.na(cycleway_left)) %>% mutate(key = "cycleway_left", value = cycleway_left),
  osm_df %>% filter(!is.na(wheelchair)) %>% mutate(key = "wheelchair", value = wheelchair),
  osm_df %>% filter(!is.na(foot)) %>% mutate(key = "foot", value = foot),
  # osm_df %>% filter(!is.na(cycleway)) %>% mutate(key = "cycleway", value = cycleway),
  osm_df %>% filter(!is.na(lit)) %>% mutate(key = "lit", value = lit),
  osm_df %>% filter(!is.na(bicycle)) %>% mutate(key = "bicycle", value = bicycle),
  osm_df %>% filter(str_detect(highway, "footway|living_street|path|pedestrian|steps|cycle")) %>%
    mutate(key = "highway", value = highway),
  osm_df %>% filter(!is.na(maxspeed)) %>% mutate(key = "maxspeed", value = maxspeed)
)
osm_highways_tagged_large_categories = osm_highways_tagged %>% 
  group_by(key, value) %>% 
  mutate(n = n()) %>% 
  filter(n > 10) %>% 
  ungroup()
summary(osm_highways_tagged_large_categories$cycleway_left)
ggplot(osm_highways_tagged_large_categories) +
  geom_bar(aes(value)) +
  facet_wrap(~key, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

# osm_in_bbox = osm_highways_mcr[map_edited$drawn$geometry, , op = sf::st_within]
osm_in_bbox = osm_highways_mcr[map_edited$drawn$geometry, ]

table(osm_in_bbox$highway)
# footway           path     pedestrian        primary    residential      secondary secondary_link 
# 74              2              5              9             14              3              1 
# service          steps       tertiary   unclassified 
# 22             14              4              5 
osm_recategorised = osm_in_bbox %>% 
  mutate(highway = case_when(
    str_detect(highway, "path|link|tert|unclassified") ~ "other",
    TRUE ~ highway
  ))
sf::st_write(osm_recategorised, "data-small/osm_recategorised.geojson")
osm_recategorised %>% select(highway) %>% plot()
osm_recategorised_sln = stplanr::SpatialLinesNetwork(osm_recategorised)
osm_clean = stplanr::sln_clean_graph(osm_recategorised_sln)
osm_clean_sf = osm_clean@sl
m1 = tm_shape(osm_recategorised, bbox = bbox) +
  tm_lines("highway", lwd = 3, palette = "Set2") +
  tm_layout(legend.frame = TRUE, legend.bg.alpha = 0.5, legend.position = c("left", "top"))
m1
tmap_save(m1, "m1.png")
mapview::mapview(osm_recategorised)
# Get satellite data ------------------------------------------------------
library(ceramic)

# Source: https://github.com/hypertidy/ceramic/issues/42
library(gdalio)
get_im_bing_from_sf = function(x, dimension = c(512, 512), resample = "bilinear") {
  bb = sf::st_bbox(x)
  ex = as.numeric(bb)[c(1, 3, 2, 4)]
  g = gdalio_set_default_grid(list(extent = ex, dimension = dimension, projection = sf::st_crs(x)$wkt))
  source(system.file("raster_format/raster_format.codeR", package = "gdalio", mustWork = TRUE))
  ve_src <- '<GDAL_WMS>
  <Service name="VirtualEarth">
    <ServerUrl>http://a${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90</ServerUrl>
  </Service>
  <MaxConnections>4</MaxConnections>
  <Cache/>
</GDAL_WMS>'
  gdalio_terra(ve_src, bands = 1:3, band_output_type = "integer", resample = resample)
}
route_mcr_projected = sf::st_transform(map_edited$drawn$geometry, 2264)
map_satellite_terra = get_im_bing_from_sf(route_mcr_projected, dimension = c(512, 1024))
terra::plotRGB(map_satellite_terra)

