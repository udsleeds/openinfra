library(tmap)
library(sf)
library(dplyr)
library(remotes)
library(osmextract)
library(openinfra)

osmextract::oe_match_pattern("Leeds")

leeds = osmextract::oe_get(
  place = "Leeds", 
  provider = "bbbike", # Indicates the provider; default is geofabrik
  layer = "lines" # Default; returns linestring geometries (highways etc) 
)


# so let's define the extra tags we want

et = c("kerb", "width", "sidewalk", "cycleway", "footway", "lit", "wheelchair",
       "maxspeed", "foot", "access", "service", "bicycle", "oneway")

leeds_2 = osmextract::oe_get(
  place = "Leeds", 
  provider = "bbbike",
  layer = "lines",
  extra_tags = et # non-default tags to download
)

# have a look at the data
leeds |> dplyr::glimpse()

# also it's useful to have a look at the downloaded data by plotting it
tmap::tm_shape(leeds |> dplyr::filter(!is.na("highway")))+
  tmap::tm_lines(col = "highway")


leeds_centre_point = sf::st_sfc(sf::st_point(c(-1.549, 53.801)), crs = "EPSG:4326")
leeds_buffer = sf::st_buffer(leeds_centre_point, dist = 2500)

leeds_defined = osmextract::oe_get(
  place = "Leeds", 
  provider = "bbbike", 
  layer = "lines", 
  extra_tags = et,
  boundary = leeds_buffer,
  boundary_type = "clipsrc"
)


leeds_second_small = leeds[leeds_buffer, op = st_within]


leeds_defined = leeds_defined |> dplyr::filter(! is.na("highway"))


leeds_cyclways = leeds_defined |> dplyr::filter(highway == "cycleway")


leeds_cycle = leeds_defined |> 
  dplyr::filter(highway == "cycleway" |
                  bicycle %in% c('yes'))


# to download cycling network as defined by `osmextract`
osme_cycle = osmextract::oe_get_network(place = "Leeds",
                                        mode = "cycling")

osme_cycle_small = osme_cycle[leeds_buffer, op = st_within]


library(tmap)

tmap_mode("view")
tm_shape(osme_cycle_small) + 
  tm_lines("highway")

leeds_drinving = osmextract::oe_get_network(place = "Leeds",
                           mode = "driving",
                          extra_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                                                          "kerb", "footway", "sidewalk", "cycleway", "segregated", "highway",
                                                          "crossing", "lit", "tactile_paving", "surface", "smoothness", "width",
                                                          "est_width", "lit_by_led", "ref")
                           )


leeds_drinving_small =  leeds_drinving[leeds_buffer,  op = st_within]

qtm(leeds_drinving)


library(openinfra)

leeds_driving_recat = openinfra::oi_recode_road_class(leeds_drinving)

tm_shape(leeds_driving_recat) + 
  tm_lines("oi_road_desc")
