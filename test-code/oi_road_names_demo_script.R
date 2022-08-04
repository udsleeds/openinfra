# testing of the new oi_road_names function 

devtools::load_all()

#remotes::install_github("udsleeds/openinfra")
library(openinfra)
library(osmextract)

all_extra_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                   "kerb", "footway", "sidewalk", "cycleway", "segregated", "highway",
                   "crossing", "lit", "tactile_paving", "surface", "smoothness", "width",
                   "est_width", "lit_by_led", "name", "ref")

leeds_data = osmextract::oe_get(
  place = "Leeds",
  layer = "lines",
  provider = "bbbike",
  extra_tags = all_extra_tags,
  force_vectortranslate = TRUE,
  quiet = FALSE
)

# Remove all features that have 
leeds_data = leeds_data %>% dplyr::filter(! is.na(highway))

test_output = oi_road_names(leeds_data)

test_output_review = test_output %>% dplyr::select(c("name", "ref", "oi_name"))

no_NA_review = test_output_review %>% dplyr::filter(! is.na(name)) %>% dplyr::filter(! is.na(ref))