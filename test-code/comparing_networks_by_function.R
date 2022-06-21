library(osmextract)
library(sf)
library(tmap)
library(dplyr)
library(reprex)

# This script investigates the pre-defined osmextract networks (oe_get_network(network_type = c("walking", "cycling",  "driving")))
# and tries to reproduce these using oe_get() in a reproducible manner. 

# Remove variables in workspace
rm(list=ls())

# Set place to Sheffield
place_name = "Sheffield"

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

# Prints the perfect provider match
print(c(place_name, "provider is:",  provider))


# Creation of the default cycling network
default_cycling = osmextract::oe_get_network(
  place = place_name,
  provider = provider,
  mode = "cycling",
  force_download = TRUE,
  force_vectortranslate = TRUE,
  extra_tags = c("bicycle", "access", "service"),
  quiet = FALSE
)

# Creation of the SQL query (vector_translate_options) cycling network.
SQL_cycling = osmextract::oe_get(
  place = place_name,
  provider = provider,
  layer = "lines",
  extra_tags =  c("bicycle", "access", "service"),
  force_download = TRUE,
  force_vectortranslate = TRUE,
  never_skip_vectortranslate = TRUE,
  vectortranslate_options = c(
    "-where", "
    (highway IS NOT NULL)
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
  quiet = FALSE
)

# Creation of total network, manual filtering applied after. 
total_place = osmextract::oe_get(
  place = place_name,
  provider = provider,
  never_skip_vectortranslate = TRUE,
  force_download = TRUE,
  quiet = FALSE,
  extra_tags = c("bicycle", "service", "access")
)

# Here we try to re-create the same cycling network as above, but by manually 
# filtering out features from the total ("walking", "driving", "cycling") network
# to leave the desired network behind

manual_cycling = total_place %>% 
  # highway IS NOT NULL
  filter(!is.na(highway)) %>%
  # highway != number of values
  filter(
    ! highway %in% c('abandoned', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
                     'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
                     'proposed', 'raceway', 'steps')
  ) %>%
  # Remove tags UNLESS bicycle = list of tags
  filter(! highway %in% c('motorway', 'motorway_link', 'footway',
                          'bridleway','pedestrian') | bicycle %in% c('yes', 'designated', 'permissive', 'destination')) %>%
  # Remove if access == private/no
  filter(! access %in% c('private', 'no')) %>%
  # Remove if bicycle == private/no/use_sidepath/restricted
  filter(! bicycle %in% c('private', 'no', 'use_sidepath', 'restricted')) %>%
  # Remove if any service == value contains the word 'private' (i.e. private, private_road, private_access etc.)
  filter(! grepl("private", service))

# From my understanding, the dimensions of all of the above networks should be the same? 
print(c("Default", dim(default_cycling)))
print(c("SQL", dim(SQL_cycling)))
print(c("Manual", dim(manual_cycling)))
