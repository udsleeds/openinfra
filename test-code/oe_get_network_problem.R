library(osmextract)
library(tidyverse)
library(sf)

region_leeds = "Leeds"
tags_needed = c("access",
                "foot",
                "service")

leeds_net_walking = osmextract::oe_get_network(
  place = region_leeds,
  mode = "walking", # What mode of transport is to be returned? Default is cycling
  provider = "bbbike",
  force_download = TRUE,
  force_vectortranslate = TRUE
)

leeds_tn = osmextract::oe_get(
  place = region_leeds,
  provider = "bbbike", 
  layer = "lines", 
  force_download = TRUE, 
  force_vectortranslate = TRUE,
  extra_tags = tags_needed
)
leeds_tn_test = osmextract::oe_get(
  place = region_leeds,
  provider = "bbbike", 
  layer = "lines", 
  force_download = TRUE, 
  force_vectortranslate = TRUE,
  extra_tags = tags_needed,
  vectortranslate_options = c(
    "-where", "
    ((highway IS NOT NULL AND highway NOT IN (
    'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'raceway', 'motorway', 'motorway_link'
    )) OR (
    highway = 'cycleway' AND foot = 'yes'
    ))
    AND
    (access IS NULL OR access NOT IN ('private', 'no') OR foot = 'yes')
    "
  )
)
leeds_tn_test2 = osmextract::oe_get(
  place = region_leeds,
  provider = "bbbike", 
  layer = "lines", 
  force_download = TRUE, 
  force_vectortranslate = TRUE,
  extra_tags = tags_needed,
  vectortranslate_options = c(
    "-where", "
    ((highway IS NOT NULL AND highway NOT IN (
    'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'raceway', 'motorway', 'motorway_link'
    )) OR (
    highway = 'cycleway' AND foot = 'yes'
    ))
    AND
    (access IS NULL OR access NOT IN ('private', 'no') OR foot = 'yes')
    AND
    (foot IS NULL OR foot NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    "
  )
)


highway_not = c("abandonded", "bus_guideway", "byway", "construction", "corridor", "elevator", "fixme", "escalator", "gallop", "historic", "no", "planned", "platform", "proposed", "raceway", "motorway", "motorway_link")

'%!in%' = Negate('%in%')                
leeds_tn_subset = leeds_tn %>% 
  filter(!is.na(highway) &  highway %!in% highway_not |  highway == "cycleway" & foot == "yes") %>% 
  filter(is.na(access) | access %!in% c("no", "private") | foot == "yes") 

leeds_tn_subset2 = leeds_tn %>% 
  filter(!is.na(highway) &  highway %!in% highway_not |  highway == "cycleway" & foot == "yes") %>% 
  filter(is.na(access) | access %!in% c("no", "private") | foot == "yes") %>% 
  filter(is.na(foot) | foot %!in% c('private', 'no', 'use_sidepath', 'restricted'))

leeds_tn_test %>% dim()
leeds_tn_subset %>% dim()
leeds_tn_test2 %>% dim()
leeds_tn_subset2 %>% dim()
