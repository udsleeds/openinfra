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

# Which rows are not being dropped?

leeds_tn_subset2_nogeom = leeds_tn_subset2 %>% sf::st_drop_geometry()
leeds_tn_test2_nogeom = leeds_tn_test2 %>% sf::st_drop_geometry()

leeds_tn_subset2_nogeom %>% nrow
leeds_tn_test2_nogeom %>% nrow

leeds_difference = leeds_tn_subset2_nogeom$osm_id  %!in% leeds_tn_test2_nogeom$osm_id
leeds_difference %>% table()
leeds_missing = leeds_tn_subset2[leeds_difference, ]

tmap::tmap_mode("view")
tmap::tm_shape(leeds_missing)+
  tmap::tm_lines()

# any rows in `leeds_tn_subset2` that have
leeds_tn_test2 %>% dplyr::filter(access %in% c("no", "private") | foot == "yes") %>% nrow()
leeds_tn_subset2 %>% dplyr::filter(access %in% c("no", "private") | foot == "yes") %>% nrow()
leeds_tn_test %>% dplyr::filter(access %in% c("no", "private") | foot == "yes") %>% nrow()
leeds_tn_subset %>% dplyr::filter(access %in% c("no", "private") | foot == "yes") %>% nrow()

leeds_tn_test2 %>% dplyr::filter(foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>% nrow()
leeds_tn_subset2 %>% dplyr::filter(foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>% nrow()
leeds_tn_test %>% dplyr::filter(foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>% nrow()
leeds_tn_subset %>% dplyr::filter(foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>% nrow()

# Andrea's solution

# packages
library(osmextract)
library(sf)
library(dplyr)

# SQL filters
sql_filters <- oe_get(
  place = "Leeds",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  vectortranslate_options = c(
    "-where", "
    (highway IS NOT NULL)
    AND
    (highway NOT IN ('abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'raceway', 'motorway', 'motorway_link') OR foot = 'yes')
    AND
    (access NOT IN ('private', 'no') OR foot = 'yes')
    AND
    (foot NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    AND
    (service NOT ILIKE 'private%' OR foot = 'yes')
    "
  ),
  quiet = TRUE
)

# Rebuild the same process with manual filters
leeds <- oe_get(
  place = "Leeds",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  quiet = TRUE
)

manual_filters <- leeds %>%
  filter(!is.na(highway)) %>%
  filter(
    ! highway %in% c(
      'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
      'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
      'proposed', 'raceway', 'motorway', 'motorway_link') | foot %in% "yes"
  ) %>%
  filter(! access %in% c('private', 'no') | foot %in% "yes") %>%
  filter(! foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>%
  filter(! grepl("private", service) | foot %in% "yes")
manual_filters %>% nrow()
sql_filters %>% nrow()

nrow(sql_filters)
#> [1] 126607
nrow(manual_filters)
#> [1] 126607

sql_filters <- oe_get(
  place = "Greater London",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  vectortranslate_options = c(
    "-where", "
    (highway IS NOT NULL)
    AND
    (highway NOT IN ('abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'raceway', 'motorway', 'motorway_link') OR foot = 'yes')
    AND
    (access NOT IN ('private', 'no') OR foot = 'yes')
    AND
    (foot NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    AND
    (service NOT ILIKE 'private%' OR foot = 'yes')
    "
  ),
  quiet = TRUE
)

gl <- oe_get(
  place = "Greater London",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  quiet = TRUE
)

manual_filters <- gl %>%
  filter(!is.na(highway)) %>%
  filter(
    ! highway %in% c(
      'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
      'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
      'proposed', 'raceway', 'motorway', 'motorway_link') | foot %in% "yes"
  ) %>%
  filter(! access %in% c('private', 'no') | foot %in% "yes") %>%
  filter(! foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>%
  filter(! grepl("private", service) | foot %in% "yes")

nrow(sql_filters)
#> [1] 347830
nrow(manual_filters)
#> [1] 347830

# adding highway = 'cycleway' & foot = 'yes' 

# sql method 1
sql_filters1 <- oe_get(
  place = "Leeds",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  vectortranslate_options = c(
    "-where", "
    (highway IS NOT NULL)
    AND
    (highway NOT IN ('abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'raceway', 'motorway', 'motorway_link', 'cycleway') OR foot = 'yes')
    AND
    (access NOT IN ('private', 'no') OR foot = 'yes')
    AND
    (foot NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    AND
    (service NOT ILIKE 'private%' OR foot = 'yes')
    "
  ),
  quiet = TRUE
)
sql_filters1 %>% nrow()
#> 124163

# sql method 2
sql_filters2 <- oe_get(
  place = "Leeds",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  vectortranslate_options = c(
    "-where", "
    (highway IS NOT NULL)
    AND
    (highway NOT IN ('abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'raceway', 'motorway', 'motorway_link'))
    OR 
    (highway = 'cycleway' AND foot = 'yes')
    AND
    (access NOT IN ('private', 'no') OR foot = 'yes')
    AND
    (foot NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    AND
    (service NOT ILIKE 'private%' OR foot = 'yes')
    "
  ),
  quiet = TRUE
)

sql_filters2 %>% nrow()
#> 133062

# manual method 1
manual_filters1 <- leeds %>%
  filter(!is.na(highway)) %>%
  filter(
    ! highway %in% c(
      'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
      'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
      'proposed', 'raceway', 'motorway', 'motorway_link', 'cycleway') | foot %in% "yes"
  ) %>%
  filter(! access %in% c('private', 'no') | foot %in% "yes") %>%
  filter(! foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>%
  filter(! grepl("private", service) | foot %in% "yes")

manual_filters1 %>% nrow()
#> 124163
sql_filters1 %>% nrow()
#> 124163

# manual method 2
manual_filters2 <- leeds %>%
  filter(!is.na(highway)) %>%
  filter(
    ! highway %in% c(
      'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
      'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
      'proposed', 'raceway', 'motorway', 'motorway_link') | 
      highway == 'cycleway' & foot %in% 'yes') %>%
  filter(! access %in% c('private', 'no') | foot %in% "yes") %>%
  filter(! foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>%
  filter(! grepl("private", service) | foot %in% "yes")
manual_filters2 %>% nrow()
#> 126598
sql_filters2 %>% nrow()
#> 133062

leeds_in_foot = leeds %>% filter(
  highway %in%c(
    'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
    'proposed', 'raceway', 'motorway', 'motorway_link') & foot == "yes")
leeds_in_foot %>% nrow()

# Andrea's version 2

sql_filters <- oe_get(
  place = "Leeds",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  vectortranslate_options = c(
    "-where", "
    (highway IS NOT NULL)
    AND
    (highway NOT IN ('abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform', 'proposed', 'raceway',
    'motorway', 'motorway_link'))
    AND
    (highway <> 'cycleway' OR foot = 'yes')
    AND
    (access NOT IN ('private', 'no'))
    AND
    (foot NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    AND
    (service NOT ILIKE 'private%' OR foot = 'yes')
    "
  ),
  quiet = TRUE
)

# Rebuild the same process with manual filters
leeds <- oe_get(
  place = "Leeds",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  quiet = TRUE
)

manual_filters <- leeds %>%
  filter(!is.na(highway)) %>%
  filter(
    ! highway %in% c(
      'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
      'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
      'proposed', 'raceway', 'motorway', 'motorway_link'
    )
  ) %>%
  filter(highway != "cycleway" | foot %in% "yes") %>%
  filter(! access %in% c('private', 'no')) %>%
  filter(! foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>%
  filter(! grepl("private", service) | foot %in% "yes")

nrow(sql_filters)
#> [1] 123949
nrow(manual_filters)
#> [1] 123949

# checking the OR/AND interaction
sql_filters1 <- oe_get(
  place = "Leeds",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  vectortranslate_options = c(
    "-where", "
    (highway IS NOT NULL)
    AND
    (highway NOT IN ('abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform', 'proposed', 'raceway',
    'motorway', 'motorway_link', 'cycleway')
    OR
    (highway = 'cycleway' AND foot = 'yes'))
    AND
    (access NOT IN ('private', 'no'))
    AND
    (foot NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    AND
    (service NOT ILIKE 'private%' OR foot = 'yes')
    "
  ),
  quiet = TRUE
)
sql_filters1 %>% nrow() 
#> 123949
sql_filters1 %>% nrow() == sql_filters %>% nrow()
#> TRUE

manual_filters1 <- leeds %>%
  filter(!is.na(highway)) %>%
  filter(
    ! highway %in% c(
      'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
      'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
      'proposed', 'raceway', 'motorway', 'motorway_link', 'cycleway'
    ) | highway == 'cycleway' & foot == 'yes' 
  ) %>%   
  filter(! access %in% c('private', 'no')) %>%
  filter(! foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>%
  filter(! grepl("private", service) | foot %in% "yes")

manual_filters1 %>% nrow
#> 123949

# If I delete 'cycleway' from the NOT IN highway list
manual_filters2 <- leeds %>%
  filter(!is.na(highway)) %>%
  filter(
    ! highway %in% c(
      'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
      'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
      'proposed', 'raceway', 'motorway', 'motorway_link'
    ) | highway == 'cycleway' & foot == 'yes' 
  ) %>%   
  filter(! access %in% c('private', 'no')) %>%
  filter(! foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>%
  filter(! grepl("private", service) | foot %in% "yes")

manual_filters2 %>% nrow()
#> 126393



# ==========
# network
leeds_network = osmextract::oe_get_network(
  place = "Leeds",
  mode = "walking",
  provider = "bbbike",
  force_download = TRUE,
  force_vectortranslate = TRUE
)

leeds_network %>% nrow()
#> 126782

# SQL filters
sql_filters <- oe_get(
  place = "Leeds",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  vectortranslate_options = c(
    "-where", "
    (highway IS NOT NULL)
    AND
    (highway NOT IN ('abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
    'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform', 'proposed', 'raceway',
    'motorway', 'motorway_link'))
    AND
    (highway <> 'cycleway' OR foot IN ('yes', 'designated', 'permissive', 'destination'))
    AND
    (access NOT IN ('private', 'no'))
    AND
    (foot NOT IN ('private', 'no', 'use_sidepath', 'restricted'))
    AND
    (service NOT ILIKE 'private%')
    "
  ),
  quiet = TRUE
)

# Rebuild the same process with manual filters
leeds <- oe_get(
  place = "Leeds",
  never_skip_vectortranslate = TRUE,
  extra_tags = c("access", "foot", "service"),
  quiet = TRUE
)

manual_filters <- leeds %>%
  filter(!is.na(highway)) %>%
  filter(
    ! highway %in% c(
      'abandonded', 'bus_guideway', 'byway', 'construction', 'corridor', 'elevator',
      'fixme', 'escalator', 'gallop', 'historic', 'no', 'planned', 'platform',
      'proposed', 'raceway', 'motorway', 'motorway_link'
    )
  ) %>%
  filter(highway != "cycleway" | foot %in% c('yes', 'designated', 'permissive', 'destination')) %>%
  filter(! access %in% c('private', 'no')) %>%
  filter(! foot %in% c('private', 'no', 'use_sidepath', 'restricted')) %>%
  filter(! grepl("private", service))

nrow(sql_filters)
#> [1] 125395
nrow(manual_filters)
#> [1] 125395
nrow(leeds_network)
#> 126782

'%!in%' = Negate('%in%')    
leeds_network %>% 
  filter(highway == 'cycleway' & foot %!in% c('yes', 'designated', 'permissive', 'destination')) %>% 
  nrow()
#> 999
leeds_network %>% 
  filter(highway == 'cycleway' & foot %in% c('yes', 'designated', 'permissive', 'destination')) %>% 
  nrow()
#> 1916
leeds_network %>% 
  filter(highway == 'cycleway') %>% 
  nrow()
#> 2915

tmap::tmap_mode("view")
leeds_network %>% 
  filter(highway == 'cycleway' & foot %!in% c('yes', 'designated', 'permissive', 'destination')) %>% 
  tmap::qtm()


