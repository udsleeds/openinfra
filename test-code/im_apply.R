# AIM:
# 1. create an index of inclusive mobility
# 2. correlate that index with economic activity

# ideas: index, factor analysis

# libraries

library(osmextract)
library(pct)
library(tidyverse)
library(tmap)
library(sf)

# get data
wy = readRDS(url("https://github.com/udsleeds/openinfra/releases/download/v0.1/wy_lines_21-03-2022.RDS"))

wy_walking = wy %>% 
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
  filter(! grepl("private", "service"))

# get data with IM recategorizations
wy_im = inclusive_mobility_get(wy_walking)

# pct for west yorkshire

wy_pct = readRDS(url("https://github.com/udsleeds/openinfra/releases/download/v0.1/wy_pct.Rds"))

wy_pct_geom = wy_pct %>% rename(geom = geometry,
                                      foot_census = foot,
                                bicycle_census = bicycle) %>% 
  select(foot_census, bicycle_census, all, car_driver, geo_code, geom, geo_name)

# census for the North

census = read_csv("~/Desktop/deprivation_wy/Data_deprivation_wy.csv") %>% dplyr::select(-c(GEO_TYP2, GEO_TYPE))

census_pct = dplyr::full_join(census,
                              wy_pct_geom,
                              by = c("GEO_CODE" = "geo_code")) %>% 
  filter(! CDU_ID %in% c(10, 49)) %>% 
  sf::st_as_sf(sf_column_name = "geom")

tmap::tm_shape(census_pct)+
  tmap::tm_polygons("F996",
                    legend.show = FALSE)
# might ask to define:
# tmap_options(max.categories = 1000)
# tmap_options(check.and.fix = TRUE)

# ------ delete later

# wy_census_pct = sf::st_join(wy_im,
#                             sf::st_is_valid(census_pct))
# 
# wy_census_pct$geom %>% plot()
# 
# 
# wy_joined = sf::st_join(wy_im,
#                         census_pct,
#                         left = TRUE) 

# ---- stop deletion


# osm highways that intersect with the boundaries defined by pct
sf::sf_use_s2(FALSE)

wy_pct_census_int = wy_im[census_pct, op = sf::st_intersects]

wy_joined = sf::st_join(wy_pct_census_int,
                        census_pct,
                        left = TRUE)



wy_highway_n = wy_joined %>% 
  group_by(GEO_CODE) %>% 
  summarize(n_lsoa = n())

tmap::tmap_mode("plot")
wy_highway_n %>% tmap::qtm()
