# Aim: create maps showing UK transport regions as of 2022

library(tmap)
library(tidyverse)
tmap_mode("view")
sf::sf_use_s2(FALSE)

# Read-in previously created regions:
# regions_dft_2020 = sf::read_sf("data-small/regions_dft_2020.geojson")
regions_dft_2020 = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/regions_dft_2020.geojson")
tm_shape(regions_dft_2020) + tm_polygons("Level")
# regions_atf = read.csv("data-small/atf-funds.csv")

# Read-in ATF funding allocations from: 
# https://www.gov.uk/government/publications/emergency-active-travel-fund-local-transport-authority-allocations/emergency-active-travel-fund-total-indicative-allocations
regions_atf = readr::read_csv("https://github.com/udsleeds/openinfra/raw/main/data-small/atf-funds.csv")

# Read in 2021 LAD data
lads_uk_2021 = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/Local_Authority_Districts_(December_2021)_UK_BUC.geojson")


summary(regions_dft_2020$Name %in% regions_atf$Name) # 26 match
regions_atf = regions_atf %>% 
  mutate(
    name_short = str_trim(str_replace_all(Name, "CA|UA|JTC|ITA", ""))
    ) 
regions_atf$name_short
summary(regions_dft_2020$Name %in% regions_atf$name_short) # 76 match
setdiff(regions_dft_2020$Name, regions_atf$name_short)
# [1] "Greater London"  "Cornwall"        "Isles of Scilly" # no ilses of scilly
regions_atf = regions_atf %>% 
  mutate(name_short = case_when(
    str_detect(name_short, pattern = "London") ~ "Greater London",
    str_detect(name_short, pattern = "Cornwall") ~ "Cornwall",
    TRUE ~ name_short
  )) %>% 
  rename(Level_long = Level)

regions_dft_2020_without_cornwall_or_scilly = regions_dft_2020 %>% 
  filter(!str_detect(Name, pattern = "Corn|Scill"))

cornwall = regions_dft_2020 %>% 
  filter(str_detect(Name, pattern = "Corn"))
cornwall$n = 2
cornwall$LA_names = "Cornwall,Isles of Scilly"
cornwall_geometry = regions_dft_2020 %>% 
  filter(str_detect(Name, pattern = "Corn|Scill")) %>% 
  sf::st_union()
qtm(cornwall_geometry)
cornwall$geometry = cornwall_geometry

regions_dft = rbind(regions_dft_2020_without_cornwall_or_scilly, cornwall)
regions_dft = regions_dft %>% 
  rename(name_short = Name)
regions_dft = inner_join(regions_dft, regions_atf)

# Generate look-up from LADs to new regions
lads_uk_2021_centroids = lads_uk_2021 %>% 
  sf::st_centroid()
lads_uk_2021_joined = st_join(lads_uk_2021_centroids, regions_dft)
summary(lads_uk_2021_joined$LAD21NM == lads_uk_2021$LAD21NM) ## all true
lads_uk_2021_joined$geometry_polygon = lads_uk_2021$geometry
plot(lads_uk_2021)

# lads without a region
lads_uk_2021_joined %>% 
  filter(is.na(Name)) %>% 
  qtm() +
  qtm(regions_dft)
# Some of the points just miss regions, buffer before join:
# dist of 750 is minimum to capture all points in regions
regions_dft_buffered = stplanr::geo_buffer(regions_dft, dist = 750)
lads_uk_2021_joined = st_join(lads_uk_2021_centroids, regions_dft_buffered)
summary(lads_uk_2021_joined$LAD21NM == lads_uk_2021$LAD21NM) ## all true
lads_uk_2021_joined$geometry_polygon = lads_uk_2021$geometry
lads_uk_2021_joined %>% 
  filter(is.na(Name)) %>% 
  qtm() +
  qtm(regions_dft_buffered) +
  tm_scale_bar()

# clean up
lads_uk_2021_joined$geometry = lads_uk_2021$geometry
lads_joined = lads_uk_2021_joined %>% 
  select(LAD21NM, name_short:Level_long) %>% 
  rename(Region_name = name_short, Region_name_long = Name)
plot(lads_joined)
qtm(lads_joined, "Region_name")
file.remove("data-small/lads_joined_2021.geojson")
sf::write_sf(lads_joined, "data-small/lads_joined_2021.geojson")
summary(is.na(lads_joined$Region_name))


# regions_new = lads_joined %>% 
#   group_by(Region_name) %>% 
#   summarise(
#     Region_name_long = first(Region_name_long),
#     N_LADs = n(),
#     LAD_names = paste(LAD21NM, collapse = ","),
#     Allocation1 = first(Allocation1),
#     Allocation2 = first(Allocation2)
#   ) %>% 
#   filter(!is.na(Region_name))
# plot(regions_new)
# regions_new$LAD_names
# 
# sf::write_sf(regions_new, "data-small/regions_new.geojson")
# 
# lads_to_regions_2021 = readr::read_csv("data-small/Local_Authority_District_to_Region_(April_2021)_Lookup_in_England.csv")
# length(unique(lads_to_regions_2021$RGN21NM)) # 9
# lads_uk_2021 = sf::read_sf("data-small/Local_Authority_Districts_(December_2021)_UK_BUC.geojson")
# lads_in_lookup = lads_uk_2021$LAD21NM %in% lads_to_regions_2021$LAD21NM
# summary(lads_in_lookup)
# qtm(lads_uk_2021[lads_in_lookup, ]) # promising: all LADS in England

# Test code:
# lads_to_regions_2020 = readRDS("~/cyipt/tempCycleways/lads.Rds")
# lads_to_regions_2020 = readRDS("~/cyipt/tempCycleways/")
# regions_dft_2020 = readRDS("~/cyipt/tempCycleways/regions_dft.Rds")
# regions_dft_2020_long = regions_dft_2020 %>%
#   sf::st_drop_geometry() %>% 
#   separate(LA_names, into = letters[1:12], sep = ",")
# nrow(regions_dft_2020) # 79
# sf::write_sf(regions_dft_2020, "regions_dft_2020.geojson")
# piggyback::pb_upload("regions_dft_2020.geojson")
