# Aim: create maps showing UK transport regions as of 2022-09

library(tmap)
library(tidyverse)
tmap_mode("view")
sf::sf_use_s2(FALSE)


# Get and process LADs ------------------------------------------------------

# https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-may-2022-uk-bsc/explore?location=55.223511%2C-3.317025%2C6.92

lads = sf::read_sf("data-small/Local_Authority_Districts_(May_2022)_UK_BSC.geojson")
lad_ta_region_lookup = read_csv("data-small/lad_ta_region_lookup_atf3.csv")

lads_2022_england = lads |> 
  filter(str_detect(string = LAD22CD, pattern = "E")) |> 
  select(LAD22CD, LAD22NM)

lads_2022_scotland = lads |> 
  filter(str_detect(string = LAD22CD, pattern = "S")) |> 
  select(LAD22CD, LAD22NM)
plot(lads_2022_scotland$geometry)

lads_2022_wales = lads |> 
  filter(str_detect(string = LAD22CD, pattern = "W")) |> 
  select(LAD22CD, LAD22NM)

countries = lads |> 
  # filter(!str_detect(string = LAD22CD, pattern = "E")) |> 
  mutate(
    Country = case_when(
      str_detect(string = LAD22CD, pattern = "S") ~ "Scotland",
      str_detect(string = LAD22CD, pattern = "W") ~ "Wales",
      str_detect(string = LAD22CD, pattern = "N") ~ "Northern Ireland",
      str_detect(string = LAD22CD, pattern = "E") ~ "England"
    )
  ) |> 
  group_by(Country) |> 
  summarise(n = n(), local_authority_names = paste0(LAD22NM, collapse = ", "))

plot(countries)
sf::write_sf(countries, "data-small/countries.geojson")

lads_england_lookup = left_join(lads_2022_england, lad_ta_region_lookup)
transport_regions_2022 = lads_england_lookup |> 
  group_by(Name = Region_name) |> 
  summarise(n_lads = n(), lad_names = paste(LAD22NM, collapse = ","))

sf::write_sf(transport_regions_2022, "data-small/transport_regions_2022.geojson")

# Get and process MSOA data -----------------------------------------------


#MSOA Middle Layer Super Output Area code here
msoas = sf::read_sf('/home/james/Desktop/LIDA_OSM_Project/openinfra/openinfraresults/data/Middle_layer_Super_Output_Areas_(December_2021)_Boundaries_Full_Clipped_EW_(BFC).geojson')

msoas_2021_england = msoas |> 
  filter(str_detect(string = MSOA21CD, pattern = "E")) |> 
  select(MSOA21CD, MSOA21NM)

msoas_2021_scotland = msoas |> 
  filter(str_detect(string = MSOA21CD, pattern = "S")) |> 
  select(MSOA21CD, MSOA21NM)
#plot(msoas_2021_scotland$geometry)

msoas_2021_wales = msoas |> 
  filter(str_detect(string = MSOA21CD, pattern = "W")) |> 
  select(MSOA21CD, MSOA21NM)
#plot(msoas_2021_wales$geometry)

countries = msoas |> 
  mutate(
    Country = case_when(
      str_detect(string = MSOA21CD, pattern = "S") ~ "Scotland",
      str_detect(string = MSOA21CD, pattern = "W") ~ "Wales",
      str_detect(string = MSOA21CD, pattern = "N") ~ "Northern Ireland",
      str_detect(string = MSOA21CD, pattern = "E") ~ "England"
    )
  ) |> 
  group_by(Country) |> 
  summarise(n = n(), middle_super_ouput_names = paste0(MSOA21NM, collapse = ", "))

sf::write_sf(transport_regions_2022, "data-small/transport_regions_msoas_2022.geojson")

# Add population data -----------------------------------------------------

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates

# Download population data from ONS published 2020
u = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates/mid2020sape23dt4/sape23dt4mid2020msoasyoaestimatesunformatted.xlsx"
f = basename(u)
download.file(u, f)
# Read population data from downloaded excel file
msoas_population_meta = readxl::read_excel(f)
msoas_population = readxl::read_excel(f, sheet = 4, skip = 3)

# Uncomment below to install {ukboundaries}
# remotes::install_github("Robinlovelace/ukboundaries")

#TODO: Can this not use 2022 MSOAS ? 
# Get all 2011 msoas boundaries
msoas = ukboundaries::msoa2011_vsimple |> 
  sf::st_point_on_surface()

# Check each region in msoas_pop is in msoas
summary(msoas_population$`MSOA Code` %in% msoas$msoa11cd)

# Set 2nd column of msoas as 1st column of msoas_population [MSOA Code]
names(msoas)[2] = names(msoas_population)[1]

# Joins msoas columns (7) onto msoas_population (98) to total (104) using
# MSOA Code as a join identifier
msoas_population_joined = dplyr::left_join(msoas, msoas_population)

# Gets all English MSOAs (MSOA Codes begining "E") and returns select columns
msoas_population_england = msoas_population_joined |> 
  dplyr::filter(stringr::str_detect(string = `MSOA Code`, pattern = "E")) |> 
  dplyr::select(msoa11cd = `MSOA Code`, msoa11nm, Population = `All Ages`, `LA name (2021 boundaries)`)

# Plots the geometry of English MSOAs as points
plot(msoas_population_england$geometry)

# Save dataset of populations of English MSOAs
sf::st_write(msoas_population_england, "data/msoas_population_england_2020_point_on_surface.geojson")

# Sums & prints total population of England (summed from all MSOAs)
sum(msoas_population_england$Population)

################################################################################
# Robin's code I don't quite understand (whats transport_authorities_atf4 ?)
sf::sf_use_s2(FALSE)
msoas_population_joined = sf::st_join(
  msoas_population_joined,
  transport_authorities_atf4 |> dplyr::select(Region_name))

transport_authorities_population = msoas_population_joined |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(Region_name) |> 
  dplyr::summarise(across(`All Ages`:`90+`, sum))
################################################################################

# Local Authority (LA) populations -----------------------------------------

# Groups MSOAs by LAs to obtain LA populations
LA_populations = msoas_population_joined |>
  dplyr::group_by(`LA name (2018 boundaries)`) |> 
  # For columns (All Ages --> 90+), sums MSOA pops to obtain LA pops
  dplyr::summarise(across(`All Ages`:`90+`, sum))

# Rename to more appropriate column name
LA_populations = LA_populations %>% 
  dplyr::rename(la_name = `LA name (2018 boundaries)`)

# Plots LAs
tmap::tm_shape(LA_populations) + 
  tmap::tm_dots(col = "la_name")

sf::st_write(LA_populations, "data/LA_population_stats_by_age_UK_NI_2022.geojson")

# Using LA --> Transport regions lookup sent to me by Robin the other day, can 
# we report on region_level stats too? 
# Load LAD --> TA Rgion Loookup

# Transport Authority (TA) Region level populations -----------------------

# Load LAD --> TA Region lookup table
lad_ta_region_lookup = read_csv("data-small/lad_ta_region_lookup_atf3.csv")

# Join TA Region Names to LADs so we can group by TA region
region_populations = left_join(lad_ta_region_lookup, LA_populations,
                               by = c("LAD22NM" = "la_name"))
# Group by TA Region name & sum LAD populations to obtain TA Region populations
region_populations = region_populations %>% 
  dplyr::group_by(Region_name) %>% 
  dplyr::summarise(across(`All Ages`:`90+`, sum))
sf::st_write(region_populations, "data/region_population_stats_by_age_UK_NI_2022.geojson")


NA_region_pops = region_populations %>% dplyr::filter(is.na(`All Ages`))
# A number of regions have NA

# Previous regions code ---------------------------------------------------


# # Read-in previously created regions:
# # regions_dft_2020 = sf::read_sf("data-small/regions_dft_2020.geojson")
# regions_dft_2020 = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/regions_dft_2020.geojson")
# tm_shape(regions_dft_2020) + tm_polygons("Level")
# # regions_atf = read.csv("data-small/atf-funds.csv")
# 
# # Read-in ATF funding allocations from: 
# # https://www.gov.uk/government/publications/emergency-active-travel-fund-local-transport-authority-allocations/emergency-active-travel-fund-total-indicative-allocations
# regions_atf = readr::read_csv("https://github.com/udsleeds/openinfra/raw/main/data-small/atf-funds.csv")
# 
# # Read in 2021 LAD data
# lads_uk_2021 = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/Local_Authority_Districts_(December_2021)_UK_BUC.geojson")
# 
# 
# summary(regions_dft_2020$Name %in% regions_atf$Name) # 26 match
# regions_atf = regions_atf %>% 
#   mutate(
#     name_short = str_trim(str_replace_all(Name, "CA|UA|JTC|ITA", ""))
#     ) 
# regions_atf$name_short
# summary(regions_dft_2020$Name %in% regions_atf$name_short) # 76 match
# setdiff(regions_dft_2020$Name, regions_atf$name_short)
# # [1] "Greater London"  "Cornwall"        "Isles of Scilly" # no ilses of scilly
# regions_atf = regions_atf %>% 
#   mutate(name_short = case_when(
#     str_detect(name_short, pattern = "London") ~ "Greater London",
#     str_detect(name_short, pattern = "Cornwall") ~ "Cornwall",
#     TRUE ~ name_short
#   )) %>% 
#   rename(Level_long = Level)
# 
# regions_dft_2020_without_cornwall_or_scilly = regions_dft_2020 %>% 
#   filter(!str_detect(Name, pattern = "Corn|Scill"))
# 
# cornwall = regions_dft_2020 %>% 
#   filter(str_detect(Name, pattern = "Corn"))
# cornwall$n = 2
# cornwall$LA_names = "Cornwall,Isles of Scilly"
# cornwall_geometry = regions_dft_2020 %>% 
#   filter(str_detect(Name, pattern = "Corn|Scill")) %>% 
#   sf::st_union()
# qtm(cornwall_geometry)
# cornwall$geometry = cornwall_geometry
# 
# regions_dft = rbind(regions_dft_2020_without_cornwall_or_scilly, cornwall)
# regions_dft = regions_dft %>% 
#   rename(name_short = Name)
# regions_dft = inner_join(regions_dft, regions_atf)
# 
# # Generate look-up from LADs to new regions
# lads_uk_2021_centroids = lads_uk_2021 %>% 
#   sf::st_centroid()
# lads_uk_2021_joined = st_join(lads_uk_2021_centroids, regions_dft)
# summary(lads_uk_2021_joined$LAD21NM == lads_uk_2021$LAD21NM) ## all true
# lads_uk_2021_joined$geometry_polygon = lads_uk_2021$geometry
# plot(lads_uk_2021)
# 
# # lads without a region
# lads_uk_2021_joined %>% 
#   filter(is.na(Name)) %>% 
#   qtm() +
#   qtm(regions_dft)
# # Some of the points just miss regions, buffer before join:
# # dist of 750 is minimum to capture all points in regions
# regions_dft_buffered = stplanr::geo_buffer(regions_dft, dist = 750)
# lads_uk_2021_joined = st_join(lads_uk_2021_centroids, regions_dft_buffered)
# summary(lads_uk_2021_joined$LAD21NM == lads_uk_2021$LAD21NM) ## all true
# lads_uk_2021_joined$geometry_polygon = lads_uk_2021$geometry
# lads_uk_2021_joined %>% 
#   filter(is.na(Name)) %>% 
#   qtm() +
#   qtm(regions_dft_buffered) +
#   tm_scale_bar()
# 
# # clean up
# lads_uk_2021_joined$geometry = lads_uk_2021$geometry
# lads_joined = lads_uk_2021_joined %>% 
#   select(LAD21NM, name_short:Level_long) %>% 
#   rename(Region_name = name_short, Region_name_long = Name)
# plot(lads_joined)
# qtm(lads_joined, "Region_name")
# file.remove("data-small/lads_joined_2021.geojson")
# sf::write_sf(lads_joined, "data-small/lads_joined_2021.geojson")
# summary(is.na(lads_joined$Region_name))
# 
# 
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
# # 
# # lads_to_regions_2021 = readr::read_csv("data-small/Local_Authority_District_to_Region_(April_2021)_Lookup_in_England.csv")
# # length(unique(lads_to_regions_2021$RGN21NM)) # 9
# # lads_uk_2021 = sf::read_sf("data-small/Local_Authority_Districts_(December_2021)_UK_BUC.geojson")
# # lads_in_lookup = lads_uk_2021$LAD21NM %in% lads_to_regions_2021$LAD21NM
# # summary(lads_in_lookup)
# # qtm(lads_uk_2021[lads_in_lookup, ]) # promising: all LADS in England
# 
# # Test code:
# # lads_to_regions_2020 = readRDS("~/cyipt/tempCycleways/lads.Rds")
# # lads_to_regions_2020 = readRDS("~/cyipt/tempCycleways/")
# # regions_dft_2020 = readRDS("~/cyipt/tempCycleways/regions_dft.Rds")
# # regions_dft_2020_long = regions_dft_2020 %>%
# #   sf::st_drop_geometry() %>% 
# #   separate(LA_names, into = letters[1:12], sep = ",")
# # nrow(regions_dft_2020) # 79
# # sf::write_sf(regions_dft_2020, "regions_dft_2020.geojson")
# # piggyback::pb_upload("regions_dft_2020.geojson")