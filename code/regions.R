# Aim: create maps showing UK transport regions as of 2022-09

library(tmap)
library(tidyverse)
tmap_mode("view")
sf::sf_use_s2(FALSE)


# Get and process LADs ------------------------------------------------------

# https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-may-2022-uk-bsc/explore?location=55.223511%2C-3.317025%2C6.92

lads = sf::read_sf("data-small/Local_Authority_Districts_(May_2022)_UK_BSC.geojson")
lad_ta_region_lookup = read_csv("data-small/lad_ta_region_lookup_atf3.csv")

#### Group all LADs by country
lads_2022_england = lads %>% 
  filter(str_detect(string = LAD22CD, pattern = "E")) %>% 
  select(LAD22CD, LAD22NM)

lads_2022_scotland = lads %>% 
  filter(str_detect(string = LAD22CD, pattern = "S")) %>% 
  select(LAD22CD, LAD22NM)
plot(lads_2022_scotland$geometry)

lads_2022_wales = lads %>% 
  filter(str_detect(string = LAD22CD, pattern = "W")) %>% 
  select(LAD22CD, LAD22NM)

#### Group and count # LADs per country
countries = lads  %>%  
  mutate(
    Country = case_when(
      str_detect(string = LAD22CD, pattern = "S") ~ "Scotland",
      str_detect(string = LAD22CD, pattern = "W") ~ "Wales",
      str_detect(string = LAD22CD, pattern = "N") ~ "Northern Ireland",
      str_detect(string = LAD22CD, pattern = "E") ~ "England"
    )
  ) %>% 
  group_by(Country) %>% 
  summarise(n = n(), local_authority_names = paste0(LAD22NM, collapse = ", "))

plot(countries)
sf::write_sf(countries, "data-small/countries.geojson")

# Create lookup for LAD --> Region Name
lads_england_lookup = left_join(lads_2022_england, lad_ta_region_lookup)

# Group LADs by Region Name (group LADs by the region they reside in)
transport_regions_2022 = lads_england_lookup %>% 
  group_by(Name = Region_name) %>% 
  summarise(n_lads = n(), lad_names = paste(LAD22NM, collapse = ","))

sf::write_sf(transport_regions_2022, 
             "data-small/transport_regions_2022.geojson",
             append = FALSE)

# Get and process MSOA data (Same as above for LADs but MSOAs here) -------

#MSOA Middle Layer Super Output Area code here
msoas = sf::read_sf('/home/james/Desktop/LIDA_OSM_Project/openinfra/openinfraresults/data/Middle_layer_Super_Output_Areas_(December_2021)_Boundaries_Full_Clipped_EW_(BFC).geojson')

#### Group MSOAs by Country
msoas_2021_england = msoas %>% 
  filter(str_detect(string = MSOA21CD, pattern = "E")) %>% 
  select(MSOA21CD, MSOA21NM)

msoas_2021_scotland = msoas %>% 
  filter(str_detect(string = MSOA21CD, pattern = "S")) %>% 
  select(MSOA21CD, MSOA21NM)
#plot(msoas_2021_scotland$geometry)

msoas_2021_wales = msoas %>% 
  filter(str_detect(string = MSOA21CD, pattern = "W")) %>% 
  select(MSOA21CD, MSOA21NM)
#plot(msoas_2021_wales$geometry)

# Groups MSOAs stats by country they reside in. 
countries = msoas %>% 
  mutate(
    Country = case_when(
      str_detect(string = MSOA21CD, pattern = "S") ~ "Scotland",
      str_detect(string = MSOA21CD, pattern = "W") ~ "Wales",
      str_detect(string = MSOA21CD, pattern = "N") ~ "Northern Ireland",
      str_detect(string = MSOA21CD, pattern = "E") ~ "England"
    )
  ) %>% 
  group_by(Country) %>% 
  summarise(n = n(), middle_super_ouput_names = paste0(MSOA21NM, collapse = ", "))

sf::write_sf(transport_regions_2022,
             "data-small/transport_regions_msoas_2022.geojson",
             append = FALSE)

# Add population data at MSOA level, published 2020 ----------------------

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates

# Download population data from ONS published 2020
u = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates/mid2020sape23dt4/sape23dt4mid2020msoasyoaestimatesunformatted.xlsx"
f = basename(u)
download.file(u, f)

# Read population data from downloaded excel file
msoas_population_meta = readxl::read_excel(f)
msoas_population = readxl::read_excel(f, sheet = 4, skip = 3)


# Get all 2011 msoas boundaries
msoas = ukboundaries::msoa2011_vsimple %>% 
  sf::st_point_on_surface()

# Check each region in msoas_pop is in msoas
summary(msoas_population$`MSOA Code` %in% msoas$msoa11cd)

# Set 2nd column of msoas as 1st column of msoas_population [MSOA Code]
names(msoas)[2] = names(msoas_population)[1]

# Joins msoas columns (7) onto msoas_population (98) to total (104) using
# MSOA Code as a join identifier
msoas_population_joined = dplyr::left_join(msoas, msoas_population)

# Gets all English MSOAs (MSOA Codes begining "E") and returns selected columns
msoas_population_england = msoas_population_joined %>% 
  dplyr::filter(stringr::str_detect(string = `MSOA Code`, pattern = "E")) %>% 
  dplyr::select(msoa11cd = `MSOA Code`, msoa11nm, Population = `All Ages`, `LA name (2021 boundaries)`)

# Plots the point geometry of English MSOAs as points
plot(msoas_population_england$geometry)

# Save dataset of populations of English MSOAs
sf::st_write(msoas_population_england, 
             "data/msoas_population_england_2020_point_on_surface.geojson",
             append = FALSE)

# Sums & prints total population of England (summed from all MSOAs)
sum(msoas_population_england$Population)

# # Get total population by LAD name from grouping MSOAs
# msoas_grouped_pops_by_lad = msoas_population_joined %>% 
#   dplyr::group_by(`LA name (2018 boundaries)`) %>% 
#   summarise(total_pop = sum(`All Ages`))


# Get population stats for each English LAD -------------------------------
sf::sf_use_s2(FALSE)

# Adds TA Region Names to each MSOA entry (already has pop figures) 
msoas_population_joined = sf::st_join(
  msoas_population_joined,
  transport_regions_2022 %>% dplyr::select(transport_authority_name = Name)
  )

# Add each LAD for each MSAO entry (Now had MSOA, LAD, Region...)
msoas_population_joined = sf::st_join(
  msoas_population_joined,
  lads_2022_england %>% dplyr::select(LAD22NM)
)

# Get population figures by LAD
transport_authorities_population = msoas_population_joined %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(LAD22NM) %>% 
  dplyr::summarise(across(`All Ages`:`90+`, sum))

# Joins LAD geometries and populations for local authorities
lads_2022_england_joined = left_join(lads_2022_england, 
                                     transport_authorities_population)

# Join back onto geo representations
lads_2022_england_joined %>% 
  select(`All Ages`) %>% 
  plot()


#atf4_regions_from_robin = sf::read_sf("data-small/transport_regions_2022.geojson")
atf4_regions_from_robin = trasnport_reg

# Plot of number of local authorities per English region.
atf4_regions_from_robin %>% 
  select(n_lads) %>% 
  plot()

# TODO: review deleting of code below this line. 

# Local Authority (LA) populations -----------------------------------------

# Groups MSOAs by LAs to obtain LA populations
LA_populations = msoas_population_joined %>%
  dplyr::group_by(`LA name (2018 boundaries)`) %>% 
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
