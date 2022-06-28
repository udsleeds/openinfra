# Date      27/06/2022
# Authour   James Hulse
# Version   1.0.0
#

# Define recode_road_class function
recode_road_class <- function(osm_sf) {
  # Created road_class columns
  osm_recat = osm_sf %>%
    # Creates road_class column
    dplyr::mutate(road_class = dplyr::case_when(
      # (7) - Motorways
      highway %in% c("motorway", "motorway_link") ~ "7",
      # (6/5) - Dual Carriageways resi & non-resi
      highway %in% c("trunk", "trunk_link") ~ "6/5",
      # (4) - Primary Roads
      highway %in% c("primary", "primary_link") | (highway == "trunk" & oneway == "F") ~ "4",
      # (3) - Secondary Roads
      highway %in% c("secondary", "secondary_link") ~ "3",
      # (2) - Tertiary Roads
      highway %in% c("tertiary", "tertiary_link") ~ "2",
      # (1) - Local Roads
      highway %in% c("living_street", "residential", "unclassified") ~ "1",
      # (0) - Traffic-free Paths
      highway == "cycleway" ~ "0"
    )) %>%
    
    # Creates road_description columns
    dplyr::mutate(road_desc = dplyr::case_when(
      # (7) - Motorways
      highway %in% c("motorway", "motorway_link") ~ "Motorways",
      # (6/5) - Dual Carriageways resi & non-resi
      highway %in% c("trunk", "trunk_link") ~ "Dual Carriageways (R&NR)",
      # (4) - Primary Roads
      highway %in% c("primary", "primary_link") | (highway == "trunk" & oneway == "F") ~ "Primary Roads",
      # (3) - Secondary Roads
      highway %in% c("secondary", "secondary_link") ~ "Secondary Roads",
      # (2) - Tertiary Roads
      highway %in% c("tertiary", "tertiary_link") ~ "Tertiary Roads",
      # (1) - Local Roads
      highway %in% c("living_street", "residential", "unclassified") ~ "Residential / Local Roads",
      # (0) - Traffic-free Paths
      highway == "cycleway" ~ "Cycleway"
    )) %>%
    
    # Removes features that have not been recodeed to a road_class value
    filter(!is.na(road_class))
}