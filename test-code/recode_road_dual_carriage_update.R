# Setup -------------------------------------------------------------------

remotes::install_github("udsleeds/openinfra")
# Library Imports
pkgs = c("sf",
         "osmextract",
         "tidyverse",
         "tmap",
         "openinfra")
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]


# Define current and updated functions ------------------------------------

recode_road_class = function(osm_sf) {
  # browser() Uncomment this to perform function debugging 
  
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
    dplyr::filter(!is.na(road_class))
}


oi_recode_roads = function(osm_sf){
  # browser() Uncomment this to perform function debugging 
  
  # Created road_class columns
  osm_recat = osm_sf %>%
    # Creates road_class column
    dplyr::mutate(oi_road_class = dplyr::case_when(
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
    dplyr::mutate(oi_road_desc = dplyr::case_when(
      # (7) - Motorways
      highway %in% c("motorway", "motorway_link") ~ "Motorways",
      # (6/5) - Dual Carriageways resi & non-resi
      (highway %in% c("trunk", "trunk_link")) & (oneway %in% c("yes", "-1", "reversible", "alternating")) ~ "Dual Carriageways (R&NR)",
      # (4) - Primary Roads
      highway %in% c("primary", "primary_link") | (highway %in% c("trunk", "trunk_link")) & (! oneway %in% c("yes", "-1", "reversible", "alternating")) ~ "Primary Roads",
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
    dplyr::filter(!is.na(oi_road_class))
}



# Get network data and apply recode_road func. ---------------------------------

network_data = sf::read_sf("https://github.com/udsleeds/openinfra/releases/download/v0.2/Leeds.geojson")

current_output = recode_road_class(network_data)
updated_output = oi_recode_roads(network_data)

# Assess the Dual Carriageway value counts
vc_dc_og = as.data.frame(table(current_output$road_desc))
vc_dc_new = as.data.frame(table(updated_output$oi_road_desc))

# Comapre the difference
waldo::compare(vc_dc_og, vc_dc_new)
# We can see that rows 2 & 4 have chaged where 2 - Dual Carriage Ways and 
# 4 - Primary (A) Roads --> thus, by changing our filtering we have moved 
# 343 ways incorrectly classified as dual carraige ways, back to being 
# Primary Roads (this was due to A roads being marked as trunk or trunk_link)
# and I was not specifying that a Dual Carriage way required a oneway key. 