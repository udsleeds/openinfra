# Script for test-code on LTN1/20 compliance - will investigate the physical 
# requirements (width, road separation etc.) of cycleways to see whether or not 
# they comply with LTN1/20 guide. Of particular use is the LTN1/20 guidance: 
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/951074/cycle-infrastructure-design-ltn-1-20.pdf
# specifically, chapters 4 & 5.


# Set-up ------------------------------------------------------------------

# Library Imports
pkgs = c("sf",
         "osmextract",
         "tidyverse",
         "tmap")
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]

required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                  "kerb", "footway", "sidewalk", "cycleway", "segregated",
                  "highway", "crossing", "lit", "tactile_paving", "surface",
                  "smoothness", "width", "est_width", "lit_by_led", "boundary",
                  "admin_level", "name", "ref", "cycleway:left",
                  "cycleway:right")

# Get data ----------------------------------------------------------------

LADs = sf::read_sf("https://github.com/udsleeds/openinfra/raw/main/data-small/lads_joined_2021.geojson")
leeds_lad_poly = LADs %>% 
  dplyr::filter(LADs$LAD21NM == "Leeds") %>% 
  dplyr::select("geometry")

# Request data specify leeds_lad_poly as palce
leeds_first_network = osmextract::oe_get(
  place = "Leeds",
  layer = "lines",
  boundary_type = "clipsrc",
  extra_tags = required_tags
)

# Re sample the network to contain ONLY the network covered by leeds_lad_poly
leeds_network = leeds_first_network[leeds_lad_poly, ]

leeds_osmex_cycling = osmextract::oe_get_network(
  mode = "cycling",
  place = "Leeds",
  provider = "bbbike",
  extra_tags = required_tags,
  force_vectortranslate = TRUE
)

leeds_osmex_cycling = leeds_osmex_cycling[leeds_lad_poly, ]


# Tag Distribution Analysis ----------------------------------------------------------------
cycleway_values = as.data.frame(table(leeds_network$cycleway))

no_NA_cycleway_lanes_leeds = leeds_network %>% dplyr::filter(! is.na(cycleway))

width_vc = as.data.frame(table(no_NA_cycleway_lanes_leeds$width))
no_NA_width_cycleway_leeds = no_NA_cycleway_lanes_leeds %>% dplyr::filter(! is.na(width))


# oi_classify_cycle_ways --------------------------------------------------

################################################################################
# Create a function that can classify a way used for cycling as being either:  #
#     - Fully Kerbed Cycle Track                                               #
#     - Stepped Cycle Track/Lance                                              #
#     - Lightly Segregated Cycle Track/Lane                                    #
#     - Cycle Lane (Mandatory & Advisory)                                      #
#     - Mixed Traffic (Motor (& maybe pedestrian?) )                           #
#                                                                              #
# Way classification will be determined by assessing the tags assigned to each #
# way and comparing those against definitions proposed by cyclestreets.net     #
# (https://bikedata.cyclestreets.net/tflcid/conversion/#cycle_lane_track) for  #
# converting London CID to OSM.                                                #
################################################################################
# Due to issues identifying levels of segregation through OSM tags, due to a   #
# lack of cycle infrastructure tagging consistency [example]. As such we will  #
# catagorise infrastructure as either:                                         #
#     - Segregated (Fully Kerbed/Stepped/Light)                                #
#     - Cycle Lanes (Mandatory/Advisory) <-- On Carriage Ways                  #
#     - Mixed Traffic (No designated cycling route, but cyclists are lgeally   #
#                      allowed to travel on most UK roads - See below url for  #
#                      more info.https://tinyurl.com/OSMaccess)                #
#                                                                              #
#                                                                              #
################################################################################



# Mixed Traffic -----------------------------------------------------------
# any road with a maxspeed <= 20 mph is suitable - find this with 
# oi_clean_maxspeed

leeds_mixed_traffic = leeds_osmex_cycling %>% dplyr::filter(
  cycleway %in% c("no", "none", "opposite")
)



# Cycle lanes  ------------------------------------------------------------

leeds_cycle_lanes = leeds_osmex_cycling %>%
  dplyr::mutate(oi_cycle_lane = dplyr::case_when(
    # Capture Obvious cycle lanes
    cycleway %in% c("lane") ~ "yes",
    # Capture more obscure cycle lanes
    cycleway_left %in% c("lane") ~ "yes",
    cycleway_right %in% c("lane") ~ "yes",
    cycleway_both %in% c("lane") ~ "yes"
  ))


# Protected cycling -------------------------------------------------------

leeds_segregated = leeds_osmex_cycling %>% 
  dplyr::mutate(oi_cycle_seg = dplyr::case_when(
    # Captures obvious track lanes - separated by definition
    cycleway %in% c("track") ~ "yes",
    # Captures more obscure track lanes 
    cycleway_left %in% c("track") ~ "yes",
    cycleway_right %in% c("track") ~ "yes",
    cycleway_both %in% c("track") ~ "yes"
    
    # Segregated tag indicates if a cycleway/footpath is shared with pedestrians
    # by default this cannot be a cycleway on the road if pedestrians are okay
    # and segregation is okay. 
  ))


leeds_cycle_lanes = leeds_cycle_lanes %>% dplyr::filter(! is.na(oi_cycle_lane))
leeds_segregated = leeds_segregated %>% dplyr::filter(! is.na(oi_cycle_seg))
# Visualise the data ------------------------------------------------------

tmap::tmap_mode("view")

visualise = tmap::tm_shape(leeds_cycle_lanes) + 
    tmap::tm_lines(col = "oi_cycle_lane", palette = "red") + 
  
  tmap::tm_shape(leeds_mixed_traffic) + 
    tmap::tm_lines(col = "highway") + 
  
  tmap::tm_shape(leeds_segregated) + 
    tmap::tm_lines(col = "oi_cycle_seg", palette = "green")

visualise
