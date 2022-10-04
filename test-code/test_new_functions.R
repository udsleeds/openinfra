# Function to recategorise OSM data based on presnece of sidewalk. 
#

# Tags required for functions wihtin this script.
required_tags = c("sidewalk", "sidewalk:left", "sidewalk:right", 
                  "sidewalk:both", "source:maxspeed", "maxspeed:type", 
                  "zone:maxspeed", "zone:traffic", "maxspeed", "HFCS", "rural")

simple_tags = c("maxspeed", "rural", "HFCS")

# Assess when sidewalks are present: 
oi_has_sidewalk = function(osm_sf, remove=FALSE){
  osm_sf_recat = osm_sf %>% dplyr::mutate(
    openinfra_sidewalk = dplyr::case_when(
      # Catch presence of sidewalks
      sidewalk %in% c("yes", "both", "left", "right", "separate") ~ "yes",
      sidewalk_left %in% c("yes", "separate") ~ "yes",
      sidewalk_right %in% c("yes", "separate") ~ "yes",
      sideqwalk_both %in% c("yes", "separate") ~ "yes",
      
      # Catch lack of sidewalks
      sidewalk %in% c("no", "none") ~ "no",
      (sidewalk_left %in% c("no", "none")) &
        (sidewalk_right %in% c("none", "no")) ~ "no",
      sidewalk_both %in% c("no", "none") ~ "no"
    ))
  
  if (remove){
    osm_sf_recat = osm_sf_recat %>% dplyr::filter(! is.na(openinfra_sidewalk))
  }
  
  return(osm_sf_recat)
}

################################################################################

oi_road_type = function(osm_sf, remove=FALSE){
  osm_sf_recat = osm_sf %>% dplyr::mutate(
    openinfra_road_type = dplyr::case_when(
      # Catch urban ways
      stringr::str_detect(source_maxspeed, "urban") |
        stringr::str_detect(maxspeed_type, "urban") | 
        stringr::str_detect(zone_maxspeed, "urban") |
        stringr::str_detect(zone_traffic, "urban") |
        stringr::str_detect(maxspeed, "urban") | 
        stringr::str_detect(HFCS, "urban") | 
        rural == "no" ~ "urban",
      
      # Catch rural ways
      stringr::str_detect(source_maxspeed, "rural") |
        stringr::str_detect(maxspeed_type, "rural|nsl_signal|nsl_dual") |
        stringr::str_detect(zone_maxspeed, "rural") |
        stringr::str_detect(zone_traffic, "rural") |
        stringr::str_detect(maxspeed, "rural") | 
        stringr::str_detect(HFCS, "rural") | 
        rural == "yes" ~ "rural"
      
    ))
  
  if (remove){
    osm_sf_recat = osm_sf_recat %>% dplyr::filter(! is.na(openinfra_road_type))
  }
  
  return(osm_sf_recat)
}

###############################################################################
# Sources: https://github.com/streetcomplete/StreetComplete/issues/492

#TODO: fix oi_infer_maxspeed
oi_infer_maxspeed = function(osm_sf, remove=FALSE){
  
  osm_sf_recat = osm_sf %>% 
    dplyr::mutate(openinfra_inferred_maxspeed = dplyr::case_when(
      
      # NOTE: oi_clean_maxspeed must be applied to the `osm_sf` BEFORE using 
      # the infer_maxspeed function as this makes use of openinfra_maxspeed col. 
      # If maxspeed is present, use this value.
      #(! is.na(openinfra_maxspeed)) ~ openinfra_maxspeed,
      
      
      ### Case of restricted road (maxspeed = 30 mph)
      # Way must be lit & have a missing maxspeed to infer...  
      ( (! lit %in% c("no", "disued")) & (is.na(maxspeed)) & (! is.na(highway)) &
        
        # One of these conditions must be met for a restricted road (hence 30 mph)  
        (maxspeed_type %in% c("GB:nsl_restricted")) | 
        (highway %in% c("residential", "living_street")) #TODO: Remove "living_street" due to issue convo with Martin 
      ) ~ "30 mph", 
    
      ### Case of motorways
      # Way must be lit & have a missing maxspeed tag to infer...
      ( is.na(maxspeed) & (! is.na(highway)) & 
        
        # One of these conditions must be met to be a motorway
         (highway %in% c("motorway", "motorway_link")) |
         (maxspeed_type %in% c("GB:motorway", "UK:motorway", "uk:motorway")) |
         (stringr::str_detect(maxspeed_type, "nsl_dual"))  
      ) ~ "70 mph",
      
      
      ### Case of single lane carriageway
      # Way must have a missing maxspeed tag to infer...
      ( is.na(maxspeed) & (! is.na(highway)) &
        
        # One of these conditions must be met to be a single lane
        (stringr::str_detect(maxspeed_type, "nsl_single"))
      ) ~ "60 mph",
      
      ### Case of dual lane carriageway
      # Way must have missing maxspeed tag to infer...
      is.na(maxspeed) & (! is.na(highway)) &
        
        # One of these conditions must be met to be dual carriageway
        stringr::str_detect(maxspeed_type, "nsl_dual") |
        highway %in% c("motorway", "motorway_link")
    ))
  
}


# Test the functions ------------------------------------------------------

osm_sf = osmextract::oe_get(
  place = "Leeds",
  layer = "lines",
  extra_tags = required_tags,
  force_vectortranslate = TRUE,
  never_skip_vectortranslate = TRUE
)

leeds_centre_point = sf::st_sfc(sf::st_point(c(-1.549, 53.801)), 
                                crs = "EPSG:4326")
leeds_buffer = sf::st_buffer(leeds_centre_point, dist = 4000) #2000 metre radius

osm_sf_buffed = osm_sf[leeds_buffer, ]

# test oi_infer_maxspeed --------------------------------------------------

infer_maxspeed_output = oi_infer_maxspeed(osm_sf_buffed)

# Compare oi_infer_maxspeed to normal oi_clean_maxspeed function. 
normal_test_output = oi_clean_maxspeed_uk(normal_test)
infer_test_output = oi_infer_maxspeed(normal_test_output)

normal_big_output = oi_clean_maxspeed_uk(normal_bug)
infer_big_output = oi_infer_maxspeed(normal_big_output)

# test oi_road_type -------------------------------------------------------

road_type_output = oi_road_type(osm_sf)  
test_vc = as.data.frame(table(road_type_output$openinfra_road_type))

view_output = road_type_output %>% 
  dplyr::filter(openinfra_road_type %in% c("rural", "urban"))

################################################################################
# Here we test the identifying motorways through implied tags. The implied tags#
# are sourced from: https://wiki.openstreetmap.org/wiki/Tag:highway%3Dmotorway #
# NOTE: that it seems these implied tags mean that, if a motorway is found then#
# these tags will apply to the infrastructure, rather than these tags being    #
# indicative of a motorway. (see osm_id=27713485) for exmaple.                 #
################################################################################
leeds_motor = leeds %>% dplyr::mutate(foot = "no")                             #
leeds_motor = leeds_motor %>% dplyr::mutate(surface = "paved")                 #
leeds_motor = leeds_motor %>% dplyr::mutate(motor_vehicle = "yes")             #
                                                                               #
motorway_imp = leeds_motor %>% dplyr::filter(                                  #
  foot == "no",                                                                #
  bicycle == "no",                                                             #
  surface == "paved",                                                          #
  motor_vehicle == "yes"                                                       #
)                                                                              #
                                                                               #
################################################################################
