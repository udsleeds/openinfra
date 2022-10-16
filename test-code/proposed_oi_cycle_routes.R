# Library set-up ----------------------------------------------------------

pkgs = c("sf",
         "osmextract",
         "tidyverse",
         "tmap",
         "openinfra"
)
# Load packages
lapply(pkgs, library, character.only = TRUE)[length(pkgs)]


# Parameters --------------------------------------------------------------

required_tags = c("foot", "bicycle", "access", "service", "maxspeed", "oneway",
                  "kerb", "footway", "sidewalk", "cycleway", "segregated",
                  "highway", "crossing", "lit", "tactile_paving", "surface",
                  "smoothness", "width", "est_width", "lit_by_led", "ref",
                  "amenity", "sidewalk", "sidewalk:left", "sidewalk:right",
                  "sidewalk:both", "source:maxspeed", "maxspeed:type",
                  "zone:maxspeed", "zone:traffic", "maxspeed", "HFCS", "rural",
                  "cycleway_left", "cycleway_right", "cycleway_both",
                  "separation", "lcn", "lcn_ref", "ncn", "ncn_ref", "type",
                  "route", "network", "cycle_network"
)

region_name = "England"

# Get data ----------------------------------------------------------------

options(timeout = 1000) # Un-comment if download fails due to timeout. 

england_osm = osmextract::oe_get(
  place = region_name,
  extra_tags = required_tags,
  max_file_size = 9e+09,
  layer = "lines"
) 

england_osm_lines = england_osm 

england_osm_line_rels = osmextract::oe_get(
  place = region_name,
  extra_tags = required_tags,
  max_file_size = 9e+09,
  layer = "multilinestrings"
)

england_osm_lines_other_rels = osmextract::oe_get(
  place = region_name,
  extra_tags = required_tags,
  max_file_size = 9e+09,
  layer = "other_relations"
)

# Combine "multilinestring" & "other_relations" layers
england_relations = rbind(england_osm_line_rels,
                  england_osm_lines_other_rels)

#ncn_eng_relations = england_relations %>% dplyr::filter(! is.na(ncn))

test = england_relations %>% dplyr::filter(osm_id ==9579)


oi_cycle_networks = function(osm_sf_ways, osm_sf_relations, cols_to_return, remove=FALSE){
  
  # First, find appropriate routes from relations layer
  message("osm relations: ", format(Sys.time(), "%a %b %d %X %Y"))
  osm_sf_rels_recat = osm_sf_relations %>% 
    
    # Find NCN and LCN routes from relation layer
    dplyr::mutate(openinfra_cycle_route = dplyr::case_when(
      #message("lcn relations")
      # lcn relation type must be a route, for a bicycle.
      (type == "route" & route == "bicycle") & 
        # relation must be related to lcns
        ((!is.na(lcn)) & (lcn != "no")) & 
        # Relation must be lcn network (or NA if not entered)
        (is.na(network) | network == "lcn")
      ~ paste("lcn:", lcn, lcn_ref, name, cycle_network),
      
      
      # ncn relation type must be a route for a bicycle
      (type == "route" & route == "bicycle") & 
        # relation must be related to ncns
        ((!is.na(ncn)) & (ncn != "no")) & 
        # Relation must be ncn network (or NA if not entered)
        (is.na(network) | network == "ncn")
      ~ paste("ncn:", ncn, ncn_ref, name, cycle_network)
    )) 
  
  message("osm ways: ", format(Sys.time(), "%a %b %d %X %Y"))
  # Second, find appropriate routes from ways layer
  osm_sf_ways_recat = osm_sf_ways %>%
    
    # Find NCN and LCN routes from ways layer
    dplyr::mutate(openinfra_cycle_route = dplyr::case_when(
      #message("lcn ways")
      # ways a part of lcn routes must be tagged lcn=*
      (! is.na(lcn) &  lcn != "no") ~ paste("lcn:", lcn, lcn_ref),
      #message("ncn ways")
      # ways a part of ncn routes must be tagged ncn=*
      (! is.na(ncn) & ncn != "no") ~ paste("ncn:", ncn, ncn_ref)
    ))
  
  message("rbinding: ", format(Sys.time(), "%a %b %d %X %Y"))
  # Now, select common columns to be returned, rbind the data frames and return the network.
  combined_osm_sf = rbind(osm_sf_rels_recat %>% dplyr::select(all_of(cols_to_return)),
                          osm_sf_ways_recat %>% dplyr::select(all_of(cols_to_return)))
  message("removing NAs: ", format(Sys.time(), "%a %b %d %X %Y"))
  # If remove = TRUE, remove rows with NA openinfra_cycle_route values
  
  message(names(combined_osm_sf))
  
  if (remove){
    combined_osm_sf = combined_osm_sf %>% 
      dplyr::filter(! is.na(openinfra_cycle_route))
  }
  
  return(combined_osm_sf)
}

oi_cycle_networks_update = function(osm_sf_ways, osm_sf_relations, cols_to_return, remove=FALSE){
  
  # First, find appropriate routes from relations layer
  message("osm relations: ", format(Sys.time(), "%a %b %d %X %Y"))
  osm_sf_rels_recat = osm_sf_relations %>% 
    
    # Find NCN and LCN routes from relation layer
    dplyr::mutate(openinfra_cycle_route = dplyr::case_when(
      #message("lcn relations")
      # lcn relation type must be a route, for a bicycle.
      (type == "route" & route == "bicycle") & 
        # relation must be related to lcns
        (((!is.na(lcn)) & (lcn != "no")) | (network == "lcn")) #& 
        # Relation must be lcn network (or NA if not entered)
        #(is.na(network) | network == "lcn")
      ~ paste("lcn:", lcn, lcn_ref, ref, name, cycle_network),
      
      
      # ncn relation type must be a route for a bicycle
      (type == "route" & route == "bicycle") & 
        # relation must be related to ncns
        (((!is.na(ncn)) & (ncn != "no")) | (network == "ncn"))
        # Relation must be ncn network (or NA if not entered)
        #(is.na(network) | network == "ncn")
      ~ paste("ncn:", ncn, ncn_ref, ref, name, cycle_network)
    )) 
  
  message("osm ways: ", format(Sys.time(), "%a %b %d %X %Y"))
  # Second, find appropriate routes from ways layer
  osm_sf_ways_recat = osm_sf_ways %>%
    
    # Find NCN and LCN routes from ways layer
    dplyr::mutate(openinfra_cycle_route = dplyr::case_when(
      #message("lcn ways")
      # ways a part of lcn routes must be tagged lcn=*
      (! is.na(lcn) &  lcn != "no") ~ paste("lcn:", lcn, lcn_ref),
      #message("ncn ways")
      # ways a part of ncn routes must be tagged ncn=*
      (! is.na(ncn) & ncn != "no") ~ paste("ncn:", ncn, ncn_ref)
    ))
  
  message("rbinding: ", format(Sys.time(), "%a %b %d %X %Y"))
  # Now, select common columns to be returned, rbind the data frames and return the network.
  combined_osm_sf = rbind(osm_sf_rels_recat %>% dplyr::select(all_of(cols_to_return)),
                          osm_sf_ways_recat %>% dplyr::select(all_of(cols_to_return)))
  message("removing NAs: ", format(Sys.time(), "%a %b %d %X %Y"))
  # If remove = TRUE, remove rows with NA openinfra_cycle_route values
  
  message(names(combined_osm_sf))
  
  if (remove){
    combined_osm_sf = combined_osm_sf %>% 
      dplyr::filter(! is.na(openinfra_cycle_route))
  }
  
  return(combined_osm_sf)
}

cols_to_return = c("osm_id", "highway", "name", "lcn", "ncn", "lcn_ref", "ncn_ref", "geometry", "openinfra_cycle_route", "network")

output = oi_cycle_networks_update(england_osm_lines, england_relations, cols_to_return)

output_noNA = output %>% dplyr::filter(! is.na(openinfra_cycle_route))

ncn_output = output_noNA %>% dplyr::filter( ncn %in% c("proposed", "yes") | network == "ncn")
lcn_output = output_noNA %>% dplyr::filter( lcn %in% c("proposed", "yes") | network == "lcn")
neither = output_noNA %>% dplyr::filter(is.na(ncn) & is.na(lcn))

tmap::tmap_mode("view")
#tmap::qtm(lcn_output)
tmap::qtm(ncn_output)
#tmap::qtm(output_noNA)


# # debug function with test ------------------------------------------------
# 
# test = function(osm_sf_ways, osm_sf_relations, remove=FALSE){
#   #browser()
#   # First, find appropriate routes from relations layer
#   osm_sf_rels_recat = osm_sf_relations %>% 
#     
#     # Find NCN and LCN routes from relation layer
#     dplyr::mutate(openinfra_cycle_route = dplyr::case_when(
#       #message("lcn relations")
#       # lcn relation type must be a route, for a bicycle.
#       (type == "route" & route == "bicycle") & 
#         # relation must be related to lcns
#         ((!is.na(lcn)) & (lcn != "no")) & 
#         # Relation must be lcn network (or NA if not entered)
#         (is.na(network) | network == "lcn")
#       ~ paste("lcn:", lcn, lcn_ref, name, cycle_network),
#       
#       #message("ncn relations")
#       # ncn relation type must be a route for a bicycle
#       (type == "route" & route == "bicycle") & 
#         # relation must be related to ncns
#         ((!is.na(ncn)) & (ncn != "no")) & 
#         # Relation must be ncn network (or NA if not entered)
#         (is.na(network) | network == "ncn")
#       ~ paste("ncn:", ncn, ncn_ref, name, cycle_network)
#     ))
#   
#   return(osm_sf_rels_recat)
#   
# }
# 
# t_out = test(england_osm_lines, england_osm_line_rels)
# t_out_noNA = t_out %>% dplyr::filter(! is.na(openinfra_cycle_route))
# 
# 
# 
# 
# # debug ways part ---------------------------------------------------------
# 
# test_ways = function(osm_sf_ways, osm_sf_relations, remove=FALSE){
#   osm_sf_ways_recat = osm_sf_ways %>%
#     
#     # Find NCN and LCN routes from ways layer
#     dplyr::mutate(openinfra_cycle_route = dplyr::case_when(
#       #message("lcn ways")
#       # ways a part of lcn routes must be tagged lcn=*
#       (! is.na(lcn) &  lcn != "no") ~ paste("lcn:", lcn, lcn_ref),
#       #message("ncn ways")
#       # ways a part of ncn routes must be tagged ncn=*
#       (! is.na(ncn) & ncn != "no") ~ paste("ncn:", ncn, ncn_ref)
#     ))
#   
#   return(osm_sf_ways_recat)
# }
# 
# t_out_way = test_ways(england_osm_lines, england_osm_line_rels, cols_to_return)
