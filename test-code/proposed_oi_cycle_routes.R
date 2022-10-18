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


# Define test functions.  -------------------------------------------------

oi_cycle_networks = function(osm_ways, osm_relations, return_cols, remove=FALSE){
  
  # First, find appropriate routes from relations layer
  message("osm relations: ", format(Sys.time(), "%a %b %d %X %Y"))
  osm_rels_recat = osm_relations %>% 
    
    # Find NCN and LCN routes from relation layer
    dplyr::mutate(openinfra_cycle_routes = dplyr::case_when(
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
  osm_ways_recat = osm_ways %>%
    
    # Find NCN and LCN routes from ways layer
    dplyr::mutate(openinfra_cycle_routes = dplyr::case_when(
      #message("lcn ways")
      # ways a part of lcn routes must be tagged lcn=*
      (! is.na(lcn) &  lcn != "no") ~ paste("lcn:", lcn, lcn_ref),
      #message("ncn ways")
      # ways a part of ncn routes must be tagged ncn=*
      (! is.na(ncn) & ncn != "no") ~ paste("ncn:", ncn, ncn_ref)
    ))
  
  message("rbinding: ", format(Sys.time(), "%a %b %d %X %Y"))
  # Now, select common columns to be returned, rbind the data frames and return the network.
  combined_osm_sf = rbind(osm_rels_recat %>% dplyr::select(all_of(return_cols)),
                          osm_ways_recat %>% dplyr::select(all_of(return_cols)))
  message("removing NAs: ", format(Sys.time(), "%a %b %d %X %Y"))
  # If remove = TRUE, remove rows with NA openinfra_cycle_route values
  
  message(names(combined_osm_sf))
  
  if (remove){
    combined_osm_sf = combined_osm_sf %>% 
      dplyr::filter(! is.na(openinfra_cycle_routes))
  }
  
  return(combined_osm_sf)
}
oi_cycle_networks_update = function(osm_ways, osm_relations, return_cols, remove=FALSE){
  
  # First, find appropriate routes from relations layer
  message("osm relations: ", format(Sys.time(), "%a %b %d %X %Y"))
  osm_relations_recat = osm_relations %>% 
    
    # Find NCN and LCN routes from relation layer
    dplyr::mutate(openinfra_cycle_routes = dplyr::case_when(
      
      # lcn relation type must be a route, for a bicycle.
      (type %in% c("route", "superroute") & route == "bicycle") & 
        # relation must be related to lcns
        (((!is.na(lcn)) & (lcn != "no")) | (network == "lcn")) 
        
      ~ paste(na.omit(c("lcn:", lcn, lcn_ref, ref, name, cycle_network))),
      
      
      # ncn relation type must be a route for a bicycle
      (type %in% c("route", "superroute") & route == "bicycle") & 
        # relation must be related to ncns
        (((!is.na(ncn)) & (ncn != "no")) | (network == "ncn"))

      ~ paste(na.omit(c("ncn:", ncn, ncn_ref, ref, name, cycle_network)))
    )) 
  
  # Second, find appropriate routes from ways layer
  message("osm ways: ", format(Sys.time(), "%a %b %d %X %Y"))
  osm_ways_recat = osm_ways %>%
    
    # Find NCN and LCN routes from ways layer
    dplyr::mutate(openinfra_cycle_routes = dplyr::case_when(
     
      # ways a part of lcn routes must be tagged lcn=*
      (!is.na(lcn) & lcn!="no") | network=="lcn" 
      ~ paste(na.omit(c("lcn:", lcn, lcn_ref, ref))),
      
      # ways a part of ncn routes must be tagged ncn=*
      (!is.na(ncn) & ncn!="no") | network=="ncn"  
      ~ paste(na.omit(c("ncn:", ncn, ncn_ref)))
    ))
  
  # Select columns to be returned, bind ways & relations, return joined network.
  message("rbinding: ", format(Sys.time(), "%a %b %d %X %Y"))
  combined_osm_sf = rbind(osm_relations_recat %>% dplyr::select(all_of(return_cols)),
                          osm_ways_recat %>% dplyr::select(all_of(return_cols)))
  
  # If remove = TRUE, remove rows with NA openinfra_cycle_route values
  if (remove){
    message("Removing NAs: ", format(Sys.time(), "%a %b %d %X %Y"))
    combined_osm_sf = combined_osm_sf %>% 
      dplyr::filter(! is.na(openinfra_cycle_route))
  }
  
  return(combined_osm_sf)
}


# Apply functions ---------------------------------------------------------

return_cols = c("osm_id", "highway", "name", "lcn", "ncn", "lcn_ref", "ncn_ref",
                "geometry", "openinfra_cycle_route", "network")

output = oi_cycle_networks_update(england_osm_lines, england_relations, return_cols)

output_noNA = output %>% dplyr::filter(! is.na(openinfra_cycle_route))


# Analysis ----------------------------------------------------------------

ncn_output = output_noNA %>% dplyr::filter( ncn %in% c("proposed", "yes") | network == "ncn")
lcn_output = output_noNA %>% dplyr::filter( lcn %in% c("proposed", "yes") | network == "lcn")
neither = output_noNA %>% dplyr::filter(is.na(ncn) & is.na(lcn))

# Try reproduce Robin's plot of ncn's & ncn's
ncn_from_relations = england_relations %>% dplyr::filter(network == "ncn")
tmap::qtm(ncn_from_relations)

tmap::tmap_mode("view")
#tmap::qtm(lcn_output)
tmap::qtm(ncn_output)
#tmap::qtm(output_noNA)


# Check network=="ncn" only. ----------------------------------------------

# read data from england-latest.osm.pbf

eng_fp = "/home/james/Desktop/LIDA_OSM_Project/openinfra/eng_osm_downloads/2022_10_10/geofabrik_england-latest.osm.pbf"

eng_lines = osmextract::oe_read(
  file_path = eng_fp,
  extra_tags = required_tags,
  layer = "lines"
)
eng_multilines = osmextract::oe_read(
  file_path = eng_fp,
  extra_tags = required_tags,
  layer = "multilinestrings"
)
eng_relations = osmextract::oe_read(
  file_path = eng_fp,
  extra_tags = required_tags,
  layer = "other_relations"
)

eng_local_relations = rbind(eng_multilines,
                            eng_relations)

ncn_local_relations = eng_local_relations %>% dplyr::filter(network == "ncn")

legacy_ncn_relation_curr = eng_local_relations %>% 
  dplyr::filter(type == "route" & route == "bicycle") %>% 
  dplyr::filter(((!is.na(ncn)) & (ncn != "no")) | (network == "ncn"))

legacy_ncn_relations = eng_local_relations %>% dplyr::filter(network == "ncn")

diff = dplyr::anti_join(legacy_ncn_relation_curr %>% sf::st_drop_geometry(),
                        legacy_ncn_relations %>% sf::st_drop_geometry())
id_1 = legacy_ncn_relation_curr$osm_id
id_2 = legacy_ncn_relations$osm_id

ids = c()

# Find relations
for(id in id_2){
  if((id %in% id_1) == FALSE){
    message("False : ", id)
    ids = c(ids, id)
  }
}

# Networks captures by network == ncn but missed by my function
a_test = legacy_ncn_relations %>% dplyr::filter(osm_id %in% ids)

# Networks captured by my function, missed by network == ncn



tmap::tmap_mode("view")
tmap::qtm(a_test)
