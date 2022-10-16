#' Get Local and National Cycle Networks (lcn/ncn) from OSM data
#'
#' This function parses OSM data at the ways (linestrings), and relations 
#' (colection of geometrical features) level to obtain local and national 
#' cycle network (lcn/ncn) routes.
#' 
#' @usage oi_cycle_routes(osm_ways, osm_relations, ..., remove=FALSE)
#' @param osm_ways - A `sf` object containing OpenStreetMap infrastructure data
#'   on ways (linestrings), obtained using the
#'   [`osmextract`](https://github.com/ropensci/osmextract) R package.
#' @param osm_relations - A `sf` object containing OpenStreetMap infrastructure 
#'   data on relations (collection of features), obtained using the
#'   [`osmextract`](https://github.com/ropensci/osmextract) R package.
#' @param remove - A boolean value, `FALSE` by default. If `TRUE`, removes 
#'   features from the combined sf object if values of 
#'   `openinfra_cycle_routes` are NA. (not part of a cycle route)
#' @param ... Through (Named) argument `return_cols`, one can specify columns
#'   returned in the joined `sf` object containing ways & relation cycle routes.
#'   
#'   `return_cols` should contain all columns to be returned in the combined 
#'   ways & relations `sf` object. If no `return_cols` is supplied, the `sf` 
#'   will be returned with the following default values: "osm_id", "highway", 
#'   "name", "lcn", "ncn", "lcn_ref", "ncn_ref", "openinfra_cycle_routes", 
#'   "geometry".
#'   
#'   **Note**: columns to be returned MUST be present in both `osm_ways` and 
#'   `osm_relations`
#'   
#' @return a single `sf` object is returned containing both local and national
#'   cycle network routes from the ways and relations layer. Information on 
#'   cycle routes assessed can be found in the `openinfra_cycle_routes` column. 
#' @details Note: the `osm_ways` & `osm_relations` must contain the following
#'  tags: "lcn", "lcn_ref", "ncn", "ncn_ref", "type", "route", "network", 
#'  "cycle_network"
#' @importFrom methods hasArg
#' @export
#' @examples
#'  osm_ways = example_data
#'  osm_relations = example_data_rels
#'  # NB: Not specifying `return_cols` here - default will be used instead. 
#'  output = oi_cycle_routes(osm_ways, osm_relations, remove=TRUE) 
#'  plot(output)  
oi_cycle_routes = function(osm_ways, osm_relations, ..., remove=FALSE){

  if(!hasArg(return_cols)){
    message("No `return_cols` argument supplied, using default values:\n ",
            paste0('\n"osm_id", "highway", "name", "lcn", "ncn", "lcn_ref",', 
                   '"ncn_ref", "openinfra_cycle_routes", "geometry"\n'),
            "\nFor more information see `?openinfra::oi_cycle_routes()`\n")
    # Use default values
    return_cols = c("osm_id", "highway", "name", "lcn", "ncn", "lcn_ref", 
                    "ncn_ref", "openinfra_cycle_routes", "geometry")
  } else {
    # Combine supplied return_cols with default
    return_cols = c(return_cols, "osm_id", "highway", "name", "lcn", "ncn",
                    "lcn_ref", "ncn_ref", "openinfra_cycle_routes", "geometry")
    
    # Remove potential duplicated tags
    return_cols = unique(return_cols)
  }
  
  
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
      (!is.na(lcn) & lcn!="no") | network=="lcn" 
      ~ paste("lcn:", lcn, lcn_ref, ref),
      #message("ncn ways")
      # ways a part of ncn routes must be tagged ncn=*
      (!is.na(ncn) & ncn!="no") | network=="ncn"  
      ~ paste("ncn:", ncn, ncn_ref)
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