#' Function to recategorise OSM infrastructure based on cycleability
#' 
#' Determines whether or not a piece of OSM infrastructure is cyclable or not 
#' depending on whether the feature is included in the default cycling network
#' defined by [`osmextract`](https://github.com/ropensci/osmextract)
#' 
#' @usage oi_active_cycle(osm_sf, remove=FALSE)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) function.
#' @param remove - Boolean - If TRUE, features that do not form part of a 
#'    cyclable network are removed from the `osm_sf` before being returned
#' @return an sf object with oi_active_cycle column added.
#' @details This function mimics the cycling filtering steps applied by the 
#' [`oe_get_network`](https://rdrr.io/cran/osmextract/man/oe_get_network.html)
#' function, part of the 
#' [`osmextract`](https://rdrr.io/cran/osmextract/) package.
#' @details Note: the `osm_sf` must contain the following tags: `c("highway", "bicycle", "access", "service")`
#' @export oi_active_cycle
#' @examples
#' data = example_data
#' example_output = oi_active_cycle(data)
#' #plot(output["oi_cycle"], key.pos = 1)
#' 
oi_active_cycle = function(osm_sf, remove=FALSE){
  #browser() # Uncomment to debug
  osm_sf_cycle = osm_sf %>% dplyr::mutate(oi_cycle = dplyr::case_when(
    
    # Highway cannot be NA
    is.na(highway) ~ "no",
    
    # Highway values not appropriate for cycling
    highway %in% c('abandoned', 'bus_guideway', 'byway', 'construction', 
                   'corridor', 'elevator', 'fixme', 'escalator', 'gallop', 
                   'historic', 'no', 'planned', 'platform', 'proposed', 
                   'raceway', 'steps') 
    ~ "no",
    
    # Create two cases to catch features typically not allowed for cyclists, but have been flagged appropriate:
    # 1 - If highway = "bad highway values" BUT bicycle = "good bicycle values" then assign ~ "yes",
    (highway %in% c('motorway', 'motorway_link', 'footway', 'bridleway','pedestrian') & bicycle %in% c('yes', 'designated', 'permissive', 'destination')) ~ "yes",
    ((highway == "footway") & (! bicycle %in% c("no", "dismount", "private"))) & (! access %in% c("private", "no", "permit")) ~ "yes",
    
    # 2 - Assign highway = "bad highway values" ONLY as ~ "no". Ways that are appropriate for cyclists will already be assigned "yes" from above
    highway %in% c('motorway', 'motorway_link', 'footway', 'bridleway','pedestrian') ~ "no",
    
    # Way must have access rights
    access %in% c('private', 'no') ~ "no",
    
    # Way must not bar cyclists
    bicycle %in% c('no', 'private', 'ue_sidepath', 'restricted') ~ "no", 
    
    # Way must not contain "private" within the service tag
    grepl("private", service) ~ "no",

  ))
  
  # Case_when above should have added "no" to all inappropriate features, now 
  # find features that do not contain "no" and set as "yes"
  osm_sf_cycle$oi_cycle[is.na(osm_sf_cycle$oi_cycle)] = "yes"
  
  # If remove = TRUE, filter out features that have oi_cycle == "no"
  if (remove){
    osm_sf_cycle = osm_sf_cycle %>% dplyr::filter(osm_sf_cycle$oi_cycle == "yes")
  }
  return(osm_sf_cycle)
}