#'Function to recategorise OSM infrastrcutre based on pedestrian walkability.
#'
#' Determins whether or not a piece of OSM infrastructure is walkable or not 
#' depending on whether the feature is included in the default walking network
#' defined by [`osmextract`](https://github.com/ropensci/osmextract)
#' 
#' @usage oi_active_walk(osm_sf, remove=FALSE)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) function.
#' @param remove - Boolean - If TRUE, features that do not form part of a 
#'    walkable network are removed from the `osm_sf` before being returned
#' @return an sf object with oi_walk column added.
#' @details This function mimics the walking filtering steps applied by the 
#' [`oe_get_network`](https://rdrr.io/cran/osmextract/man/oe_get_network.html)
#' function, part of the 
#' [`osmextract`](https://rdrr.io/cran/osmextract/) package.
#' @details Note: the `osm_sf` must contain the following tags: `c("highway", "foot", "access", "service")`
#' @export oi_active_walk
#' @examples 
#' data = example_data
#' example_output = oi_active_walk(data)

oi_active_walk = function(osm_sf, remove = FALSE){
  #browser() #<-- Uncomment to debug function.
  osm_sf_walking = osm_sf %>% dplyr::mutate(openinfra_walk = dplyr::case_when(
    
    # Highway tag cannot be NA
    is.na(highway) ~ "no",
    
    # Highway tag cannot have un-walkable values
    highway %in% c('abandoned', 'bus_guideway', 'byway', 'construction', 
                   'corridor', 'elevator', 'fixme', 'escalator', 'gallop', 
                   'historic', 'no', 'planned', 'platform', 'proposed',
                   'raceway', 'motorway', 'motorway_link') 
    ~ "no",
    
    # Hihgway tag cannot be a cycleway UNLESS walking is permitted
    ((highway == "cycleway") & (foot %in% c('yes', 'designated', 'permissive', 'destination'))) ~ "yes",
    # Below catches highway == cycleway and foot != values below INCLUDING NAs. 
    ((highway == "cycleway") & (! foot %in% c('yes', 'designated', 'permissive', 'destination'))) ~ "no",
    
    (highway %in% c("footway", "pedestrian", "path")) & (! foot %in% c("no", "private")) & (! access %in% c("private", "no")) ~ "yes",
    
    # Access cannot be restricted
    ((access %in% c('private', 'no')) & (foot == "yes")) ~ "yes",
    ((access %in% c('private', 'no'))) ~ "no",
    
    # Foot usage must be permitted,
    foot %in% c('private', 'no', 'use_sidepath', 'restricted') ~ "no",
    
    # Service value does not contain "private"
    ((grepl("private", service)) & (foot == "yes")) ~ "yes",
    ((grepl("private", service)) & (foot != "yes")) ~ "no",
    
  ))
  
  # find features that do not contain "no" and set as "yes"
  osm_sf_walking$openinfra_walk[is.na(osm_sf_walking$openinfra_walk)] = "yes"
  
  # If remove = TRUE, filter out features that have oi_walking == "no"
  if (remove){
    osm_sf_walking = osm_sf_walking %>% dplyr::filter(osm_sf_walking$openinfra_walk == "yes")
  }
  
  return(osm_sf_walking)
}