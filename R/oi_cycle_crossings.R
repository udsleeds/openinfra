#' Function to get OSM data based on cyclist crossing infrastructure.
#' 
#' 
#' @usage oi_bicycle_parking(osm_sf)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) function.
#' @param remove - Boolean - If TRUE, features that do not form part of cycle
#'   crossings are removed from the `osm_sf` before being returned.
#' @return an `sf` object with openinfra_cycle_crossings column added. 
#' @details - Note: the `osm_sf` must contain the following tags: 
#'   `c("cycleway", "crossing", "bicycle", "crossing_island", "crossing:island",
#'      "crossing:ref", "crossing_ref")`
#'   - Note: The `osm_sf` must contain linestring geometries rather than nodes.
#'   Further motivation behind this function can be found
#'   [here](https://github.com/udsleeds/openinfra/issues/126).
#' @export oi_cycle_crossings
#' @examples 
#' data = example_data
#' example_output = oi_cycle_crossings(data)
#' plot(example_output["openinfra_cycle_crossings"])
#'   
# TODO: Finnish RoxyGen documentation comments for new oi_cycle_crossings function.
# TODO: Create oi_cycle_crossing function that gets OSM data on road crossings for cyclists

oi_cycle_crossings = function(osm_sf, remove=FALSE){
  osm_sf_crossings = osm_sf %>% 
    dplyr::mutate(openinfra_cycle_crossings = dplyr::case_when(
      
      # Catches ways tagged cycleway="crossing"
      cycleway == "crossing" ~ "yes"
    ))
  
  if (remove){
    # Remove features that are not part of cycle crossings
    osm_sf_crossings = osm_sf_crossings %>% 
      dplyr::filter(openinfra_cycle_crossings == "yes")
  }
  
  return(osm_sf_crossings)
}