#' Identify bicycle parking places from OSM
#'
#' This function re-categorises OSM data, adding a column to an OSM network 
#' `openinfra_cycle_parking` with value `"yes"` if a node/way within the OSM network has
#' facilities for bicycle parking. 
#'
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#' infrastructure data, obtained from the 
#' [`osmextract`](https://github.com/ropensci/osmextract) function.
#' @param remove - if `TRUE`, removes all nodes that are not suitable for 
#' bicycle parking.
#' @return - The provided `osm_sf` is returned with an additional column, 
#' `openinfra_cycle_parking` containing the value `"yes"` if this feature allows 
#' bicycle parking.
#' @details - Note: the input `osm_sf` must containt the following tags:
#' `c("amenity")`.
#' - Note that as most (if not all) amenity tags for bicycle 
#' parking are assigned to nodes, the `osm_sf` must also contain nodes. (the
#' layer `"points"` must be requested when using `osmextract::oe_get()`)
#' @export oi_bicycle_parking 
#' @examples
#' data = example_data_pois
#' example_output = oi_bicycle_parking(data)
#' example_output = example_output %>% dplyr::filter(openinfra_cycle_parking == "yes")
#' plot(example_output["openinfra_cycle_parking"])

oi_bicycle_parking = function(osm_sf, remove = FALSE){
  osm_sf_recat = osm_sf %>% 
    dplyr::mutate(openinfra_cycle_parking = dplyr::case_when(
      amenity == "bicycle_parking" ~ "yes"
    ))
  
  if (remove){
    osm_sf_recat = osm_sf_recat %>% dplyr::filter(! is.na(openinfra_cycle_parking))
  }
  
  return(osm_sf_recat)
} 

