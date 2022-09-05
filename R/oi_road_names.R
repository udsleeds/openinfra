#' Function to recategorise OSM data, combining both the `name` & `ref` field
#' for an OSM feature, or whichever is available. 
#'
#' @usage oi_road_names(osm_sf)
#' @param osm_sf - A Simple Features `sf` and `data.frame` object containing 
#'   OpenStreetMap infrastructure data.
#' @param remvoe - If TRUE, will remove all features that still have a NA for
#'   openinfra_road_name column (removes nameless features).
#' @return an sf object with openinfra_road_name column added, indicating the 
#'   name and ref fields of the feature, if they are included.
#' @details This function analyses OSM features, specifically the `name` and 
#'   `ref` fields that contain a road name (i.e. Otley Road) and the road 
#'   reference field (i.e A62). If both the `name` and `ref` appear, then 
#'   `openinfra_road_name` will be `name | ref`, otherwise `openinfra_road_name`
#'   will be whichever field appears within the OSM data. 
#'   
#'   Note: the `osm_sf` must contain the following tags: `c("name", "ref")`
#' @export oi_road_names
#' @examples
#' data = example_data
#' data = data %>% dplyr::mutate(ref = "ref_field")
#' example_output = oi_road_names(data)

oi_road_names = function(osm_sf, remove=FALSE){
  osm_edited = osm_sf %>% dplyr::mutate(openinfra_road_name = dplyr::case_when(
    # name & ref are there
    (! is.na(name) & ! is.na(ref)) ~ paste0(name, ", ", ref),
    
    # name there, ref is NA
    (! is.na(name) & is.na(ref)) ~ name,
    
    # ref there, name is NA
    (is.na(name) & ! is.na(ref)) ~ ref
  ))
  
  if (remove){
    # If TRUE, will remove features with NA openinfra_road_name
    osm_edited = osm_edited %>% dplyr::filter(! is.na(openinfra_road_name))
  }
  
  return(osm_edited)
}
