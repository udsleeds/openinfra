#' Function to recategorise OSM data based on the type of cycle infrastructure.
#' 
#' @details This function recetegorises OSM data on cycling infrastrcutre to one of the 
#'   protection categories defined within the LTN1/20 guidance (Mixed Traffic | 
#'   Cycle Lanes | Protected Cycling Space).
#'
#'  **Note**: The `osm_sf` must contain the following tags: c("cycleway", 
#'  "cycleway_left", "cycleway_right", "cycleway_both")   
#' @usage oi_cycle_infra(osm_sf, remove=FALSE)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) function.
#' @param remove - Boolean, FALSE by default, if TRUE, removes ways that are not
#'   part of any dedicated cycling infrastructure.
#' @return The provided `osm_sf` is returned with an additional column, 
#'   `openinfra_cycle_infa`, which indicates if a way is part of cycling 
#'   infrastructure, and the level of protection provided by the infrastructure 
#'   ("mixed traffic", "cycle lanes", "protected cycling space")
#' @export oi_cycle_separation
#' @examples
#' library(sf)
#' data = example_data
#' example_output = oi_cycle_separation(data)
#' plot(example_output["openinfra_cycle_infra"]) 


# TODO: Finish oi_cycle_infa RoxyGen documentation. 
# TODO: Finish new oi_cycle_infra function code

oi_cycle_separation = function(osm_sf, remove=FALSE){
  # We need to use the oi_clean_maxspeed function to be able to assess the speed
  # limits for mixed traffic criteria - that or we just apply the other function
  # first, but I think it is still important to include a check for it.
  
  if (! "openinfra_maxspeed" %in% colnames(osm_sf)){
    # Need to add openinfra_maxspeed column for mix_traffic assessment 
    osm_sf = openinfra::oi_clean_maxspeed_uk(osm_sf, del = FALSE)    
  } 

  osm_cycle_infra = osm_sf %>% 
    dplyr::mutate(openinfra_cycle_infra = dplyr::case_when(

# Get cycle lanes ---------------------------------------------------------
      # Obvious cycle lanes
      cycleway %in% c("lane") ~ "cycle lane",
      # More obscure cycle lanes
      cycleway_left %in% c("lane") ~ "cycle lane",
      cycleway_right %in% c("lane") ~ "cycle lane",
      cycleway_both %in% c("lane") ~ "cycle lane",
      

# Mixed traffic condition -------------------------------------------------
     
      # No cycling geometry present & maxspeed compliant with LTN1/20
      (cycleway %in% c("no", "none", "opposite")) & 
        (openinfra_maxspeed == "20 mph") ~ "yes - 20 mph",
      
      (cycleway %in% c("no", "none", "opposite")) & 
        (openinfra_maxspeed == "< 20 mph") ~ "yes - < 20 mph", 
      
      # No cycling geometry present & missing maxspeed tag 
      (cycleway %in% c("no", "none", "opposite")) & 
        (is.na(maxspeed)) ~ "yes - NA maxspeed",
      

# Segregated cycling infrastructure ---------------------------------------
     # Captures obvious cycle tracks - separated by definition
     cycleway %in% c("track") ~ "yes - track",
     # Captures more obscure track lanes
     cycleway_left %in% c("track") ~ "yes - track",
     cycleway_right %in% c("track") ~ "yes - track",
     cycleway_both %in% c("track") ~ "yes - track",

     # Captures highway=cycleway (fully separated by definition)
     (highway %in% c("cycleway")) ~ "yes - cycleway",
     (highway %in% c("cycleway")) & 
       (separation == "no") ~ "shared cycleway",

     # Captures other cycling paths with shared use (may remove?)
     (highway %in% c("path")) & (bicycle %in% c("designated")) ~ "yes - path"

     # For LTN1/20 separation SHOULD be used when 
    ))
}
