#' Function to recategorise OSM data based on the type of cycle infrastructure.
#' 
#' @details This function recetegorises OSM data on cycling infrastructure to one of the 
#'   protection categories defined within the LTN1/20 guidance (Mixed Traffic | 
#'   Cycle Lanes | Protected Cycling Space).
#'
#'  **Note**: The `osm_sf` must contain the following tags: c("cycleway", 
#'  "cycleway_left", "cycleway_right", "cycleway_both")   
#' @usage oi_cycle_separation(osm_sf, remove=FALSE)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) function.
#' @param remove - Boolean, FALSE by default, if TRUE, removes ways that are not
#'   part of any dedicated cycling infrastructure.
#' @return The provided `osm_sf` is returned with an additional column, 
#'   `openinfra_cycle_infa`, which indicates the type of cycling infrastructure 
#'   mapped as being one of the following values: 
#'   `c("[cycle crossing](https://tinyurl.com/cycle-crossing)",
#'   "[cycle lane](https://tinyurl.com/cycle-lane)",
#'   "Mixed traffic - 20 mph" (no cycling infrastructure, carriageway maxspeed
#'   is 20 mph), "Mixed traffic - NA maxspeed" (no cycling infrastructure, 
#'   carriageway maxspeed value is NA), "Segregated - cycleway/track" (
#'   infrastructure is segregated from carriageway by definition and is either a 
#'   [cycleway](https://tinyurl.com/higway-cycleway) or a 
#'   [cycle track](https://tinyurl.com/cycle-track)), 
#'   "shared lane/busway" (there is a lane drawn for cyclists, but it is either
#'   shared with a [busway](https://tinyurl.com/sahre-bus) or a 
#'   [shared lane](https://tinyurl.com/share-lane) with carriageway users)`
#' @export oi_cycle_separation
#' @examples
#' library(sf)
#' data = example_data
#' example_output = oi_cycle_separation(data, remove=TRUE)
#' plot(example_output$geometry) 

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

      #TODO: investigate highway=path & surface=* combinations
      
# Get cycle lanes ---------------------------------------------------------
      
# Obvious cycle lanes
      cycleway %in% c("lane", "opposite_lane") ~ "cycle lane",
      # More obscure cycle lanes
      cycleway_left %in% c("lane", "opposite_lane") ~ "cycle lane",
      cycleway_right %in% c("lane", "opposite_lane") ~ "cycle lane",
      cycleway_both %in% c("lane", "opposite_lane") ~ "cycle lane",

      # Catches cycleway crossings (crossing carriageways)
      cycleway %in% c("crossing") ~ "cycle crossing",
      

# Mixed traffic condition -------------------------------------------------
     
      # No cycling geometry present & maxspeed compliant with LTN1/20
      (cycleway %in% c("no", "none", "opposite")) & 
        (openinfra_maxspeed == "20 mph") ~ "Mixed traffic - 20 mph",
      
      (cycleway %in% c("no", "none", "opposite")) & 
        (openinfra_maxspeed == "< 20 mph") ~ "Mixed traffic - < 20 mph", 
      
      # No cycling geometry present & missing maxspeed tag 
      (cycleway %in% c("no", "none", "opposite")) & 
        (is.na(maxspeed)) ~ "Mixed traffic - NA maxspeed",

      # Cycleway is "share_busway" condition - still mixed with traffic
      (cycleway %in% c("share_busway", "shared_lane")) ~ "shared lane/busway",

# Segregated cycling infrastructure ---------------------------------------
     # Captures obvious cycle tracks - separated by definition
     cycleway %in% c("track") ~ "segregated - cycleway/track",
     # Captures more obscure track lanes
     cycleway_left %in% c("track") ~ "segregated - cycleway/track",
     cycleway_right %in% c("track") ~ "segregated - cycleway/track",
     cycleway_both %in% c("track") ~ "segregated - cycleway/track",

     # Captures highway=cycleway (fully separated by definition)
     (highway %in% c("cycleway")) ~ "segregated - cycleway/track",
     (highway %in% c("cycleway")) & 
       (separation == "no") ~ "segregated shared use cycleway",

     # Captures other cycling paths with shared use (may remove?)
     (highway %in% c("path")) & (bicycle %in% c("designated", "yes")) & 
     ((surface %in% c("asphalt", "ashphalt", "paved", "concrete", 
                     "paving_stones", "compacted", "fine_gravel", "sett")) |
     (smoothness %in% c("excellent", "good", "intermediate", "very_good",
                        "medium", "good or excellent", "intermediate;good",
                        "good;intermediate", "good;excellent")))
     ~ "yes - path"

     # For LTN1/20 separation SHOULD be used when cycling adjacent to roads, but
     # how to we determine this in OSM?
    ))
  
  if (remove){
    osm_cycle_infra = osm_cycle_infra %>% 
      dplyr::filter(! is.na(openinfra_cycle_infra))
  }
  
}
