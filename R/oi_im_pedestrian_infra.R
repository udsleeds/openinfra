#' Function to assess pedestrian infrastructure data. 
#'
#' @usage oi_im_pedestrian_infra(osm_sf)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) package.
#' @return The `osm_sf` data frame is returned with additional 
#'   `openinfra_im_pedestrian_infra` column, containing descriptive values on 
#'   pedestrian infrastructure.
#' @details Note: the `osm_sf` must contain the following tags: `c(
#'   "footway", "sidewalk", "cycleway", "foot", "segregated", "access", 
#'   "crossing", "footway")`
#' @export
#' @examples 
#' data = example_data
#' output = oi_im_pedestrian_infra(data)
#' plot(output["openinfra_im_footway"])
#' #plot(output["openinfra_im_footpath"])
#' #plot(output["openinfra_im_crossing"])
#' #plot(output["openinfra_im_footway_imp"])


oi_im_pedestrian_infra = function(osm_sf){
  osm_sf_im = osm_sf %>% 
    
  # Assesses footway presence - a ‘pavement’ adjacent to a road
  # openinfra_im_footway
  dplyr::mutate(openinfra_im_footway = dplyr::case_when(
    
    # Captures obvious footways
    footway %in% c("left", "right", "both", "sidewalk") | 
    sidewalk %in% c("left", "right", "both", "yes", "separate") |
    
    # Captures footways shared with cyclists
    # cycling infrastructure that is part of carriageway
    (! is.na(cycleway) & cycleway != "no") & 
    foot %in% c("yes", "designated") | segregated %in% c("yes") ~ "footway",
    TRUE ~ "no"
  )) %>% 
    
  # openinfra_im_footpath
  # Assesses footpath, any other pedestrian right of way not adjacent to a road.
  dplyr::mutate(openinfra_im_footpath = dplyr::case_when(
    (highway %in% "footway" & openinfra_im_footway %in% c("no")) |
       # not (always) an inherent part of the road
       highway %in% c("cycleway", "bridleway", "path") & # foot = "designated" is implied
       openinfra_im_footway %in% "no" &
       ! foot %in% c("no", "private") | 
       ! access %in% c("no", "private") &
       segregated %in% c("no") # a shared space
    ~ "footpath",
    TRUE ~ "no"
  )) %>%
    
  # openinfra_im_crossing
  # Assesses presence and type (where available) of pedestrian crossing:
  # Values: give-way, signal controlled, none, or yes (but the type is unknown)
  dplyr::mutate(openinfra_im_crossing = dplyr::case_when(
    stringr::str_detect(crossing, "zebra|uncontr|marked") 
    ~ "give-way crossing",
    
    stringr::str_detect(crossing, paste0("toucan|pedex|puffin|equestrian",
                                         "|light|signal")) 
    ~ "signal-controlled crossing",
    
    highway %in% c("crossing") | 
      footway  %in% "crossing" | 
      !is.na(crossing) ~ "unknown crossing type",
    TRUE ~ "no"
  )) %>%
    
  # openinfra_im_imp_footway
  # Assesses implied footways but there's a lack of data to verify
  dplyr::mutate(openinfra_im_footway_imp = dplyr::case_when(
    openinfra_im_footway %in% c("no") & 
    openinfra_im_footpath %in% c("no") & 
    openinfra_im_crossing %in% c("no") ~ "implied footway",
    TRUE ~ "no"
  ))
  
  return(osm_sf_im)
}

#1 # openinfra_im_footway | ["footway", "sidewalk", "cyclewalk", "foot", "segregated"] | ("yes", "no") 
#2 # openinfra_im_footpath | ["highway", "openinfra_im_footway", "foot", "access", "segregated"] | ("yes", "no")
#3 # openinfra_im_crossing (pedestrian) | ["crossing", "highway", "footway"] | ("give-way", "signal-controlled", "yes" ,"no")
#4 # openinfra_im_imp_footway | ["openinfra_im_footway", "openinfra_im_footpath", "openinfra_im_crossing"] | ("yes", "no")