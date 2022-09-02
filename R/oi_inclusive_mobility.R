#' Function to recategorise OSM data based on compliance with the Inclusive Mobility (IM)
#'
#' This function is based on the Inclusive Mobility (IM) guide
#' [UK](https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1044542/inclusive-mobility-a-guide-to-best-practice-on-access-to-pedestrian-and-transport-infrastructure.pdf).
#' 
#' See details in the link above, the function's source code, and our 
#' justification 
#' [vignette](https://udsleeds.github.io/openinfra/articles/im_get.html).
#'
#' @usage oi_inclusive_mobility(osm_sf)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap infrastructure data, obtained from the [`osmextract`](https://github.com/ropensci/osmextract) function.
#' @return The `osm_sf` data frame is returned with additional inclusive mobility columns returned.
#' @importFrom readr parse_number
#' @importFrom stringr str_detect
#' @details Note: the `osm_sf` must contain the following tags: `c("cycleway",
#'    "bicycle", "wheelchair", "kerb", "disabled", "mobility_scooter", "foot",
#'    "handicap", "lit", "access", "sidewalk", "footway", "incline", "width",
#'    "smoothness", "est_width", "sidewalk_left", "ramp_wheelchair", "priority",
#'    "footway_left", "sidewalk_both_surface", "footway_surface", "pedestrian",
#'    "footway_right", "ramp", "sidewalk_right", "sidewalk_both_width", "path",
#'    "sidewalk_left_width", "sidewalk_right_width", "sidewalk_right_surface",
#'    "sidewalk_left_surface", "maxspeed", "segregated", "sloped_curb",
#'    "surface", "tactile_paving")`
#' @export oi_inclusive_mobility
#' @examples
#' library(sf)
#' internal_data = example_data
#' dim(internal_data)
#' output = oi_inclusive_mobility(internal_data)

oi_inclusive_mobility = function(osm_sf) {

  #browser() # <-- Uncomment for debugging.
  
  osm_sf_im = osm_sf %>% 
    # Assesses whether a kerb is flush or not
    dplyr::mutate(openinfra_im_kerb = dplyr::if_else(kerb == "flush" | kerb == "no", "flush", "other")) %>% 
    
    # Assesses footway - a ‘pavement’ adjacent to a road
    dplyr::mutate(openinfra_im_footway = dplyr::case_when(
      footway %in% c("left", "right", "both", "sidewalk") |
        sidewalk %in% c("left", "right", "both", "yes", "separate") |
        # trying to capture footways shared with cyclists
        !is.na(cycleway) & # map cycling infrastructure that is an inherent part of the road
        foot %in% c("yes", "designated") |
        segregated %in% "yes"
      ~ "yes",
      TRUE ~ "no" 
    ) 
    ) %>% 
    # Assesses footpath - any other right of way for pedestrians, that does not run adjacent to a road.
    dplyr::mutate(openinfra_im_footpath = dplyr::case_when(
      highway %in% "footway" & 
        openinfra_im_footway %in% "no" | 
        # not (always) an inherent part of the road
        highway %in% c("cycleway", "bridleway", "path") & # foot = "designated" is implied
        openinfra_im_footway %in% "no" &
        ! foot %in% c("no", "private") | 
        ! access %in% c("no", "private") &
        segregated %in% "no" # shared space
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>%
    
    # Assesses presence of a crossing and what type: give-way, signal controlled, none, or yes (but the type is unknown)
    dplyr::mutate(openinfra_im_crossing = dplyr::case_when(
      stringr::str_detect(crossing, "zebra|uncontr|marked")~ "give-way",
      stringr::str_detect(crossing, "toucan|pedex|puffin|equestrian|light|signal")~ "signal-controlled",
      highway %in% "crossing" | footway  %in% "crossing" | !is.na(crossing) ~ "yes",
      TRUE ~ "no"
    )) %>% 
    
    # implied footways but there's a lack of data to verify
    dplyr::mutate(openinfra_im_footway_imp = dplyr::case_when(
      openinfra_im_footway %in% "no" &
        openinfra_im_footpath %in% "no" &
        openinfra_im_crossing %in% "no"
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>% 
    # Assesses whether the way is lit or not
    dplyr::mutate(openinfra_im_light = dplyr::case_when( 
      # highway %in% "street_lamp" |
      ! lit %in% c("no", "disused") & ! is.na(lit)
      ~ "yes",
      TRUE ~ "no"
    )
    ) %>% 
    
    # Assesses the presence of tactile paving - either yes, no.
    dplyr::mutate(openinfra_im_tactile = dplyr::case_when(
      ! tactile_paving %in% c("no", "incorrect", "bad") & ! is.na(tactile_paving) 
      ~ "yes",
      ! is.na(tactile_paving)
      ~ "no"
    )
    ) %>%
    
    # Assesses whether surface is paved, unpaved, or other
    dplyr::mutate(
      openinfra_im_surface_paved = dplyr::case_when(
        highway %in% "cycleway"
        ~ "paved",
        
        stringr::str_detect(surface,
                            "pav|asph|chipseal|concrete|paving|sett|cobble|metal|wood|stepping")
        ~ "paved",
        highway %in% c("footway", "bridleway") & # highway = footway implied surface value is unpaved
          ! surface %in% stringr::str_detect(surface, "pav|asph|chipseal|concrete|paving|sett|cobble|metal|wood|stepping")
        ~ "unpaved",
        stringr::str_detect(surface, "unpav|compact|gravel|rock|pebble|ground|dirt|grass|mud|sand|woodchips|snow|ice|salt")
        ~ "unpaved",
        TRUE & !is.na(surface) ~ "other"
      )
    ) %>% 
    # Assesses whether surface is even or uneven
    dplyr::mutate(openinfra_im_surface = dplyr::case_when(
      stringr::str_detect(surface, "asph|concrete")
      ~ "even",
      
      openinfra_im_surface_paved %in% "paved" &
        smoothness %in% c("excellent", "good")
      ~ "even",
      ! is.na(openinfra_im_surface_paved) 
      ~ "uneven"
    )
    ) %>% 
    # Assesses way width - either under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      openinfra_im_width =  width %>% 
        readr::parse_number(),
      openinfra_im_width = dplyr::case_when(
        openinfra_im_width > 0 & openinfra_im_width < 1.5 ~ " < 1.5",
        openinfra_im_width <= 1.5 & openinfra_im_width <= 2 ~ "1.5 - 2",
        openinfra_im_width > 2 ~ "> 2"
      )
    ) %>% 
    # Assesses estimated way width - either under 1.5 meters, 1.5-2 meters, or over 2 meters
    dplyr::mutate(
      openinfra_im_width_est = est_width %>% 
        readr::parse_number(),
      openinfra_im_width_est = dplyr::case_when(
        openinfra_im_width_est > 0 & openinfra_im_width_est < 1.5 ~ "< 1.5",
        openinfra_im_width_est <= 1.5 & openinfra_im_width_est <= 2 ~ "1.5 - 2",
        openinfra_im_width_est > 2 ~ "> 2"
      )
    )
  return(osm_sf_im)
}
