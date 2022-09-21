#' Function to assess surfaces of OSM ways. 
#'
#' @usage oi_im_surfaces(osm_sf)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained using the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) package.
#' @return the `osm_sf` is returned with additional columns 
#'   `openinfra_im_surface_paved` and `openinfra_im_surface` containing values
#'   on presence of surface paving, and whether or not the surface is even 
#'   respectively.
#' @details Note: the `osm_sf` must contain the following tags: `c("surface", 
#'   "smoothness")`
#'   
#'   

# TODO: finish splitting these into a single function - assess whether 
#       im_surface is really needed. Also, does it catch roads (asphalt etc.)


oi_im_surfaces = function(osm_sf){
  # Assesses whether surface is paved, unpaved, or other
  osm_sf_im = osm_sf %>%
    dplyr::mutate(openinfra_im_paved_surface = dplyr::case_when(
      highway %in% c("cycleway") ~ "paved",
      
      stringr::str_detect(surface, 
                          paste0("pav|asph|chipseal|concrete|paving|sett|",
                                 "cobble|metal|wood|stepping")) ~ "paved",
   
      (highway %in% c("footway", "bridleway") & #highway=footway implies unpaved
        (! surface %in% stringr::str_detect(
          surface, paste0("pav|asph|chipseal|concrete|paving|sett|",
                          "cobble|metal|wood|stepping")))) ~ "unpaved",
      
      stringr::str_detect(surface, 
                          paste0("unpav|compact|gravel|rock|pebble|ground|dirt",
                                 "|grass|mud|sand|woodchips|snow|ice|salt"))   
      ~ "unpaved",
      
      TRUE & !is.na(surface) ~ "other"
    )) %>%

    # Assesses whether surface is even or uneven
    dplyr::mutate(openinfra_im_surface_level = dplyr::case_when(
      stringr::str_detect(surface, "asph|concrete") ~ "even",
      
      openinfra_im_paved_surface %in% c("paved") &
        smoothness %in% c("excellent", "good") ~ "even", 
      
      (! is.na(openinfra_im_paved_surface)) ~ "uneven"
    ))
  
  return(osm_sf_im)
}
