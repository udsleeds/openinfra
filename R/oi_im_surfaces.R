#' Function to assess surfaces of OSM ways. 
#'
#' @usage oi_im_surfaces(osm_sf)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained using the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) package.
#' @return the `osm_sf` is returned with additional columns 
#'   `openinfra_im_paved_surface` which assesses the presence and type of paving
#'   with values `c("paved", "unpaved", "unknown")` and 
#'   `openinfra_im_surface_level` which assesses whether a way is even or 
#'   uneven with values `c("even", "uneven")`
#'   on presence of surface paving, and whether or not the surface is even 
#'   respectively.
#' @details Note: the `osm_sf` must contain the following tags: `c("surface", 
#'   "smoothness")`
#' @export
#' @examples
#' data = example_data
#' output = oi_im_surfaces(data)
#' plot(output["openinfra_im_paved_surface"])
#' # Uncomment below to plot other columns
#' #plot(output["openinfra_im_surface_level"])

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
      
      TRUE & !is.na(surface) ~ "unknown"
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