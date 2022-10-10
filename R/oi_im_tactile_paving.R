#' Assess the presence of tactile paving from OSM data. 
#' 
#' @usage oi_im_tactile_paving(osm_sf)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) package.
#' @return The `osm_sf` data frame is returned with additional 
#'   `openinfra_im_tactile_paving` column, containing values `c("yes", "no")`
#' @details Note: the `osm_sf` must contain the tag: `c("tactile_paving")`.
#' @export
#' @examples
#' data = example_data
#' output = oi_im_tactile_paving(data)
#' plot(output$geometry)

oi_im_tactile_paving = function(osm_sf){
  # Assesses the presence of tactile paving - either yes, no.
  osm_sf_im = osm_sf %>% 
    dplyr::mutate(openinfra_im_tactile_paving = dplyr::case_when(
      ! tactile_paving %in% c("no", "incorrect", "bad", "dangerous", "incomplete",
                            "wrong") & ! is.na(tactile_paving)
      ~ "yes",
      ! is.na(tactile_paving)
      ~ "no"
    ))
  
  return(osm_sf_im)
}