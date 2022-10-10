#' Assesses whether a flush kerb is compliant with Inclusive Mobility guidance. 
#' 
#' This function is used to assess if a flush kerb is complicit with current 
#' [Inclusive Mobility](https://tinyurl.com/IM-guide-link) guidance. The 
#' function first assesses the presence of tactile paving, which should be 
#' present for a compliant flush kerb, before assessing the kerb type.  
#' @usage oi_im_flush_kerb(osm_sf)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) package.
#' @return The `osm_sf` data frame is returned with additional 
#'   columns `openinfra_im_tactile_paving`, assessing the presence of tactile 
#'   paving with values `c("yes", "no")`, and
#'   `openinfra_im_flush_kerb`, which assesses whether or not there is a flush
#'   kerb compliant with the latest
#'   [Inclusive Mobility](https://tinyurl.com/IM-guide-link) Guidance.
#'   
#' @details Note: `osm_sf` must contain the tags: `c("kerb", "tactile_paving")`.
#' @export
#' @examples 
#' data = example_data
#' output = oi_im_flush_kerb(data)
#' output = output %>% dplyr::filter(! is.na(openinfra_im_flush_kerb))
#' plot(output[,"openinfra_im_flush_kerb"])

oi_im_flush_kerb = function(osm_sf){

  # For IM compliant flush kerbs, tactile paving should be present. 
  osm_sf_im = oi_im_tactile_paving(osm_sf)
  
  # Now we can assess IM flush kerbs using presence of tactile paving.
  osm_sf_im = osm_sf_im %>% 
    dplyr::mutate(openinfra_im_flush_kerb = dplyr::case_when(
      kerb %in% c("flush", "no") & openinfra_im_tactile_paving == "yes"
      ~ "yes",
      TRUE ~ "no"
    ))
  return(osm_sf_im)
  }