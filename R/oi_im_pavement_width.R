#' Function to assess pedestrian infrastructure widths.
#' 
#' @usage oi_im_pavement_width(osm_sf)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) package.
#' @return the `osm_sf` is returned with additional columns `openinfra_im_width`
#'   and `openinfra_im_width_est` with recategorised way width and estimated 
#'   width value ranges appended respectively.
#' @details Note: the `osm_sf` must contain the following tags: `c("width", 
#'   "est_width")`
#' @export 
#' @examples
#' data = example_data
#' output = oi_im_pavement_width(data)
#' plot(output["openinfra_im_width_est"])
#' # Uncomment below to plot other columns
#' # plot(output["openinfra_im_width"])

oi_im_pavement_width = function(osm_sf){
  # Assesses way width - either < 1.5 meters, 1.5-2 meters, or > 2 meters.
  osm_sf_im = osm_sf %>% dplyr::mutate(openinfra_im_width = width %>% 
    readr::parse_number(),
    openinfra_im_width = dplyr::case_when(
      openinfra_im_width > 0 & openinfra_im_width < 1.5 ~ "< 1.5",
      openinfra_im_width <= 1.5 & openinfra_im_width <= 2 ~ "1.5 - 2",
      openinfra_im_width > 2 ~ "> 2"
    )) %>%
    
  # Assesses estimated way width - < 1.5 meters, 1.5-2 meters, > 2 meters.
  dplyr::mutate(
    openinfra_im_width_est = est_width %>% 
      readr::parse_number(),
    openinfra_im_width_est = dplyr::case_when(
      openinfra_im_width_est > 0 & openinfra_im_width_est < 1.5 ~ "< 1.5",
      openinfra_im_width_est <= 1.5 & openinfra_im_width_est <= 2 ~ "1.5 - 2",
      openinfra_im_width_est > 2 ~ "> 2"
    ))
  
  return(osm_sf_im)
}