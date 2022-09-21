#' Function to assess whether a kerb (where tagged) is flush or not.
#'
#' @usage oi_im_flush_kerb(osm_sf)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) package.
#' @return The `osm_sf` data frame is returned with additional 
#'   `openinfra_im_flush_kerb` column.
#' @details Note: the `osm_sf` must contain the following tags: `c("kerb")`
#' @export
#' @examples 
#' data = example_data
#' output = oi_im_flush_kerb(data)
#' output = output %>% dplyr::filter(! is.na(openinfra_im_flush_kerb))
#' plot(output[,"openinfra_im_flush_kerb"])
#'


oi_im_flush_kerb = function(osm_sf){
  
  osm_sf_im = osm_sf %>% 
    # Assesses whether a kerb is flush or not
    dplyr::mutate(openinfra_im_flush_kerb = 
                    dplyr::if_else(kerb == "flush" | kerb == "no", "flush", 
                                    "other"))
  return(osm_sf_im)
  }


