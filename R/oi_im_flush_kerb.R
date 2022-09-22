#' Function to assess whether a kerb (where tagged) is flush or not.
#'
#' @usage oi_im_flush_kerb(osm_sf)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data, obtained from the 
#'   [`osmextract`](https://github.com/ropensci/osmextract) package.
#' @return The `osm_sf` data frame is returned with additional 
#'   columns `openinfra_im_kerb`, assessing the presence and type of kerb with 
#'   values `c("lowered kerb", "flush kerb", "no kerb", "raised kerb",
#'   "rolled kerb", "normal kerb", "unknown", "missing data")` and
#'   `openinfra_accessible_kerb`, assesses accessibility of kerb for 
#'   pedestrians with impaired mobility with values `c("wheelchair - yes", 
#'   "wheelchair - no", "lacking data")` and `openinfra_im_flush_kerb` which
#'   assesses whether or not there is a flush kerb compliant with the latest
#'   [Inclusive Mobility](https://tinyurl.com/IM-guide-link) Guidance.
#' @details Note: the `osm_sf` must contain the following tags: `c("kerb")`
#' @export
#' @examples 
#' data = example_data
#' output = oi_im_flush_kerb(data)
#' output = output %>% dplyr::filter(! is.na(openinfra_kerb))
#' plot(output[,"openinfra_kerb"])

oi_im_flush_kerb = function(osm_sf){

  osm_sf_im = osm_sf %>% 
    # Assesses the presence of a kerb, and what type of kerb it is.
    dplyr::mutate(openinfra_kerb = dplyr::case_when(
      stringr::str_detect(kerb, "lower") ~ "lowered kerb",
      kerb == "flush" ~ "flush kerb",
      kerb %in% c("no", "none", "flat") ~ "no kerb", 
      kerb == "raised" ~ "raised kerb",
      kerb == "rolled" ~ "rolled kerb",
      kerb %in% c("yes", "regular", "normal") ~ "normal kerb",
      (TRUE & !is.na(kerb)) ~ "unknown",
      is.na(kerb) ~ "missing data"
    )) %>% 
    dplyr::mutate(openinfra_im_accessible_kerb = dplyr::case_when(
      # Based on assessments above, assesses whether the kerb is accessible
      # for wheelchair users. Definitions decided based on kerb wiki below.
      # See: https://wiki.openstreetmap.org/wiki/Key:kerb
      openinfra_kerb %in% c("lowered kerb", "flush kerb",
                               "no kerb") ~ "wheelchair - yes",
      openinfra_kerb %in% c("raised kerb", "rolled kerb", "normal kerb"
                               ) ~ "wheelchair - no",
      openinfra_kerb %in% c("unknown", "missing data") ~ "lacking data"
    )) %>% 
    dplyr::mutate(openinfra_im_flush_kerb = dplyr::case_when(
      openinfra_kerb %in% c("flush kerb", "no kerb") ~ "yes"
    ))
  
  return(osm_sf_im)
  }