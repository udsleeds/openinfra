#' OSM infrastructure data to showcase the recode_road_class openinfra function.
#'
#' @docType data
#' 
#' @usage data(example_data)
#' 
#' @format A data frame of infrastructure features for Leeds, with 36,065 features and 16 variables
#'  
#' total_place = osmextract::oe_get(
#' place = place_name,
#' provider = provider,
#' layer = "lines",
#' never_skip_vectortranslate = TRUE,
#' force_download = TRUE,
#' quiet = FALSE,
#' extra_tags = c(all_extra_tags, "oneway", "maxspeed") # Add in "oneway" for recode_road_class)
#'
#'
#' @source {Created using the [osmextract](https://github.com/ropensci/osmextract) package for R using oe_get() }
#'          
#'
#'
#' @examples 
#' data(example_data)    # Lazy Loading. Data becomes visible as soon as it is requested. 
"example_data"