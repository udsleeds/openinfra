#' OSM infrastructure data to showcase the recode_road_class openinfra function.
#'
#' @docType data
#' @keywords dataset
#' 
#' @usage 
#' data(example_data)
#' example_data
#' @format A data frame of infrastructure features for Leeds, with 36,065 features and 16 variables
#'  
#' total_place = osmextract::oe_get(\cr
#' place = place_name,\cr
#' provider = provider,\cr
#' layer = "lines",\cr
#' never_skip_vectortranslate = TRUE,\cr
#' force_download = TRUE,\cr
#' quiet = FALSE,\cr
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