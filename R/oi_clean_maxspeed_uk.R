#' Re-classifies the maxspeed column of an OSM data frame to be compliant with current [UK speed limits](https://www.gov.uk/speed-limits). 
#' 
#' The clean re-coded speeds are stored in `openinfra_maxspeed`.
#'
#' @usage oi_clean_maxspeed_uk(osm_sf, no_NA = FALSE, del = FALSE) 
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap infrastructure data, obtained from the [`osmextract`](https://github.com/ropensci/osmextract) function.
#' @param no_NA - Boolean, `FALSE` by default. If `TRUE` then any oi_maxspeed == NA are removed. (i.e. maxspeed value is NOT compliant with [UK speed limits](https://www.gov.uk/speed-limits)) are removed from \code{osm_sf}.
#' @param del - Boolean, `FALSE` by default. If `TRUE` then the original `maxspeed` column is deleted and only `openinfra_maxspeed` is returned.
#' @return  The `osm_sf` simple features data frame is returned with the maxspeed column values cleaned based on `allowed_speeds`.
#' @details Note: the `osm_sf` param must contain the following tags: `c("maxspeed", "highway")`
#' @export oi_clean_maxspeed_uk
#' @examples 
#' library(sf)
#' internal_data = example_data
#' output = oi_clean_maxspeed_uk(internal_data, no_NA = TRUE)
#' plot(output["openinfra_maxspeed"])
#'  
#' #' # Advanced plot with tmap - un-comment following four lines to run! 
#' #' tmap_mode("view")
#' #' tmap::tm_shape(output |> dplyr::select(openinfra_maxspeed)) +
#' #' tmap::tm_lines(col = "openinfra_maxspeed", title.col = "Cleaned maxspeed") +
#' #' tmap::tm_layout(legend.bg.color = "white")

oi_clean_maxspeed_uk = function(osm_sf, no_NA = FALSE, del = FALSE) {
  
  # Define NOT IN 
  `%!in%` = Negate(`%in%`)
  
  osm_clean = osm_sf %>%
    dplyr::mutate(openinfra_maxspeed = dplyr::case_when(
      # maxspeed == national, when on motorway
      (maxspeed == "national" & highway %in% c("motorway", "motorway_link")) ~ "70 mph",
      
      # maxspeed == national, when NOT on motorway
      (maxspeed == "national" & highway %!in% c("motorway", "motorway_link")) ~ "60 mph",  
      
      # maxspeed == national, when on standard (i.e Non-Residential) dual carriageway
      # Default is 60 mph - if there is physical separation this is 70 mph, but 
      # assume that if there is separation then maxspeed tag = 70 mph. Rather be
      # conservative stating 60 mph if not stated.
      (maxspeed == "national" & highway %in% c("trunk", "trunk_link")) ~ "60 mph",
      
      # Catch maxspeeds of 5, 10, 15 and set to 20 mph
      maxspeed %in% c("5", "10", "15", 
                      "5 mph", "10 mph", "15 mph") ~ "< 20 mph",
      
      # maxspeed == (20|30|40|50|60|70 --> + mph)
      maxspeed == "20" ~ "20 mph",
      maxspeed == "30" ~ "30 mph",
      maxspeed == "40" ~ "40 mph",
      maxspeed == "50" ~ "50 mph",
      maxspeed == "60" ~ "60 mph",
      maxspeed == "70" ~ "70 mph",
      
      # Already cleaned speeds (to stop mutate missing these)
      maxspeed == "20 mph" ~ "20 mph",
      maxspeed == "30 mph" ~ "30 mph",
      maxspeed == "40 mph" ~ "40 mph",
      maxspeed == "50 mph" ~ "50 mph",
      maxspeed == "60 mph" ~ "60 mph",
      maxspeed == "70 mph" ~ "70 mph",
    )) 
  
  if (no_NA){
    # if TURE, will remove features if their oi_maxspeed == NA
    osm_clean = osm_clean %>% dplyr::filter(!is.na(openinfra_maxspeed))
  }
  
  if (del){
    # If TRUE, will delete original `maxspeed` column
    osm_clean = subset(osm_clean, select = -c(maxspeed))
  } 

  return(osm_clean)
  }
