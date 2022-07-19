#' Function to recategorise OSM infrastrcutre based on lighting presence.
#' 
#' Determins whether or not a piece of OSM infrastructure is lit or not.
#' @usage oi_is_lit(osm_sf, remove=FALSE)
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'   infrastructure data.
#' @param remove - Boolean - If TRUE, features that do not have lighting
#'    are removed from the `osm_sf` before being returned.
#' @return an sf object with oi_is_lit column added, indicating if the way is
#'    lit or not.
#' @details This function adds a new column, `oi_is_lit` containing values "yes"
#' , "no", whether or not the feature is lit.
#' @export oi_is_lit
#' 
#' @examples
#' data = example_data
#' example_output = oi_is_lit(data)
#' 
#' # Quick Plot
#' plot(example_output["oi_is_lit"], key.pos = 1)
#' 
#' # Advanced plot with tmap - un-comment following four lines to run! 
#' #tmap::tmap_mode("view")
#' # tmap::tm_shape(example_output |> dplyr::select(oi_is_lit)) +
#' # tmap::tm_lines(col = "oi_is_lit", title.col = "Lighting presence") +
#' # tmap::tm_layout(legend.bg.color = "white")
#' 
"oi_is_lit"

oi_is_lit = function(osm_sf, remove=FALSE){
  
  # Select only the ways that have a highway value (not train tracks etc.)
  osm_sf = osm_sf %>% dplyr::filter(! is.na(osm_sf$highway))
  
  # Recategorise the data
  osm_sf_lit = osm_sf %>% dplyr::mutate(oi_is_lit = dplyr::case_when( 
    # If lit=* is not a unlit value - set as yes.
    (! lit %in% c("no", "disused") & ! is.na(lit)) ~ "yes",
    # If way is known to be unlit
    lit %in% c("no", "disued") ~ "no",
    
    # Used if street light is an LED (increasing popularity)
    lit_by_led == "yes" ~ "yes - LED",
    lit_by_led == "no" ~ "no - LED",
    ))
  
  # Finally - all remaining N/A are unknown as either lit or unlit.
  osm_sf_lit$oi_is_lit[is.na(osm_sf_lit$oi_is_lit)] = "unknown"
  
  # If remove = TRUE, filter out features that have oi_is_lit == "no"
  if (remove){
    osm_sf_lit = osm_sf_lit %>% dplyr::filter(osm_sf_lit$oi_is_lit %in% c("yes", "yes - LED", "unknown"))
  }
  
  return(osm_sf_lit)
}