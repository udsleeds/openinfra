#' Re-classifies OSM Roads
#' 
#' Determines road classification defined by 
#' [Chan and Cooper's](https://www.nature.com/articles/s41598-019-55669-8) work. 
#' Specifically re-classifies roads as one of 8 road classes (0-7) specified in 
#' the following 
#' [table](https://www.nature.com/articles/s41598-019-55669-8/tables/6),
#' depending on each features `highway=` key values.
#' 
#' Re-classifies data based on the following highway key values:
#' 
#' Road Class | Description | Selection from OSM
#' -- | -- | --
#' 7 | Motorways | highway = motorway OR highway = motorway_link
#' 6 | Non-residential Dual Carriageways | highway =  trunk OR highway = trunk_link*manual classification needed
#' 5 | Residential Dual Carriageways | highway =  trunk OR highway =  trunk_link*manual classification needed
#' 4 | Primary Roads | highway =  primary OR highway =  primary_link OR (highway =  trunk AND oneway = F)
#' 3 | Secondary Roads | highway =  secondary OR highway =  secondary_link
#' 2 | Tertiary Roads | highway = tertiary OR highway = tertiary_link
#' 1 | Local Roads | highway = living_street OR highway = residential OR highway = unclassified
#' 0 | Traffic-free Paths | highway = cycleway
#'
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap 
#'    infrastructure data, obtained from the 
#'    [`osmextract`](https://github.com/ropensci/osmextract) function.
#' @param del If `TRUE`, features not re-coded to one of the 8 road 
#'    classifications are removed. `FALSE` by default.
#' @param remove - If TRUE, will remove roads not recategorised by the function
#'   into one of the pre-set definitions.
#' @usage oi_recode_road_class(osm_sf, del=FALSE)
#' @return  The \code{osm_sf} simple features data frame is returned with 
#'     additional columns openinfra_road_class and openinfra_road_desc based on 
#'     Chan and Cooper's road classifications.
#' @details Note: the `osm_sf` must contain the following tags:
#'     `c("highway", "oneway")`
#' @export oi_recode_road_class
#' 
#' @examples 
#' library(sf)
#' internal_data = example_data
#' output = oi_recode_road_class(internal_data, del = FALSE)

#' # Quick plot:
#'  plot(output["openinfra_road_desc"], key.pos = 1)
#' 
#' # Advanced plot with tmap - un-comment following four lines to run! 
#' # tmap_mode("view")
#' # tmap::tm_shape(output |> dplyr::select(openinfra_road_desc)) +
#' #  tmap::tm_lines(col = "openinfra_road_desc", title.col = "Road class") +
#' #  tmap::tm_layout(legend.bg.color = "white")

oi_recode_road_class = function(osm_sf, del = FALSE) {
  # browser() Un-comment this to perform function debugging 
  
  # Created road_class columns
  osm_recat = osm_sf %>%
    # Creates road_class column
    dplyr::mutate(openinfra_road_class = dplyr::case_when(
      # (7) - Motorways
      highway %in% c("motorway", "motorway_link") ~ "7",
      # (6/5) - Dual Carriageways resi & non-resi
      highway %in% c("trunk", "trunk_link") ~ "6/5",
      # (4) - Primary Roads
      highway %in% c("primary", "primary_link") | (highway == "trunk" & oneway == "F") ~ "4",
      # (3) - Secondary Roads
      highway %in% c("secondary", "secondary_link") ~ "3",
      # (2) - Tertiary Roads
      highway %in% c("tertiary", "tertiary_link") ~ "2",
      # (1) - Local Roads
      highway %in% c("living_street", "residential", "unclassified") ~ "1",
      # (0) - Traffic-free Paths
      highway == "cycleway" ~ "0"
    )) %>%
    
    # Creates road_description columns
    dplyr::mutate(openinfra_road_desc = dplyr::case_when(
      # (7) - Motorways
      highway %in% c("motorway", "motorway_link") ~ "Motorways",
      # (6/5) - Dual Carriageways, residential & non-residential
      (highway %in% c("trunk", "trunk_link")) & (oneway %in% c("yes", "-1", "reversible", "alternating")) ~ "Dual Carriageways (R&NR)",
      # (4) - Primary Roads
      highway %in% c("primary", "primary_link") | (highway %in% c("trunk", "trunk_link")) & (! oneway %in% c("yes", "-1", "reversible", "alternating")) ~ "Primary Roads",
      # (3) - Secondary Roads
      highway %in% c("secondary", "secondary_link") ~ "Secondary Roads",
      # (2) - Tertiary Roads
      highway %in% c("tertiary", "tertiary_link") ~ "Tertiary Roads",
      # (1) - Local Roads
      highway %in% c("living_street", "residential", "unclassified") ~ "Residential / Local Roads",
      # (0) - Traffic-free Paths
      highway == "cycleway" ~ "Cycleway"
    )) #%>%
    
  # If FALSE, remove features that have not been re-coded to a road_class value
  if (del){
    osm_recat = osm_recat %>% dplyr::filter(!is.na(openinfra_road_class))
  }
  
  return(osm_recat)
}
