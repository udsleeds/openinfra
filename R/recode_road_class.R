#' Re-classifies OSM Roads
#' 
#' Determines road classification defined by [Chan and Cooper's](https://www.nature.com/articles/s41598-019-55669-8) work. 
#' Specifically re-classifies roads as one of 8 road classes (0-7) specified in the following [table](https://www.nature.com/articles/s41598-019-55669-8/tables/6),
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
#' 
#' @param osm_sf - A `sf` and `data.frame` object containing OpenStreetMap infrastructure data, obtained from the [`osmextract`](https://github.com/ropensci/osmextract) function.
#' @return  The \code{osm_sf} simple features data frame is returned with additional columns road_class and road_desc based on Chan and Cooper's road classifications.
#' @export
#' 
#'
#' @examples 
#' library(sf)
#' u_data_large = paste0("https://github.com/udsleeds/openinfra/releases",
#'                       "/download/v0.2/bbbike_leeds_27_6_22.geojson")
#' u_data_small = paste0("https://github.com/udsleeds/openinfra/releases",
#'                       "/download/v0.2/30_06_22_bbbike_LCC_func_example_5_75km.geojson")
#' internal_data = example_data
#' output = recode_road_class(internal_data)

#' # Quick plot:
#'  plot(output["road_desc"], key.pos = 1)
#' 
#' # Advanced plot with tmap - un-comment following four lines to run! 
#' # tmap_mode("view")
#' # tmap::tm_shape(output |> dplyr::select(road_desc)) +
#' #  tmap::tm_lines(col = "road_desc", title.col = "Road class") +
#' #  tmap::tm_layout(legend.bg.color = "white")

recode_road_class = function(osm_sf) {
  # browser() Uncomment this to perform function debugging 

  # Created road_class columns
  osm_recat = osm_sf %>%
    # Creates road_class column
    dplyr::mutate(road_class = dplyr::case_when(
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
    dplyr::mutate(road_desc = dplyr::case_when(
      # (7) - Motorways
      highway %in% c("motorway", "motorway_link") ~ "Motorways",
      # (6/5) - Dual Carriageways resi & non-resi
      highway %in% c("trunk", "trunk_link") ~ "Dual Carriageways (R&NR)",
      # (4) - Primary Roads
      highway %in% c("primary", "primary_link") | (highway == "trunk" & oneway == "F") ~ "Primary Roads",
      # (3) - Secondary Roads
      highway %in% c("secondary", "secondary_link") ~ "Secondary Roads",
      # (2) - Tertiary Roads
      highway %in% c("tertiary", "tertiary_link") ~ "Tertiary Roads",
      # (1) - Local Roads
      highway %in% c("living_street", "residential", "unclassified") ~ "Residential / Local Roads",
      # (0) - Traffic-free Paths
      highway == "cycleway" ~ "Cycleway"
    )) %>%
    
    # Removes features that have not been recodeed to a road_class value
    dplyr::filter(!is.na(road_class))
}
