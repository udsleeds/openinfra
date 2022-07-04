#' OSM infrastructure data to showcase the recode_road_class openinfra function.
#'
#' @docType data
#' @keywords dataset
#' 
#' @format A data frame of infrastructure features for Leeds, with 36,065 features and 16 variable. Each feature is described by the following columns.\cr
#'  \describe{
#'      \item{osm_id}{Unique feature type (Node|Way|Relation) and numeric ID combination. e.g OSM Way 2746 has osm_id = W2746.}
#'      \item{name}{This key is set to the primary name of the feature in the real world.}
#'      \item{highway}{Main key used to identify any kind of road, street or path.}
#'      \item{waterway}{Key used to describe water flows like rivers, streams, boat-yards. Or, elements that interact with water like dams, weirs, locks etc.}
#'      \item{aerialway}{Key used for various forms of passenger or goods transport that uses wires, including cable-cars, chair-lifts, gondolas etc.}
#'      \item{barrier}{Key used for a physical ground structures which blocks or impeads movement.}
#'      \item{man_made}{Key used to identify man made structures.}
#'      \item{access}{Key to describe restrictions on the use of highways, and other trasnportation routes (warterways|railways|aerialways) as well as facilities such as buildings.}
#'      \item{service}{Key to show feature is used for access to a building, service station, beach, campsite, industrial estate, business park etc. Commonly also used for acces to parking, driveways and alleyways.}
#'      \item{bicycle}{Key to indicate legal restrictions for cyclists. i.e "dismount" implies dismount the cycle, "yes" implies suitable for cyclists.}
#'      \item{foot}{Key to indicate legal resrictions for pedestrians}
#'      \item{oneway}{Key to indicate the oneway access restriction on highways, and other ways, for vehicles (car|bicycle|truck etc.) as appropriate }
#'      \item{maxspeed}{Key used to define the maximum legal speed limit for general traffic on a particular road.}
#'      \item{z_order}{A field in osm2pgsql datamodel that map OSM Layers in OSM tags to a number in osm2pgsql model. See [Within Renderers](https://wiki.openstreetmap.org/wiki/Layer#Within_the_renderer_.2F_Mapnik)}
#'      \item{other_tags}{Any additional tags attributed to a feature that is not returned as its own column (muhc like this column) by default.}
#'      \item{geometry}{A column that contains the geometry of the feature in question. Typically either Linestring, MultiLinestring, Polygon, MultiPolygon.}
#'      }
#'  
#' \cr
#' place_name = "Leeds"\cr
#' provider = "bbbike"\cr
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
#' @source {Created using the [osmextract](https://github.com/ropensci/osmextract) package for R using oe_get() - params shown above.}
#'      
#'
#'
#' @examples 
#' example_data   # Lazy Loading. Data becomes visible as soon as it is requested. 
"example_data"