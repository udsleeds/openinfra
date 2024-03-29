% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oi_cycle_routes.R
\name{oi_cycle_routes}
\alias{oi_cycle_routes}
\title{Get Local and National Cycle Networks (lcn/ncn) from OSM data}
\usage{
oi_cycle_routes(osm_ways, osm_relations, ..., remove=FALSE)
}
\arguments{
\item{osm_ways}{\itemize{
\item A \code{sf} object containing OpenStreetMap infrastructure data
on ways (linestrings), obtained using the
\href{https://github.com/ropensci/osmextract}{\code{osmextract}} R package.
}}

\item{osm_relations}{\itemize{
\item A \code{sf} object containing OpenStreetMap infrastructure
data on relations (collection of features), obtained using the
\href{https://github.com/ropensci/osmextract}{\code{osmextract}} R package.
}}

\item{...}{Through (Named) argument \code{return_cols}, one can specify columns
returned in the joined \code{sf} object containing ways & relation cycle routes.

\code{return_cols} should contain all columns to be returned in the combined
ways & relations \code{sf} object. If no \code{return_cols} is supplied, the \code{sf}
will be returned with the following default values: "osm_id", "highway",
"name", "lcn", "ncn", "lcn_ref", "ncn_ref", "openinfra_cycle_routes",
"geometry".

\strong{Note}: columns to be returned \strong{MUST} be present in both \code{osm_ways} and
\code{osm_relations} to perform \code{rbind}. For more information see \code{?rbind()}}

\item{remove}{\itemize{
\item A boolean value, \code{FALSE} by default. If \code{TRUE}, removes
features from the combined sf object if values of
\code{openinfra_cycle_routes} are NA. (not part of a cycle route)
}}
}
\value{
a single \code{sf} object is returned containing both local and national
cycle network routes from the ways and relations layer. Information on
cycle routes assessed can be found in the \code{openinfra_cycle_routes} column.
}
\description{
This function parses OSM data at the ways (linestrings), and relations
(colection of geometrical features) level to obtain local and national
cycle network (lcn/ncn) routes.
}
\details{
Note that the routes obtained through this function are for bicycle routes
only. That is, features from relations must contain the route="bicycle" tag,
as opposed to say, route="mtb", a tag for mountain bike trails. For more
information see \href{https://wiki.openstreetmap.org/wiki/Key:route}{routes}.

Note: the \code{osm_ways} & \code{osm_relations} must contain the following
tags: "lcn", "lcn_ref", "ncn", "ncn_ref", "type", "route", "network",
"cycle_network"
}
\examples{
 osm_ways = example_data
 osm_relations = example_data_rels
 # NB: Not specifying `return_cols` here - default will be used instead. 
 output = oi_cycle_routes(osm_ways, osm_relations, remove=TRUE) 
 plot(output)  
}
