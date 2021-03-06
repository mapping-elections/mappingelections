% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-manipulation.R
\name{get_county_map_data}
\alias{get_county_map_data}
\title{Get the data to map party vote percentages by county state by state}
\usage{
get_county_map_data(map_id, election_date = NULL)
}
\arguments{
\item{map_id}{The ID of the map from \code{meae_maps}.}

\item{election_date}{The date to be used when getting the county boundaries.}
}
\description{
Get the data to map party vote percentages by county state by state
}
\examples{
get_county_map_data("meae.congressional.congress01.md.county")
}
