#' Mapping Early American Elections
#'
#' @name mappingelections
#' @docType package
#'
#' @importFrom dplyr %>%
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("meae_maps", "meae_id", "meae_maps_to_elections",
                           "meae_elections", "meae_congressional_counties",
                           "congress.x", "congress.y", "state.x", "state.y",
                           "county_ahcb", "distinct", "party",
                           "party_percentage", "party_vote", "vote",
                           "county_fips", "state"))
}
