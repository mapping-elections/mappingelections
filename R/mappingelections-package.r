#' Mapping Early American Elections
#'
#' @name mappingelections
#' @docType package
#'
#' @importFrom dplyr %>%
#' @import USAboundaries
#' @import sf
NULL

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("meae_maps", "meae_id", "meae_maps_to_elections",
                           "meae_elections", "meae_congressional_counties",
                           "congress.x", "congress.y", "state.x", "state.y",
                           "county_ahcb", "distinct", "party",
                           "party_percentage", "party_vote", "vote",
                           "county_fips", "state", "id", "federalist_percentage",
                           "state_abbr", "ST", "population", "year",
                           "congress_numbering", "state_codes", "statename",
                           "startcong", "endcong", "histcongress",
                           "election_id", "congress", "district", "desc",
                           "total_vote", "percent_vote", "winner",
                           "district", "candidate", "append_colnames",
                           "candidate_id", "contender", "data_frame",
                           "federalist_vote", "geography_type", "<NA>",
                           "guarantee_colnames", "other_percentage",
                           "other_vote", "republican_percentage",
                           "republican_vote", "state_name", "votes"))
}
