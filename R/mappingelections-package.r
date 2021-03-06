#' Mapping Early American Elections
#'
#' @name mappingelections
#' @docType package
#'
#' @importFrom dplyr %>%
#' @import USAboundaries
#' @import USAboundariesData
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
                           "other_vote", "demrep_percentage",
                           "demrep_vote", "state_name", "votes",
                           "meae_congress_candidate_totals",
                           "meae_congress_counties_parties", "meae_candidates",
                           "congbio_url", "unopposed", "democratic-republican_vote",
                           "democratic-republican_percentage",
                           "geography", "level", "meae_staterepresentative_counties_parties",
                           "meae_staterepresentative_candidate_totals",
                           "meae_congressional_elections_dates" ))
}
