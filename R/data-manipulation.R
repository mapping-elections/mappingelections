#' Get the votes by candidate and geographic unit
#'
#' Given a MEAE map ID, get the votes by candidate and geographic unit for the
#' elections associated with that map.
#'
#' @param map_id An MEAE map ID from the \code{\link{meae_maps}} table.
#'
#' @return A data frame containing the votes for each candidate in a county.
#'
#' @examples
#' vote_counts("meae.congressional.congress08.md.county")
#'
#' @export
vote_counts <- function(map_id) {
  stopifnot(is.character(map_id),
            length(map_id) == 1)

  map <- meae_maps %>%
    dplyr::filter(meae_id == map_id)

  stopifnot(nrow(map) == 1)

  elections <- map %>%
    dplyr::left_join(meae_maps_to_elections, by = "meae_id") %>%
    dplyr::left_join(meae_elections, by = "election_id")

  stopifnot(nrow(elections) > 0)

  if (map$type == "congressional" && map$geography == "county") {

    votes <- elections %>%
      dplyr::left_join(meae_congressional_counties, by = "election_id")

  } else {

    stop("Unable to get election returns for that map ID")

  }

  votes %>%
    dplyr::select(-congress.y,
                  -state.x,
                  -state.y) %>%
    dplyr::rename(congress = congress.x)

}

#' Aggregate votes for a geographic unity by political party
#'
#' @param data A data frame returned by \code{\link{vote_counts}}.
#' @param geography The geography to aggregate by for mapping. For instance,
#'   \code{"county"}.
#' @param parties The parties to explicitly keep by name. All other parties,
#'   along with unaffiliated candidates, will be collapsed into \code{"other"}.
#'
#' @return A data frame with one row for each geographic unit (e.g. for each
#'   county) in the data frame. There will be columns for each party, giving
#'   their votes recorded and vote percentage, along with the total vote in that
#'   geographic unit.
#'
#' @examples
#' votes <- vote_counts("meae.congressional.congress05.ny.county")
#' aggregate_party_votes(votes)
#'
#' @export
aggregate_party_votes <- function(data, geography = c("county"),
                                  parties = c("Federalist", "Republican")) {

  stopifnot(is.data.frame(data))
  geography <- match.arg(geography)

  # Get just the relevant parties
  data <- data %>%
    dplyr::mutate(party = dplyr::if_else(!party %in% parties, "Other", party))

  if (geography == "county")  {
    aggregates <- data %>%
      dplyr::group_by(county_ahcb, party) %>%
      dplyr::summarize(party_vote = sum(vote, na.rm = TRUE)) %>%
      dplyr::group_by(county_ahcb) %>%
      dplyr::mutate(county_vote = sum(party_vote, na.rm = TRUE),
             party_percentage = round(party_vote / county_vote, 3))

    votes <- aggregates %>%
      dplyr::select(county_ahcb, party, party_vote) %>%
      tidyr::spread(party, party_vote, fill = 0) %>%
      append_colnames("_vote")

    county_vote <- aggregates %>%
      dplyr::ungroup() %>%
      dplyr::distinct(county_ahcb, county_vote)

    percentages <- aggregates %>%
      dplyr::select(county_ahcb, party, party_percentage) %>%
      tidyr::spread(party, party_percentage, fill = 0) %>%
      append_colnames("_percentage")

    output <- votes %>%
      dplyr::left_join(county_vote, by = "county_ahcb") %>%
      dplyr::left_join(percentages, by = "county_ahcb")

  } else {
    stop("Unable to aggregate election returns for that type of geography")
  }

  output

}
