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

  map_data <- meae_maps %>%
    dplyr::filter(meae_id == map_id)

  stopifnot(nrow(map_data) == 1)

  elections <- map_data %>%
    dplyr::left_join(meae_maps_to_elections, by = "meae_id") %>%
    dplyr::left_join(meae_elections, by = "election_id")

  stopifnot(nrow(elections) > 0)

  if (map_data$type == "congressional" && map_data$geography == "county") {

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

  years <- unique(data$year)
  if (length(years) > 1) {
    warning("These elections happened over more than one year, from ",
            range(data$year, na.rm = TRUE)[1], " to ",
            range(data$year, na.rm = TRUE)[2], ". Using the year ",
            most_common_year(data$year), ", which is the most common year.")
  }
  year <- most_common_year(data$year)

  meae_id <- unique(data$meae_id)
  type <- unique(data$type)
  congress <- unique(data$congress)
  office <- unique(data$election_office)

  # Get just the relevant parties
  data <- data %>%
    dplyr::mutate(party = dplyr::if_else(!party %in% parties, "Other", party))

  if (geography == "county")  {
    aggregates <- data %>%
      dplyr::group_by(state, county_ahcb, county_fips, party) %>%
      dplyr::summarize(party_vote = sum(vote, na.rm = TRUE)) %>%
      dplyr::group_by(state, county_ahcb, county_fips) %>%
      dplyr::mutate(county_vote = sum(party_vote, na.rm = TRUE),
             party_percentage = round(party_vote / county_vote, 3))

    votes <- aggregates %>%
      dplyr::select(state, county_ahcb, county_fips, party, party_vote) %>%
      tidyr::spread(party, party_vote, fill = 0) %>%
      append_colnames("_vote")

    county_vote <- aggregates %>%
      dplyr::ungroup() %>%
      dplyr::distinct(state, county_ahcb, county_fips, county_vote)

    percentages <- aggregates %>%
      dplyr::select(state, county_ahcb, county_fips, party, party_percentage) %>%
      tidyr::spread(party, party_percentage, fill = 0) %>%
      append_colnames("_percentage")

    output <- votes %>%
      dplyr::left_join(county_vote, by = c("state", "county_ahcb", "county_fips")) %>%
      dplyr::left_join(percentages, by = c("state", "county_ahcb", "county_fips"))

  } else {
    stop("Unable to aggregate election returns for that type of geography")
  }

  output <- output %>%
    dplyr::ungroup()

  # Guarantee that `other` and party names requested will appear, even if they
  # received no votes.
  output <- guarantee_colnames(output, parties = parties)

  # Add basic metadata back in
  output <- output %>%
    dplyr::mutate(meae_id = meae_id,
                  type = type,
                  office = office,
                  congress = congress,
                  year = year)

  output

}

#' Join aggregated party data to spatial data
#'
#' @param data A data frame returned by \code{\link{aggregate_party_votes}}.
#' @param geography The geography to aggregate by for mapping. For instance,
#'   \code{"county"}.
#' @param resolution Use \code{"high"} or \code{"low"} resolution geographic
#'   data.
#' @param date Override the date for the geographies used in this map. Otherwise
#'   the appropriate date will be guessed from the data passed in.
#'
#' @return A \code{sf} object with polygons for the appropriate geographies and
#'   the data joined to them.
#'
#' @examples
#' votes <- vote_counts("meae.congressional.congress05.ny.county")
#' aggregates <- aggregate_party_votes(votes)
#' join_to_spatial(aggregates, resolution = "low")
#'
#' @export
join_to_spatial <- function(data, geography = c("county"),
                            resolution = c("high", "low"),
                            date = NULL) {

  stopifnot(is.data.frame(data))
  stopifnot(is.null(date) || is.character(date) || inherits(date, "Date"))
  geography <- match.arg(geography)
  resolution <- match.arg(resolution)

  states <- data %>%
    dplyr::distinct(state) %>%
    dplyr::filter(!is.na(state)) %>%
    dplyr::left_join(USAboundaries::state_codes, by = c("state" = "state_abbr"))

  years <- unique(data$year)
  if (length(years) > 1) {
    warning("These elections happened over more than one year, from ",
            range(data$year, na.rm = TRUE)[1], " to ",
            range(data$year, na.rm = TRUE)[2], ". Using the year ",
            most_common_year(data$year), ", which is the most common year.")
  }
  year <- most_common_year(data$year)

  if (is.null(date)) { date <- as.Date(paste0(year, "-12-31")) }

  spatial <- USAboundaries::us_counties(map_date = date,
                                        resolution = resolution,
                                        states = states$state_name) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(id = as.character(id))

  dplyr::left_join(spatial, data, by = c("id" = "county_ahcb")) %>%
    dplyr::mutate(map_date = date)

}
