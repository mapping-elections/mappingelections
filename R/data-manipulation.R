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
#' @importFrom dplyr if_else mutate
aggregate_party_votes <- function(data, geography = c("county"),
                                  parties = c("Federalist", "Democratic-Republican")) {

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

  ## Make sure to make data NA if there are no votes
  output <- output %>%
    dplyr::rename(demrep_vote = `democratic-republican_vote`,
           demrep_percentage = `democratic-republican_percentage`) %>%
    dplyr::mutate(federalist_vote = if_else(county_vote == 0, NA_real_, federalist_vote)) %>%
    dplyr::mutate(demrep_vote = if_else(county_vote == 0, NA_real_, demrep_vote)) %>%
    dplyr::mutate(other_vote = if_else(county_vote == 0, NA_real_, other_vote)) %>%
    dplyr::mutate(federalist_percentage = if_else(county_vote == 0, NA_real_, federalist_percentage)) %>%
    dplyr::mutate(other_percentage = if_else(county_vote == 0, NA_real_, other_percentage)) %>%
    dplyr::mutate(demrep_percentage = if_else(county_vote == 0, NA_real_, demrep_percentage))

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
#' @param party_votes A data frame of vote percentages by party.
#' @param elections A data frame containing metadata about the NNV elections
#'   being mapped.
#' @param resolution Use \code{"high"} or \code{"low"} resolution geographic
#'   data.
#' @param election_date The date to use when getting the county boundaries.
#'
#' @return A \code{sf} object with polygons for the appropriate geographies and
#'   the data joined to them.
#'
#' @export
join_to_spatial <- function(party_votes, elections, resolution = c("high", "low"),
                            election_date = NULL) {

  stopifnot(is.data.frame(party_votes))
  stopifnot(is.data.frame(elections))
  resolution <- match.arg(resolution)

  geography <- unique(elections$geography)[1]
  state <- dplyr::data_frame(state_abbr = unique(elections$state_map)) %>%
    dplyr::left_join(USAboundaries::state_codes, by = "state_abbr")
  year <- most_common_year(elections$year)
  state_to_filter <- state$state_name
  if (!any(is.na(state_to_filter)) && state_to_filter == "Massachusetts" && year < 1820) {
    state_to_filter <- c("Massachusetts", "Maine")
  }
  if (any(is.na(state_to_filter))) {
    state_to_filter <- NULL
  }
  if (is.null(election_date)) {
    map_date <- as.Date(paste0(year, "-01-01"))
  } else {
    map_date <- as.Date(election_date)
  }

  spatial <- USAboundaries::us_counties(map_date = map_date,
                                        resolution = resolution,
                                        states = state_to_filter) %>%
    # sf::st_as_sf() %>%
    dplyr::mutate(id = as.character(id))

  dplyr::left_join(spatial, party_votes, by = c("id" = "county_ahcb")) %>%
    dplyr::mutate(map_date = map_date)

}

#' Get the data to map party vote percentages by county state by state
#' @param map_id The ID of the map from \code{meae_maps}.
#' @param election_date The date to be used when getting the county boundaries.
#'
#' @examples
#' get_county_map_data("meae.congressional.congress01.md.county")
#' @export
get_county_map_data <- function(map_id, election_date = NULL) {
  stopifnot(is.character(map_id),
            length(map_id) == 1)
  meae_map <- meae_maps %>% dplyr::filter(meae_id == map_id)
  elections <- meae_map %>%
    dplyr::left_join(meae_maps_to_elections, by = "meae_id") %>%
    dplyr::left_join(meae_elections, by = "election_id", suffix = c("_map", ""))
  party_votes <- meae_map %>%
    dplyr::left_join(meae_congress_counties_parties, by = "meae_id")
  map_data <- join_to_spatial(party_votes, elections, election_date = election_date)
  map_data
}

#' Get the data to map party vote percentages by county for the nation
#' @param map_id The ID of the map from \code{meae_maps}.
#' @examples
#' get_national_map_data("meae.congressional.congress01.national.county")
#' @export
get_national_map_data <- function(map_id) {
  stopifnot(is.character(map_id),
            length(map_id) == 1)
  meae_national_map <- meae_maps %>% dplyr::filter(meae_id == map_id)
  meae_state_maps <- meae_maps %>%
    dplyr::filter(congress == meae_national_map$congress,
                  geography == "county",
                  level == "state") %>%
    dplyr::left_join(meae_congressional_elections_dates, by = c("meae_id", "congress", "state"))
  party_votes <- meae_state_maps %>%
    dplyr::left_join(meae_congress_counties_parties, by = "meae_id")
  elections <- meae_national_map %>%
    dplyr::left_join(meae_maps_to_elections, by = "meae_id") %>%
    dplyr::left_join(meae_elections, by = "election_id", suffix = c("_map", ""))

  geog_combined <- NULL

  for (i in seq_len(nrow(meae_state_maps))) {
    state <- meae_state_maps[[i, "state"]]
    date <- meae_state_maps[[i, "map_date"]]
    # message("State: ", state, ". Date: ", date, ".")
    if (state == "MA" && date <= as.Date("1820-01-01")) state <- c("MA", "ME")
    geog <- USAboundaries::us_counties(date, "high", state)
    if (is.null(geog_combined)) {
      geog_combined <- geog
    } else {
      geog_combined <- rbind(geog_combined, geog)
    }
  }

  map_data <- dplyr::left_join(geog_combined, party_votes, by = c("id" = "county_ahcb"))
  map_data
}

#' Get the spatial data to map party vote percentages by county for state legislative elections
#' @param map_id The ID of the map from \code{meae_maps}.
#' @param state The state that the map should display.
#' @param election_date The date to be used when getting the county boundaries.
#' @return An sf spatial data frame joined to the election data.
#'
#' @examples
#' get_staterep_map_data("meae.staterepresentative.1796.de.county", state = "DE",
#'                       election_date = "1796-10-01")
#' @export
#' @importFrom dplyr starts_with
get_staterep_map_data <- function(map_id, state = NULL, election_date = NULL) {
  stopifnot(is.character(map_id),
            length(map_id) == 1,
            is.character(state),
            length(state) == 1)
  meae_map <- meae_maps %>% dplyr::filter(meae_id == map_id)
  party_votes <- meae_map %>%
    dplyr::left_join(meae_staterepresentative_counties_parties, by = "meae_id") %>%
    dplyr::select(-dplyr::starts_with("X"))

  spatial <- USAboundaries::us_counties(map_date = election_date,
                                        resolution = "high",
                                        states = state) %>%
    dplyr::mutate(id = as.character(id))

  map_data <- dplyr::left_join(spatial, party_votes, by = c("id" = "county_ahcb")) %>%
    dplyr::mutate(map_date = as.Date(election_date))

  map_data
}

