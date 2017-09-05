#' Create table of results for election
#'
#' @param results The results as returned by
#'   \code{\link{aggregate_candidate_votes}}.
#' @examples
#' meae_id <- "meae.congressional.congress08.md.county"
#' votes <- vote_counts(meae_id)
#' results <- aggregate_candidate_votes(votes)
#' results_to_table(results)
#' @importFrom xml2 xml_find_all xml_set_attrs xml_set_attr xml_integer
#'   xml_find_first
#' @export
results_to_table <- function(results) {

  stopifnot(is.data.frame(results))

  formatted_df <- results %>%
    dplyr::mutate(percent_vote = stringr::str_c(round(percent_vote * 100, 0), "%")) %>%
    dplyr::mutate(winner = ifelse(winner, "✔", "")) %>%
    dplyr::mutate(party = ifelse(is.na(party), "", party)) %>%
    dplyr::select(District = district,
                  Candidate = candidate,
                  Party = party,
                  Vote = vote,
                  Percentage = percent_vote,
                  Elected = winner
                  )
  results_kable <- knitr::kable(formatted_df,
               format = "html",
               align = "cllrrc",
               format.args = list(big.mark = ","))

  results_xml <- xml2::read_html(as.character(results_kable))

  # Add attributes for the parties
  results_xml %>%
    xml_find_all(".//td[text()=' Federalist ']",) %>%
    xml_set_attrs(c("class" = "party-federalist",
                    "data-party" = "federalist"))

  results_xml %>%
    xml_find_all(".//td[text()=' Republican ']",) %>%
    xml_set_attrs(c("class" = "party-republican",
                    "data-party" = "republican"))

  # Add attributes for candidates elected
  results_xml %>%
    xml_find_all(".//td[text()=' elected ']",) %>%
    xml_set_attrs(c("class" = "elected",
                    "data-elected" = "true"))

  # Add attributes to group by district
  # Does the district number change?
  district_num <- results_xml %>%
    xml_find_all(".//tr/td[1]") %>%
    xml_integer()
  district_changed = dplyr::lead(district_num, default = max(district_num) + 1) - district_num > 0
  xml_find_all(results_xml, ".//tbody/tr")[district_changed] %>%
    xml_set_attr("class", "district-changed")
  xml_find_all(results_xml, ".//tbody/tr")[!district_changed] %>%
    xml_set_attr("class", "district-unchanged")
  xml_find_all(results_xml, ".//tbody/tr")[district_num %% 2 == 0] %>%
    xml_set_attr("data-district-type", "even")
  xml_find_all(results_xml, ".//tbody/tr")[district_num %% 2 != 0] %>%
    xml_set_attr("data-district-type", "odd")

  results_xml %>%
    xml_find_first(".//table") %>%
    as.character()

}

#' Create data frame of results of election by state
#'
#' This creates a data frame of results for each Congressional district in a
#' state.
#'
#' @param votes The votes data frame for the elections
#' @param keep_percentage What percentage of the votes should a candidate have
#'   gotten in order to be displayed in the data frame? In othe words, how much
#'   of a contender did the person have to be?
#' @examples
#' meae_id <- "meae.congressional.congress08.md.county"
#' votes <- vote_counts(meae_id)
#' aggregate_candidate_votes(votes)
#' @export
aggregate_candidate_votes <- function(votes, keep_percentage = 0.01) {
  stopifnot(is.data.frame(votes))

  votes %>%
    dplyr::group_by(candidate, election_id, party, congress, district, state) %>%
    dplyr::summarize(vote = sum(vote, na.rm = TRUE)) %>%
    dplyr::arrange(state, congress, district, desc(vote)) %>%
    dplyr::group_by(election_id) %>%
    dplyr::mutate(total_vote = sum(vote)) %>%
    dplyr::mutate(percent_vote = vote / total_vote) %>%
    dplyr::mutate(winner = vote == max(vote)) %>%
    dplyr::filter(percent_vote > keep_percentage) %>%
    dplyr::ungroup()
}
