#' Get a table of results by candidate and Congressional district
#'
#' @param map_id An ID from the \code{meae_maps} table.
#' @examples
#' map_id <- "meae.congressional.congress03.va.county"
#' candidate_results(map_id)
#' @rdname results-table
#' @importFrom dplyr select
#' @export
candidate_results <- function(map_id) {

  stopifnot(is.character(map_id),
            length(map_id) == 1)

  meae_maps %>%
    dplyr::filter(meae_id == map_id) %>%
    dplyr::left_join(meae_maps_to_elections, by = "meae_id") %>%
    dplyr::left_join(meae_elections, by = "election_id",
                     suffix = c("", "_candidates")) %>%
    select(-district) %>%
    dplyr::left_join(select(meae_congress_candidate_totals, -meae_id), # select should be unnecessary eventually
              by = "election_id")

}

#' Format candidate results in an HTML table
#'
#' @param results A data frame of results from \code{candidate_results}.
#' @param keep_percentage What percentage of votes defines a contender?
#'
#' @importFrom xml2 xml_find_all xml_set_attrs xml_set_attr xml_integer
#'   xml_find_first
#' @export
#' @examples
#' map_id <- "meae.congressional.congress03.va.county"
#' results <- candidate_results(map_id)
#' results_to_table(results)
#' @rdname results-table
results_to_table <- function(results, keep_percentage = 0.05) {

  stopifnot(is.data.frame(results))

  # if (!any(is.na(suppressWarnings(as.integer(results$district))))) {
  #   results$district <- as.integer(results$district)
  # }

  # Get just the contenders. Keep the ones above a certain percentage, but
  # also keep the winners, who sometimes have no votes available.
  results_abbr <- results %>%
    mutate(district = coerce_if(district)) %>%
    dplyr::left_join(meae_candidates, by = "candidate_id") %>%
    dplyr::select(election_id, district, candidate, party, vote, percent_vote,
                  winner, congbio_url, unopposed) %>%
    dplyr::mutate(contender = (percent_vote > keep_percentage) | winner) %>%
    dplyr::mutate(candidate = ifelse(contender, candidate, "Other candidates"),
                  candidate = ifelse(stringr::str_detect(candidate, "scattering"),
                                     "Other candidates", candidate),
                  party = ifelse(contender, party, "")) %>%
    dplyr::group_by(election_id, district, candidate, party, winner, congbio_url) %>%
    dplyr::summarize(vote = sum(vote),
                     percent_vote = sum(percent_vote),
                     unopposed = all(unopposed)) %>%
    dplyr::ungroup() %>%
    dplyr::filter((percent_vote > keep_percentage) | winner) %>%
    dplyr::arrange(district, desc(vote), desc(percent_vote))

  # Format the data frame for display
  formatted_df <- results_abbr %>%
    dplyr::mutate(candidate = link_to_congbio(candidate, congbio_url),
                  percent_vote = stringr::str_c(round(percent_vote * 100, 1), "%"),
                  winner = ifelse(winner, "\u2713", ""),
                  party = ifelse(is.na(party), "", party),
                  vote = prettyNum(vote, big.mark = ","),
                  vote = ifelse(vote == "NA", "", vote),
                  percent_vote = ifelse(is.na(percent_vote), "", percent_vote),
                  percent_vote = ifelse(unopposed, "unopposed", percent_vote)) %>%
    dplyr::select(District = district,
                  Candidate = candidate,
                  Party = party,
                  Vote = vote,
                  Percentage = percent_vote,
                  Elected = winner)

  results_kable <- knitr::kable(formatted_df,
               format = "html",
               align = "cllrrc", escape = FALSE)

  results_xml <- xml2::read_html(as.character(results_kable))

  # Add attributes for the parties
  results_xml %>%
    xml_find_all(".//td[text()=' Federalist ']",) %>%
    xml_set_attrs(c("class" = "party-federalist",
                    "data-party" = "federalist"))

  results_xml %>%
    xml_find_all(".//td[text()=' Democratic-Republican ']",) %>%
    xml_set_attrs(c("class" = "party-demrep",
                    "data-party" = "demrep"))

  # Add attributes for candidates elected
  results_xml %>%
    xml_find_all(".//td[text()=' elected ']",) %>%
    xml_set_attrs(c("class" = "elected",
                    "data-elected" = "true"))

  # Add attributes to group by district
  # Does the district number change?
  district_num <- results_xml %>%
    xml_find_all(".//tr/td[1]") %>%
    xml_text()

  if (!all(stringr::str_detect(district_num, "At-large"))) {
    district_num <- as.integer(district_num)
    district_changed = dplyr::lead(district_num, default = max(district_num) + 1) - district_num > 0
    xml_find_all(results_xml, ".//tbody/tr")[district_changed] %>%
      xml_set_attr("class", "district-changed")
    xml_find_all(results_xml, ".//tbody/tr")[!district_changed] %>%
      xml_set_attr("class", "district-unchanged")
    xml_find_all(results_xml, ".//tbody/tr")[district_num %% 2 == 0] %>%
      xml_set_attr("data-district-type", "even")
    xml_find_all(results_xml, ".//tbody/tr")[district_num %% 2 != 0] %>%
      xml_set_attr("data-district-type", "odd")
  }

  results_xml %>%
    xml_find_first(".//table") %>%
    as.character()

}
