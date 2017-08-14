#' Create table of results for election
#' @examples
#' meae_id <- "meae.congressional.congress08.md.county"
#' votes <- vote_counts(meae_id)
#' results <- aggregate_candidate_votes(votes)
#' results_to_table(results)
#' @export
results_to_table <- function(results) {
  stopifnot(is.data.frame(results))
  formatted_df <- results %>%
    mutate(percent_vote = stringr::str_c(round(percent_vote * 100, 0), "%")) %>%
    mutate(winner = if_else(winner, "elected", "")) %>%
    select(District = district,
           Candidate = candidate,
           Party = party,
           Vote = vote,
           Percentage = percent_vote,
           Result = winner
           )
  knitr::kable(formatted_df,
               format = "html",
               align = "cllrrl",
               format.args = list(big.mark = ",")) %>%
    kableExtra::collapse_rows(columns = 1)
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
