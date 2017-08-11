#' @export
tabulate_results <- function(votes) {
  stopifnot(is.data.frame(votes))
  knitr::kable(votes)
}