#' Parse an election XML file from NNV
#'
#' Given the path to an NNV file with election results in NNV, return the totals
#' by geography for each candidate.
#'
#' @param file Path to the XML file.
#'
#' @examples
#' file1 <- "~/dev/mapping-elections/nnv-xml/New York/1789/ny.uscongress1.1789.xml"
#' file2 <- "~/dev/mapping-elections/nnv-xml/Virginia/1789/va.uscongress.7.1789.xml"
#' file3 <- "~/dev/mapping-elections/nnv-xml/Tennessee/1801/tn.congress.1801.xml"
#' parse_nnv_xml(file1)
#' parse_nnv_xml(file2)
#' parse_nnv_xml(file3)
#' @import xml2
#' @export
parse_nnv_xml <- function(file) {
  stopifnot(file.exists(file))
  input <- read_xml(file)
  ns <- xml_ns(input)

  election_id <- input %>% xml_attr("election_id")

  candidates_xml <- input %>%
    xml_find_first(".//d1:ballot", ns) %>%
    xml_children()

  candidate_num = xml_attr(candidates_xml, "candidate_num")
  candidate_id = xml_attr(candidates_xml, "name_id")
  candidate_name = xml_attr(candidates_xml, "name")
  affiliation_party = xml_attr(candidates_xml, "affiliation")
  affiliation_id = xml_attr(candidates_xml, "affiliation_id")

  replace_null <- function(x) {
    dplyr::if_else(x == "null", NA_character_, x)
  }

  candidates <- dplyr::data_frame(election_id, candidate_num, candidate_name,
                                  candidate_id, affiliation_party,
                                  affiliation_id) %>%
    dplyr::mutate_if(is.character, replace_null)

  overview_xml <- input %>%
    xml_find_first(".//d1:overview", ns) %>%
    xml_find_all(".//d1:candidate_summary", ns)

  overview <- dplyr::data_frame(
    candidate_num = xml_attr(overview_xml, "candidate_ref"),
    overview = xml_attr(overview_xml, "vote_total") %>% as.integer()
  )

  sub_units <- input %>%
    xml_find_all(".//d1:sub_unit")

  results_from_sub_units <- function(sub_unit) {
    res <- sub_unit %>%
      xml_find_all("./d1:result") # just the immediate children

    geography_type <- sub_unit %>% xml_attr("type") %>% tolower()
    geography_type[geography_type == "city"] <- "town"
    geography_name <- sub_unit %>% xml_attr("name")

    if (length(res) == 0) {
      return(data_frame(geography_name = geography_name,
                        geography_type = geography_type,
                        candidate_num = candidate_num,
                        votes = NA_integer_))
    }

    candidate_num <- res %>% xml_attr("candidate_ref")
    votes <- res %>% xml_attr("vote") %>% as.integer()

    dplyr::data_frame(geography_name, geography_type, candidate_num, votes)
  }

  if (length(sub_units) == 0 ) {
    votes_by_unit <- data_frame(geography_name = NA_character_,
                                geography_type = NA_character_,
                                candidate_num = candidate_num,
                                votes = NA_integer_)

  } else {
    votes_by_unit <- sub_units %>% purrr::map_df(results_from_sub_units)
  }

  vote_totals <- votes_by_unit %>%
    dplyr::group_by(geography_type, candidate_num) %>%
    dplyr::summarize(votes = sum(votes, na.rm = TRUE)) %>%
    tidyr::spread(geography_type, votes)

  out <- candidates %>%
    dplyr::left_join(vote_totals, by = "candidate_num") %>%
    dplyr::left_join(overview, by = "candidate_num") %>%
    select(-candidate_num)

  if ("<NA>" %in% colnames(out)) out <- out %>% select(-`<NA>`)

  out
}