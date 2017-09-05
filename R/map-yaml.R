#' Generate the YAML for each Congressional election map
#'
#' @param meae_id A unique Mapping Early American Elections id.
#' @param legend_type A description of the data visualization. Default set to fed-vs-reub-percentage.
#' @param always_allow_html Default to TRUE.
#' @param layout_type Type of html layout. Default set to maps-show.
#'
#' @return A list of all the needed metadata for rendering the maps in Jekyll
#'
#' @examples
#' generate_map_metadata(meae_id = "meae.congressional.congress05.ny.county")
#' @export
generate_map_metadata <- function(meae_id, legend_type = "fed-vs-repub-percentage",
                              always_allow_html = TRUE, layout_type = "maps-show") {

  meae_id <- tolower(meae_id)
  stopifnot(meae_id %in% unique(meae_maps$meae_id))

  md_doc <- list(variant = "markdown")
  html_doc <- list(theme = "default")
  docs_output <- list(md_document = md_doc, html_document = html_doc)
  final_docs <- list(output = docs_output)

  legend_type <- list(legend_type = legend_type)
  always_allow_html <- list(always_allow_html = always_allow_html)
  layout_type <- list(layout = layout_type)


  basic_info <- get_general_info(meae_id)
  title <- get_title(basic_info)
  related_elections <- get_related_elections(meae_id)
  connected_maps <- get_related_maps(meae_id)


  output <- c(basic_info, title, legend_type, related_elections, connected_maps,
                final_docs, always_allow_html, layout_type)
  output
}


get_general_info <- function(meae_id){
  election_info <- meae_maps[meae_maps$meae_id == meae_id, ]
  state_name <- tolower(as.character(state_codes[state_codes$state_abbr==election_info$state, 1]))

  output <- list(`meae-id` = election_info$meae_id, type = election_info$type,
                 level = election_info$level, state = state_name,
                 geography = election_info$geography, congressnum = election_info$congress)

  stopifnot(!is.null(output$`meae-id`) & !is.null(output$type) & !is.null(output$level) &
              !is.null(output$state) & !is.null(output$geography) & !is.null(output$congressnum))
  output
}

get_title <- function(basic_info){
  stopifnot(!is.null(basic_info))

  # ordinal <- as.character(congress_numbering[congress_numbering$number == basic_info$congressnum, 2])
  ordinal <- as.character(congress_numbering[congress_numbering$number == basic_info$congressnum, "ordinal_abbr"])
  elect_year <- meae_maps_to_elections[meae_maps_to_elections$meae_id == basic_info$`meae-id`, 2]
  election_year <- as.character(meae_elections[meae_elections$election_id == as.character(elect_year[[1]][1]), 6])

  map_title <- paste0(proper_case(ordinal), " Congress: ",
                     proper_case(basic_info$state), " ", election_year)
  output <- list(title = map_title)

  stopifnot(!is.null(output$title))
  output
}

# Need to order the districts numerically
get_related_elections <- function(meae_id){

  rel_elect <- meae_maps_to_elections[meae_maps_to_elections$meae_id == meae_id, 2]
  election_list <- list()

  if(nrow(rel_elect) == 1){
    election_info <- meae_elections[meae_elections$election_id == rel_elect[[1]][1], ]

    name_desc <- paste0(election_info$state, " US Congress ", election_info$year, " At Large")
    election <- list(id = rel_elect[[1]][1], name = name_desc)
    election_list <- election
  } else {
  for (i in 1:nrow(rel_elect)) {
    election_info <- meae_elections[meae_elections$election_id == rel_elect[[1]][i], ]
    name_desc <- paste0(election_info$state, " US Congress ", election_info$year,
                        " District ", election_info$district)

    election <- list(id = rel_elect[[1]][i], name = name_desc)
    election_list <- c(election_list, list(election))
  }
  }

  # Sort the list by district
  district_num <- vapply(election_list, function(item) {
    as.integer(stringr::str_extract(item$name, "\\d+$"))
  }, integer(1))
  election_list <- election_list[order(district_num)]

  related_elections <- list(nnv = election_list)

  stopifnot(!is.null(election_list))
  related_elections
}

get_related_maps <- function(meae_id){

  congress_num <- as.integer(gsub("\\D", "", meae_id))
  mapping_id <- strsplit(meae_id, "[.]")

  prev_map <- previous_map(mapping_id, congress_num)
  nex_map <- next_map(mapping_id, congress_num)
  nat_map <- national_map(mapping_id, congress_num)

  maps_list <- c(list(prev_map), list(nex_map), list(nat_map))

  related_maps_list <- list(`related-maps` = maps_list)
}

previous_map <- function(mapping_id, congress_num){
  # stopifnot(!is.null(meae_id) & !is.null(congress_num))

  if (congress_num >= 2) {
    if (congress_num <= 9) {
      previous_congress <- paste0("0", as.character(congress_num - 1))
    } else {
      previous_congress <- as.character(congress_num - 1)
    }

  previous_id <- paste0(mapping_id[[1]][1], ".", mapping_id[[1]][2], ".", "congress", previous_congress, ".",
                      mapping_id[[1]][4], ".", mapping_id[[1]][5])

  stopifnot(previous_id %in% unique(meae_maps$meae_id))

  previous_ordinal <- as.character(congress_numbering[congress_numbering$number == congress_num - 1, 2])
  previous_name <- paste(toupper(mapping_id[[1]][4]), proper_case(previous_ordinal), "Congress")

  previous_type <- "previous"
  previous_info <- list(id = previous_id, name = previous_name, type = previous_type)

  stopifnot(!is.null(previous_info$id) & !is.null(previous_info$name) & !is.null(previous_info$type))
  } else {
    previous_info <- ""
  }
  previous_info
}

next_map <- function(mapping_id, congress_num){
  # stopifnot(!is.null(meae_id) & !is.null(congress_num))

  if (congress_num <= 18) {
    if (congress_num <= 9) {
      next_congress <- paste0("0", as.character(congress_num + 1))
    } else {
      next_congress <- as.character(congress_num + 1)
    }

  next_id <- paste0(mapping_id[[1]][1], ".", mapping_id[[1]][2], ".", "congress", next_congress, ".",
                    mapping_id[[1]][4], ".", mapping_id[[1]][5])

  stopifnot(next_id %in% unique(meae_maps$meae_id))

  next_ordinal <- as.character(congress_numbering[congress_numbering$number == congress_num + 1, 2])
  next_name <- paste(toupper(mapping_id[[1]][4]), proper_case(next_ordinal), "Congress")

  next_type <- "next"
  next_info <- list(id = next_id, name = next_name, type = next_type)

  stopifnot(!is.null(next_info$id) & !is.null(next_info$name) & !is.null(next_info$type))
  } else {
    next_info <- ""
  }
  next_info
}

national_map <- function(mapping_id, congress_num){
  # stopifnot(!is.null(meae_id) & !is.null(congress_num))

  national_id <- paste0(mapping_id[[1]][1], ".", mapping_id[[1]][2], ".", mapping_id[[1]][3], ".national", ".district")
  national_ordinal <- as.character(congress_numbering[congress_numbering$number == congress_num, 2])
  national_name <- paste("National", proper_case(national_ordinal), "Congress")

  national_type <- "national"
  national_info <- list(id = national_id, name = national_name, type = national_type)

  stopifnot(!is.null(national_info$id) & !is.null(national_info$name) & !is.null(national_info$type))
  national_info
}

proper_case <- function(x){
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse=" ")
}