#' Generate the YAML for each Congressional election map
#'
#' @param meae_id A unique Mapping Early American Elections id
#'
#' @export
generate_map_yaml <- function(meae_id, legend = "fed-vs-repub-percentage",
                              always_allow_html = "true", layout = "maps-show"){


    md_doc <- list(variant = "markdown")
    html_doc <- list(theme = "default")
    docs_output <- list(md_document = md_doc, html_document = html_doc)
    final_docs <- list(output = docs_output)


    basic_info <- get_general_info(meae_id)
    caption_title <- get_title_caption(basic_info)
    related_elections <- get_related_elections(meae_id)


    output <- c(basic_info, caption_title, related_elections, final_docs)
    output
}


get_general_info <- function(meae_id){
  election_info <- meae_maps[meae_maps$meae_id == meae_id, ]
  state_name <- tolower(as.character(state_codes[state_codes$state_abbr==election_info$state, 1]))

  output <- list(meae_id = election_info$meae_id, type = election_info$type,
                 level = election_info$level, state = state_name,
                 geography = election_info$geography, congressnum = election_info$congress)

}

get_title_caption <- function(basic_info){
  ordinal <- as.character(congress_numbering[congress_numbering$number == basic_info$congressnum, 2])
  elect_year <- meae_maps_to_elections[meae_maps_to_elections$meae_id == basic_info$meae_id, 2]
  election_year <- as.character(meae_elections[meae_elections$election_id == as.character(elect_year[1]), 6])

  map_title <- paste0("Elections for ", proper_case(ordinal), " Congress in ",
                     proper_case(basic_info$state), ", ", election_year)
  caption <- "This is the brief description of the contents of the map."

  output <- list(title = map_title, caption = caption)
}

proper_case <- function(x){
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

get_related_elections <- function(meae_id){

  counter <- 1
  election_list <- list()
  rel_elect <- meae_maps_to_elections[meae_maps_to_elections$meae_id==meae_id, 2]

  for(i in rel_elect){

    election_info <- meae_elections[meae_elections$election_id == rel_elect[counter], ]

    name_desc <- paste0(election_info$state, " US Congress ", election_info$year, " District ", election_info$district)

    election <- list(id = rel_elect[counter], name = name_desc)

    election_list <-  c(election_list, list(election))

    counter <- counter + 1
  }


related_elections <- list(nnv = election_list)


}