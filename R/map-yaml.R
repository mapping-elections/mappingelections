#' Generate the YAML for each Congressional election map
#'
#' @param meae_id A unique Mapping Early American Elections id.
#' @param legend_type A description of the data visualization. Default set to fed-vs-reub-percentage.
#' @param always_allow_html Default to TRUE.
#' @param layout Type of html layout. Default set to maps-show.
#'
#' @return A list of all the needed metadata for rendering the maps in Jekyll
#'
#' @export
generate_map_yaml <- function(meae_id, legend_type = "fed-vs-repub-percentage",
                              always_allow_html = "true", layout = "maps-show"){

   ## stopifnot( as.character(meae_id),
   ## length(strsplit(meae_id, "[.]")[[1]]) == 5)

    md_doc <- list(variant = "markdown")
    html_doc <- list(theme = "default")
    docs_output <- list(md_document = md_doc, html_document = html_doc)
    final_docs <- list(output = docs_output)


    basic_info <- get_general_info(meae_id)
    caption_title <- get_title_caption(basic_info)
    related_elections <- get_related_elections(meae_id)
    connected_maps <- get_related_maps(meae_id)


    output <- c(basic_info, caption_title, related_elections, connected_maps, final_docs)
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
  election_year <- as.character(meae_elections[meae_elections$election_id == as.character(elect_year[[1]][1]), 6])

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

# Need to order the districts numerically
get_related_elections <- function(meae_id){

  rel_elect <- meae_maps_to_elections[meae_maps_to_elections$meae_id == meae_id, 2]
  election_list <- list()

  if(nrow(rel_elect) == 1){
    election_info <- meae_elections[meae_elections$election_id == rel_elect[[1]][1], ]
    name_desc <- paste0(election_info$state, " US Congress ", election_info$year, " At Large")
    election <- list(id = rel_elect[[1]][i], name = name_desc)
    election_list <- election
  }else {
  for(i in 1:nrow(rel_elect)){

    name_desc <- paste0(election_info$state, " US Congress ", election_info$year, " District ", election_info$district)
    election_info <- meae_elections[meae_elections$election_id == rel_elect[[1]][i], ]

    election <- list(id = rel_elect[[1]][i], name = name_desc)
    election_list <-  c(election_list, list(election))

  }
}

related_elections <- list(nnv = election_list)
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

  if(congress_num >= 2){
    if(congress_num <= 9){
      previous_congress <- paste0("0", as.character(congress_num - 1))
    }else {
      previous_congress <- as.character(congress_num - 1)
    }
    ## Spacing ISSUE

    previous_id <- paste0(mapping_id[[1]][1], ".", mapping_id[[1]][2], ".", "congress", previous_congress, ".",
                      mapping_id[[1]][4], ".", mapping_id[[1]][5])

  previous_ordinal <- as.character(congress_numbering[congress_numbering$number == congress_num - 1, 2])
  previous_name <- paste(toupper(mapping_id[[1]][4]), proper_case(previous_ordinal), "Congress")

  previous_type <- "previous"
  previous_info <- list(id = previous_id, name = previous_name, type = previous_type)
  }else {
    previous_info <- ""
  }
  previous_info
}

next_map <- function(mapping_id, congress_num){
  if(congress_num <= 18){
    if(congress_num <= 9){
      next_congress <- paste0("0", as.character(congress_num + 1))
    }else {
      next_congress <- as.character(congress_num + 1)
    }

  next_id <- paste0(mapping_id[[1]][1], ".", mapping_id[[1]][2], ".", "congress", next_congress, ".",
                    mapping_id[[1]][4], ".", mapping_id[[1]][5])

  next_ordinal <- as.character(congress_numbering[congress_numbering$number == congress_num + 1, 2])
  next_name <- paste(toupper(mapping_id[[1]][4]), proper_case(next_ordinal), "Congress")

  next_type <- "next"

  next_info <- list(id = next_id, name = next_name, type = next_type)
  }else {
    next_info <- ""
  }
  next_info
}

national_map <- function(mapping_id, congress_num){

  national_id <- paste0(mapping_id[[1]][1], ".", mapping_id[[1]][2], ".", mapping_id[[1]][3], ".national", ".district")

  national_ordinal <- as.character(congress_numbering[congress_numbering$number == congress_num, 2])
  national_name <- paste(toupper(mapping_id[[1]][4]), proper_case(national_ordinal), "Congress")

  national_type <- "national"

  national_info <- list(id = national_id, name = national_name, type = national_type)
}