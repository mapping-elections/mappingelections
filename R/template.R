#' Make a map page for the Jekyll site
#'
#' This function generates an R Markdown page which in turn generates the map
#' view page in the Jekyll website. The R Markdown file will be created in the
#' directory passed to the function (presumably the \code{_maps}) directory in
#' Jekyll). It contains the YAML metadata header plus the R code that creates
#' the maps and tables. This file will need to be edited by hand to include text
#' and captions, so by default this function will not overwrite the created
#' file. The R Markdown file has to be run separately to create the Markdown
#' file used by Jekyll.
#'
#' @param meae_id The MEAE ID of the map to be created.
#' @param ... Additional arguments passed on to
#'   \code{\link{generate_map_metadata}}.
#' @param dir The directory in which to create R Markdown file. The file will be
#'   named after the \code{meae_id}.
#' @param overwrite If the file already exists, should it be overwritten?
#' @return Invisibly returns the path to the file that was created.
#' @examples
#' meae_id <- "meae.congressional.congress08.va.county"
#' # The function invisibly returns the path to the file it created
#' file <- make_map_page(meae_id, dir = tempdir(), overwrite = TRUE)
#' cat(readr::read_file(file))
#' @export
make_map_page <- function(meae_id, ..., dir, overwrite = FALSE) {
  metadata <- generate_map_metadata(meae_id, ...) %>% yaml::as.yaml()
  template <- readLines(system.file("template/map-page.Rmd",
                                    package = "mappingelections"))
  rmd <- whisker::whisker.render(template,
                                 list(meae_id = meae_id,
                                      metadata = metadata))
  stopifnot(dir.exists(dir))
  out_file <- file.path(dir, paste0(meae_id, ".Rmd"))
  if (!file.exists(out_file) || overwrite) {
    readr::write_file(rmd, out_file)
    message("Successfully created ", out_file)
  } else {
    warning("Not overwriting ", out_file)
  }
  invisible(rmd)
}
