append_colnames <- function(data, append) {
  stopifnot(is.data.frame(data))
  replacements <- paste0(colnames(data), append)
  # Only fix the data columns, not the state or county columns
  tofix <- colnames(data) %in% c("state", "county_ahcb", "county_fips")
  replacements[tofix] <- colnames(data)[tofix]
  colnames(data) <- tolower(replacements)
  data
}

guarantee_colnames <- function(data, parties) {
  parties <- tolower(unique(c(parties, "Other")))
  for (party in parties) {
    if (!any(paste0(party, "_vote") %in% colnames(data))) {
      data[[paste0(party, "_vote")]] <- 0
      data[[paste0(party, "_percentage")]] <- 0
    }
  }
  data
}

most_common_year <- function(years) {
  as.integer(names(sort(table(stats::na.omit(years)), decreasing = TRUE))[1])
}

most_common_state <- function(years) {
  names(sort(table(stats::na.omit(years)), decreasing = TRUE))[1]
}

replace_with_zero <- function(x) { ifelse(is.na(x), 0, x) }

link_to_congbio <- function(text, url) {
  out <- paste0("<a href='", url, "'>", text, "</a>")
  out[is.na(url)] <- text[is.na(url)]
  out
}
