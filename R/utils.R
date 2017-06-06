append_colnames <- function(data, append) {
  stopifnot(is.data.frame(data))
  replacements <- paste0(colnames(data), append)
  # Only fix the data columns, not the state or county columns
  tofix <- colnames(data) %in% c("state", "county_ahcb", "county_fips")
  replacements[tofix] <- colnames(data)[tofix]
  colnames(data) <- tolower(replacements)
  data
}

most_common_year <- function(years) {
  as.integer(names(sort(table(years), decreasing = TRUE))[1])
}