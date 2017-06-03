append_colnames <- function(data, append) {
  stopifnot(is.data.frame(data))
  replacements <- paste0(colnames(data), append)
  replacements[1] <- colnames(data)[1]
  colnames(data) <- tolower(replacements)
  data
}
