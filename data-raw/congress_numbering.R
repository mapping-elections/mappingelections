library(tidyverse)

congress_numbering <- read_csv("data-raw/congress_numbering.csv")

devtools::use_data(congress_numbering, overwrite = TRUE, compress = "xz")