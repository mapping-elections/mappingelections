library(tidyverse)

congress_numbering <- read_csv("data-raw/congress_numbering.csv")

devtools::use_data(congress_numbering, internal = TRUE, overwrite = TRUE,
                   compress = "xz")