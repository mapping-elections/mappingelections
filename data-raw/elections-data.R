# Convert the elections data from the git submodule to the format for the R
# package

suppressPackageStartupMessages(library(tidyverse))

meae_candidates <- read_csv("data-raw/elections-data/candidates.csv",
                            col_types = cols(
                              candidate_id = col_character(),
                              candidate = col_character()
                            ))

meae_elections <- read_csv("data-raw/elections-data/elections.csv",
                           col_types = cols(
                             election_id = col_character(),
                             election_office = col_character(),
                             state = col_character(),
                             congress = col_integer(),
                             district = col_integer(),
                             year = col_integer(),
                             election_type = col_character()
                           ))

meae_congressional_counties <- read_csv("data-raw/elections-data/congressional-counties.csv",
                                        col_types = cols(
                                          election_id = col_character(),
                                          county = col_character(),
                                          state = col_character(),
                                          county_ahcb = col_character(),
                                          county_fips = col_integer(),
                                          candidate = col_character(),
                                          candidate_id = col_character(),
                                          party = col_character(),
                                          party_id = col_character(),
                                          vote = col_integer()
                                        ))

meae_maps_to_elections <- read_csv("data-raw/elections-data/maps-to-elections.csv",
                                   col_types = cols(
                                     meae_id = col_character(),
                                     election_id = col_character())
                                   )

meae_maps <- read_csv("data-raw/elections-data/maps.csv",
                                   col_types = cols(
                                     meae_id = col_character(),
                                     type = col_character(),
                                     congress = col_integer(),
                                     state = col_character(),
                                     geography = col_character(),
                                     level = col_character())
                                   )

devtools::use_data(meae_candidates, meae_congressional_counties, meae_elections,
                   meae_maps_to_elections, meae_maps,
                   overwrite = TRUE)
