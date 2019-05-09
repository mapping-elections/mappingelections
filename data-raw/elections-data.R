# Convert the elections data from the git submodule to the format for the R
# package

suppressPackageStartupMessages(library(tidyverse))

meae_candidates <- read_csv("data-raw/elections-data/candidates.csv",
                            col_types = cols(
                                candidate_id = col_character(),
                                candidate_name = col_character(),
                                congbio_id = col_character(),
                                congbio_name = col_character(),
                                congbio_birthyear = col_integer(),
                                congbio_deathyear = col_integer(),
                                congbio_url = col_character()
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
                                          vote = col_integer()
                                        ))

meae_congress_counties_parties <-
  read_csv("data-raw/elections-data/congressional-counties-parties.csv",
           col_types = cols(
             meae_id = col_character(),
             county_ahcb = col_character(),
             county_fips = col_character(),
             districts = col_character(),
             federalist_vote = col_integer(),
             federalist_percentage = col_double(),
             antifederalist_vote = col_integer(),
             antifederalist_percentage = col_double(),
             demrep_vote = col_integer(),
             demrep_percentage = col_double(),
             chesapeake_vote = col_integer(),
             chesapeake_percentage = col_double(),
             potomac_vote = col_integer(),
             potomac_percentage = col_double(),
             repfac_vote = col_integer(),
             repfac_percentage = col_double(),
             adamsclay_vote = col_integer(),
             adamsclay_percentage = col_double(),
             jacksonian_vote = col_integer(),
             jacksonian_percentage = col_double(),
             anticaucus_vote = col_integer(),
             anticaucus_percentage = col_double(),
             caucus_vote = col_integer(),
             caucus_percentage = col_double(),
             other_vote = col_integer(),
             other_percentage = col_double(),
             county_source = col_character()
           ))

meae_congress_candidate_totals <-
  read_csv("data-raw/elections-data/congressional-candidate-totals.csv",
           col_types = cols(
             meae_id = col_character(),
             election_id = col_character(),
             candidate = col_character(),
             candidate_id = col_character(),
             district = col_character(),
             party = col_character(),
             vote = col_integer(),
             total_vote = col_integer(),
             percent_vote = col_double(),
             winner = col_logical(),
             unopposed = col_logical()
           ))

meae_congressional_elections_dates <-
  read_csv("data-raw/elections-data/congressional-elections-dates.csv",
           col_types = cols(
             congress = col_integer(),
             state = col_character(),
             meae_id = col_character(),
             map_date = col_date()
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

nnv_name_authorities <- read_csv("data-raw/elections-data/nnv-name-authorities.csv",
                                 col_types = cols(
                                     candidate_id = col_character(),
                                     candidate_name = col_character()
                                   ))

meae_congbio_elected <- read_csv("data-raw/elections-data/congbio_elected.csv",
                                 col_types = cols(
                                   congress = col_integer(),
                                   state = col_character(),
                                   district = col_integer(),
                                   congbio_position = col_character(),
                                   congbio_id = col_character(),
                                   candidate_id = col_character(),
                                   meae_id = col_character()
                                 ))

meae_staterepresentative_counties_parties <-
  read_csv("data-raw/elections-data/staterepresentative-counties-parties.csv",
           col_types = cols(
             meae_id = col_character(),
             county_ahcb = col_character(),
             county_fips = col_character(),
             districts = col_character(),
             federalist_vote = col_integer(),
             federalist_percentage = col_double(),
             antifederalist_vote = col_integer(),
             antifederalist_percentage = col_double(),
             demrep_vote = col_integer(),
             demrep_percentage = col_double(),
             chesapeake_vote = col_integer(),
             chesapeake_percentage = col_double(),
             potomac_vote = col_integer(),
             potomac_percentage = col_double(),
             repfac_vote = col_integer(),
             repfac_percentage = col_double(),
             adamsclay_vote = col_integer(),
             adamsclay_percentage = col_double(),
             jacksonian_vote = col_integer(),
             jacksonian_percentage = col_double(),
             anticaucus_vote = col_integer(),
             anticaucus_percentage = col_double(),
             caucus_vote = col_integer(),
             caucus_percentage = col_double(),
             other_vote = col_integer(),
             other_percentage = col_double(),
             county_source = col_character()
           ))

meae_staterepresentative_candidate_totals <-
  read_csv("data-raw/elections-data/staterepresentative-candidate-totals.csv",
           col_types = cols(
             meae_id = col_character(),
             election_id = col_character(),
             candidate = col_character(),
             candidate_id = col_character(),
             district = col_character(),
             party = col_character(),
             vote = col_integer(),
             total_vote = col_integer(),
             percent_vote = col_double(),
             winner = col_logical(),
             unopposed = col_logical()
           ))

usethis::use_data(meae_candidates, meae_congressional_counties, meae_elections,
                  meae_maps_to_elections, meae_maps,
                  meae_congress_counties_parties, nnv_name_authorities,
                  meae_congress_candidate_totals,
                  meae_congressional_elections_dates,
                  meae_congbio_elected,
                  meae_staterepresentative_counties_parties,
                  meae_staterepresentative_candidate_totals,  
                  overwrite = TRUE)

