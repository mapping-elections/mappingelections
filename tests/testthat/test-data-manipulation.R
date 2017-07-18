context("Data manipulation")

test_that("data manipulation returns a data frame", {
  expect_is(vote_counts("meae.congressional.congress06.ma.county"), "data.frame")
})