context("Generate Map Metadata")

test_that("Incorrect mapping election id will throw error", {
  expect_error(generate_map_metadata())
  expect_error(generate_map_metadata("Virginia 3rd Congressional election"))
  expect_error(generate_map_metadata(meae_id = "ma.uscongres.york.1810"))
  expect_error(generate_map_metadata(meae_id = "meae.congressional.congress20.ny.county"))
})


test_that("Metadata output matches reference .rds", {
  expect_equal_to_reference(generate_map_metadata(meae_id = "meae.congressional.congress11.ny.county"), "map-metadata.rds")
})