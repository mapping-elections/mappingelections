context("Helpers")

test_that("can create a Leaflet CRS", {
  sc <- make_leaflet_crs("SC")
  expect_is(sc, "leaflet_crs")
  expect_equal(sc$code, "ESRI:32133")
})
