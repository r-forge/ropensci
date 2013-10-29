context("country_codes")

test_that("returns the correct class", {
  expect_is(country_codes(country_name="United"), "data.frame")
  expect_is(country_codes(country_name="the", fuzzy=TRUE), "data.frame")
})

test_that("returns the correct value", {
  expect_equal(as.character(country_codes(country_name="United States")[2,"name"]), 
               "United States")
  expect_equal(as.character(country_codes(country_name="the", 
                                          fuzzy=TRUE)[2,"name"]), 
               "Saint Barthalemy")
})