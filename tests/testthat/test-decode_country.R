context("test-decode_country.R")

test_that(".decode_country", {
  expect_equal(.decode_country("Australia"),
               "AUS")
  expect_equal(.decode_country(c("Australia", "United Kingdom")),
               c("AUS", "GBR"))
})
