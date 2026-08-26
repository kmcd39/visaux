
rm(list = ls())
devtools::load_all()

test_that("number to formatted string - singles", {


  expect_equal(number.to.formatted.string(1.2e6), "1.2 million")

  expect_equal(number.to.formatted.string(3.5e9), "3.5 billion")

  expect_equal(number.to.formatted.string(125), "1.2 hundred")

  expect_equal(number.to.formatted.string(1234), "1.2 thousand")

})


test_that("number to formatted string - vector w nas", {

  x <- c(10e9, 200e6, NA)

  expect_equal(number.to.formatted.string(x),
               c("10 billion", "200 million", NA))
})


test_that("number to formatted string - only NAs", {
  x <- c(NA, NA)
  suppressWarnings(
  testthat::expect_error(
      number.to.formatted.string(x) ))
})

