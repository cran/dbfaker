library(dbfaker)
context("Numeric utility validation")

test_that("precision is accurate", {
  expect_equal(precision(123), 3)
  expect_equal(precision(0.123), 0)
  expect_equal(precision(123.123), 3)
  expect_equal(precision(.123), 0)
})

test_that("scale is accurate", {
  expect_equal(scale(123), 0)
  expect_equal(scale(0.123), 3)
  expect_equal(scale(123.123), 3)
  expect_equal(scale(.123), 3)
  expect_equal(scale(.000123), 6)
  expect_equal(scale(.123000), 3)
})
