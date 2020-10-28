
context("testthatlabs")

test_that("testthatlabs works", {

  expect_true(TRUE)
  expect_true(FALSE)
})

test_that("this is a skip", {
  skip("not this time")
})

test_that("this is a warning", {
  warning("beware!")
  expect_true(TRUE)
})
