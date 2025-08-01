context("testthatlabs")

test_that("testthatlabs works", {
  for (i in 1:10) {
    expect_true(TRUE)
  }
})

test_that("lorem ipsum", {
  for (i in 1:10) {
    expect_true(TRUE)
  }
  expect_true(FALSE)
  for (i in 1:5) {
    expect_true(TRUE)
  }
})

test_that("voluptate adipisicing", {
  for (i in 1:10) {
    expect_true(TRUE)
  }
  skip("not this time")
})

test_that("eiusmod mollit ad id", {
  expect_true(TRUE)
})
