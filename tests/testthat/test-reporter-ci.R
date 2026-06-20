test_that("format_duration", {
  expect_equal(format_duration(0), "0ms")
  expect_equal(format_duration(0.089), "89ms")
  expect_equal(format_duration(0.999), "999ms")
  expect_equal(format_duration(4.3), "4.3s")
  expect_equal(format_duration(12), "12.0s")
  expect_equal(format_duration(NA_real_), "0ms")
  expect_equal(format_duration(-1), "0ms")
})

test_that("CiReporter renders blocks, skips and failures", {
  withr::local_options(cli.num_colors = 1)

  tf <- withr::local_tempfile(
    fileext = ".R",
    lines = c(
      "test_that('is.na', {",
      "  expect_true(TRUE)",
      "  expect_equal(1, 1)",
      "})",
      "",
      "test_that('on cran', {",
      "  skip('On CRAN')",
      "})",
      "",
      "test_that('a failure', {",
      "  expect_equal(1, 2)",
      "})"
    )
  )
  file.rename(tf, tf2 <- file.path(dirname(tf), "test-checks.R"))

  out <- testthat::capture_output_lines(
    testthat::test_file(tf2, reporter = CiReporter$new(package = "demo"))
  )
  out <- cli::ansi_strip(out)

  expect_true(any(grepl("demo test suite \u2500+$", out)))
  expect_true(any(grepl("\u203a checks .* \u00bb is.na \\.\\. \\[", out)))
  expect_true(any(grepl("^PASS x2  FAIL x1  WARN x0  SKIP x1  \\[", out)))
})

test_that("CiReporter groups consecutive skips", {
  withr::local_options(cli.num_colors = 1)

  tf <- withr::local_tempfile(
    fileext = ".R",
    lines = c(
      "test_that('ok', { expect_true(TRUE) })",
      "test_that('s1', { skip('On CRAN') })",
      "test_that('s2', { skip('On CRAN') })",
      "test_that('s3', { skip('On CRAN') })"
    )
  )

  out <- testthat::capture_output_lines(
    testthat::test_file(tf, reporter = CiReporter$new(package = "demo"))
  )
  out <- cli::ansi_strip(out)

  skip_idx <- grep("^SKIP \u203a ", out)
  expect_length(skip_idx, 3)
  expect_equal(skip_idx, seq(skip_idx[1], by = 1, length.out = 3))
})
