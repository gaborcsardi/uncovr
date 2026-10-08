test_that("diff() with no code lines in the changed lines", {
  lines <- data.frame(
    lines = c("x <- 1", "# comment", "y <- 2"),
    status = c("instrumented", "noncode", "instrumented"),
    id = c(1L, NA_integer_, 2L),
    coverage = c(1L, NA_integer_, 0L)
  )
  funs <- data.frame(line1 = integer(), coverage = integer())
  coverage <- data.frame(
    path = c("R/a.R", "R/b.R"),
    code_lines = c(2L, 2L),
    lines_covered = c(1L, 1L),
    total_hits = c(1, 1),
    percent_covered = c(50, 50),
    function_count = c(0L, 0L),
    functions_hit = c(0L, 0L)
  )
  coverage$line_count <- c(3L, 3L)
  coverage$lines <- I(list(lines, lines))
  coverage$funs <- I(list(funs, funs))
  coverage$uncovered <- I(list(list(3L), list(3L)))
  flt <- I(empty_data_frame(nrow = 2))
  # only the comment line changed in R/b.R
  flt$diff <- list(list(3L), list(2L))
  coverage$filters <- flt

  d <- diff("diff", coverage = coverage)
  expect_equal(d$code_lines, c(1L, 0L))
  expect_equal(d$percent_covered, c(0, 100))
  expect_false(anyNA(format(d)))
})
