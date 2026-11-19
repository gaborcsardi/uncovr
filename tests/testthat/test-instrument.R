cov_instrument_in_tempdir <- function(src_lines, symbol = ".__cov_t") {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  withr::local_dir(dir, .local_envir = parent.frame())
  path <- "f.R"
  writeLines(src_lines, path)
  res <- cov_instrument_file(path, symbol)
  res$instrumented <- readLines(path)
  res
}

has_counter <- function(line, symbol = ".__cov_t") {
  grepl(paste0("`", symbol, "`\\["), line)
}

test_that("excluded expressions get no counter injected", {
  res <- cov_instrument_in_tempdir(c(
    "f <- function(x) {",
    "  a <- 1",
    "  b <- 2 # nocov",
    "  a + b",
    "}"
  ))

  expect_false(has_counter(res$instrumented[3]))
  expect_true(has_counter(res$instrumented[2]))
  expect_true(has_counter(res$instrumented[4]))

  expect_equal(res$lines$status[3], "excluded")
  expect_true(is.na(res$lines$id[3]))
  expect_true(is.na(res$lines$coverage[3]))
})

test_that("`# nocov start` / `# nocov end` blocks get no counters", {
  res <- cov_instrument_in_tempdir(c(
    "f <- function(x) {",
    "  a <- 1",
    "  # nocov start",
    "  b <- 2",
    "  d <- 3",
    "  # nocov end",
    "  a + x",
    "}"
  ))

  expect_false(has_counter(res$instrumented[4]))
  expect_false(has_counter(res$instrumented[5]))
  expect_true(has_counter(res$instrumented[2]))
  expect_true(has_counter(res$instrumented[7]))

  expect_equal(res$lines$status[3:6], rep("excluded", 4))
})

test_that("excluding the head of a multi-line expression excludes the span", {
  res <- cov_instrument_in_tempdir(c(
    "f <- function() {",
    "  d <- c( # nocov",
    "    1,",
    "    2",
    "  )",
    "  d",
    "}"
  ))

  expect_false(has_counter(res$instrumented[2]))
  expect_equal(res$lines$status[2:5], rep("excluded", 4))
  expect_true(all(is.na(res$lines$id[2:5])))
})

test_that("excluding a non-head line of a multi-line expression keeps it", {
  res <- cov_instrument_in_tempdir(c(
    "f <- function() {",
    "  d <- c(",
    "    1,    # nocov",
    "    2",
    "  )",
    "  d",
    "}"
  ))

  expect_true(has_counter(res$instrumented[2]))
  expect_equal(res$lines$status[2], "instrumented")
  expect_equal(res$lines$status[3], "excluded")
  expect_equal(res$lines$status[4], "instrumented")
  expect_equal(res$lines$id[4], 2L)
})

count_counters <- function(line, symbol = ".__cov_t") {
  m <- regmatches(line, gregexpr(paste0("`", symbol, "`\\["), line))
  length(m[[1]])
}

test_that("a function wrapped in `# nocov start`/`end` is dropped", {
  res <- cov_instrument_in_tempdir(c(
    "f <- function(x) {",
    "  x + 1",
    "}",
    "# nocov start",
    "h <- function(y) {",
    "  y + 1",
    "}",
    "# nocov end"
  ))

  expect_equal(nrow(res$funs), 1L)
  expect_equal(res$funs$line1, 1L)
  expect_false(any(vapply(res$instrumented[4:8], has_counter, logical(1))))
})

test_that("counter calls evaluate to NULL", {
  # Otherwise inserting a counter into an empty function body would change
  # the function's return value from NULL to the counter's integer value.
  res <- cov_instrument_in_tempdir(c(
    "f <- function() {",
    "}",
    "g <- function() {",
    "  1",
    "}"
  ))
  src <- paste(res$instrumented, collapse = "\n")
  env <- new.env()
  assign(".__cov_t", .Call(c_cov_make_counter, nrow(res$funs) + nrow(res$lines)), envir = env)
  eval(parse(text = src), envir = env)
  expect_null(env$f())
  expect_equal(env$g(), 1)
})

test_that("a function survives when only its def line is excluded", {
  # The top-level expression counter for line 1 must be dropped (line 1 is
  # excluded), but the function-body counter at `{` should still be injected
  # because the body has surviving instrumented expressions, so the function
  # stays in funres.
  res <- cov_instrument_in_tempdir(c(
    "h <- function(y) { # nocov",
    "  z <- y + 1",
    "  z",
    "}"
  ))

  expect_equal(nrow(res$funs), 1L)
  # Line 1 carries exactly one counter — the function-brace counter — not the
  # top-level expression counter that would normally precede `h <-`.
  expect_equal(count_counters(res$instrumented[1]), 1L)
  expect_false(grepl("^`\\.__cov_t`\\[", res$instrumented[1]))
  expect_true(has_counter(res$instrumented[2]))
})

test_that("col_to_char follows the parser's tab stops", {
  lines <- c("\tx", "a+\tx", "abcdefg+\tx", "\t\tx", "y <- \"\t\"; z")
  for (line in lines) {
    psd <- utils::getParseData(parse(text = line, keep.source = TRUE))
    psd <- psd[psd$terminal, ]
    pos <- col_to_char(line, psd$col1)
    expect_equal(substring(line, pos, pos), substr(psd$text, 1, 1))
  }
  # the column right after the end of the line
  expect_equal(col_to_char("a+\t{", 10L), 5L)
})

test_that("expand_tabs expands to the next tab stop", {
  expect_equal(
    expand_tabs(c("\tx", "a+\tx", "abcdefg+\tx", "no tabs")),
    c(
      paste0(strrep(" ", 8), "x"),
      paste0("a+", strrep(" ", 6), "x"),
      paste0("abcdefg+", strrep(" ", 8), "x"),
      "no tabs"
    )
  )
})

test_that("counters go to the right place after a TAB mid-line", {
  # The TAB is at column 15, so it only takes up two columns, and the
  # function counter goes right after the `{`, after the TAB.
  res <- cov_instrument_in_tempdir(c(
    "f<-function(a,\tb) {",
    "\tif (a) { \ty <- b",
    "\t\ty + 1 }",
    "}"
  ))
  src <- paste(res$instrumented, collapse = "\n")
  env <- new.env()
  assign(
    ".__cov_t",
    .Call(c_cov_make_counter, nrow(res$funs) + nrow(res$lines)),
    envir = env
  )
  eval(parse(text = src), envir = env)
  expect_equal(env$f(TRUE, 1), 2)
  expect_equal(nrow(res$funs), 1L)
  expect_true(has_counter(res$instrumented[2]))
  expect_equal(
    res$lines$lines[1:2],
    c(
      paste0("f<-function(a,", strrep(" ", 2), "b) {"),
      paste0(strrep(" ", 8), "if (a) { ", strrep(" ", 7), "y <- b")
    )
  )
})

test_that("TABs in string literals are kept", {
  res <- cov_instrument_in_tempdir(c(
    "f <- function() {",
    "\tx <- \"a\tb\"",
    "\tx",
    "}"
  ))
  src <- paste(res$instrumented, collapse = "\n")
  env <- new.env()
  assign(
    ".__cov_t",
    .Call(c_cov_make_counter, nrow(res$funs) + nrow(res$lines)),
    envir = env
  )
  eval(parse(text = src), envir = env)
  expect_equal(env$f(), "a\tb")
})
