#' @export

print.cov_testthat_results <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

test_name <- function(x) {
  sub("^test-?", "", sub("[.][rR]$", "", x))
}

testthat_results_by_test <- function(x) {
  # This uses some testthat internals, ideally it would be in testthat
  file <- map_chr(x, "[[", "file")
  context <- map_chr(x, function(x) x$context %||% test_name(x$file))
  test <- map_chr(x, "[[", "test")
  broken <- map_int(x, function(x1) {
    sum(map_lgl(x1$results, expectation_broken))
  })
  skip <- map_int(x, function(x1) {
    sum(map_lgl(x1$results, expectation_skip))
  })
  warning <- map_int(x, function(x1) {
    sum(map_lgl(x1$results, expectation_warning))
  })
  success <- map_int(x, function(x1) {
    sum(map_lgl(x1$results, expectation_success))
  })
  by_test <- data.frame(
    file = file,
    test = test,
    context = context,
    broken = broken,
    skip = skip,
    warning = warning,
    success = success
  )
  by_test <- by_test[order(by_test$context), ]
  class(by_test) <- c("tbl", class(by_test))
  by_test
}

testthat_results_by_file <- function(x) {
  by_test <- testthat_results_by_test(x)
  file <- factor(by_test$file)
  by_file <- data.frame(
    file = levels(file),
    broken = tapply(by_test$broken, file, sum),
    skip = tapply(by_test$skip, file, sum),
    warning = tapply(by_test$warning, file, sum),
    success = tapply(by_test$success, file, sum)
  )
  by_file$context <- test_name(by_file$file)
  by_file <- by_file[order(by_file$context), ]
  by_file
}

#' @export

format.cov_testthat_results <- function(x, ...) {
  by_file <- testthat_results_by_file(x)
  report_by_file <- by_file[
    by_file$broken > 0 | by_file$skip > 0 | by_file$warning > 0,
  ]
  pkg <- attr(x, "pkg")
  at <- attr(x, "at")
  if (!is.null(at)) {
    at[] <- round(unclass(at))
  }

  c(
    if (nrow(report_by_file) > 0) {
      F <- format(c("F", zero(report_by_file$broken)))
      W <- format(c("W", zero(report_by_file$warning)))
      S <- format(c("S", zero(report_by_file$skip)))
      OK <- format(c(cli::symbol$tick, zero(report_by_file$success)))
      ctx <- c("Context", report_by_file$context)
      lines <- c(
        paste0(
          c(style_orange(F[1]), style(F[-1], "broken")),
          " ",
          c(cli::col_magenta(W[1]), style(W[-1], "warning")),
          " ",
          c(cli::col_blue(S[1]), style(S[-1], "skip")),
          " ",
          c(cli::col_green(OK[1]), style(OK[-1], "success")),
          " \u2502 ",
          ctx
        ),
        cli::rule(line = 2)
      )
      lines[1] <- style_bg_grey(cli::ansi_align(lines[1]))
      lines
    },
    "",
    paste0(
      summary_line(
        n_ok = sum(by_file$success),
        n_fail = sum(by_file$broken),
        n_warn = sum(by_file$warning),
        n_skip = sum(by_file$skip)
      ),
      if (!is.null(at)) {
        paste0(
          cli::col_grey("   @ "),
          cli::col_grey(round(at)),
          cli::col_grey(", "),
          cli::col_grey(format_time_ago$time_ago(at))
        )
      }
    )
  )
}

style <- function(x, how = c("broken", "warning", "skip", "success")) {
  how <- match.arg(how)
  st <- switch(
    how,
    broken = style_bg_orange,
    warning = cli::bg_magenta,
    skip = cli::bg_blue,
    success = cli::col_green
  )
  ifelse(grepl("^\\s*$", x), x, st(x))
}

expectation_broken <- function(exp) {
  expectation_failure(exp) || expectation_error(exp)
}

expectation_failure <- function(exp) {
  expectation_type(exp) == "failure"
}

expectation_error <- function(exp) {
  expectation_type(exp) == "error"
}

expectation_skip <- function(exp) {
  expectation_type(exp) == "skip"
}

expectation_warning <- function(exp) {
  expectation_type(exp) == "warning"
}

expectation_success <- function(exp) {
  !expectation_broken(exp) &&
    !expectation_skip(exp) &&
    !expectation_warning(exp)
}

expectation_type <- function(exp) {
  gsub("^expectation_", "", class(exp)[[1]])
}
