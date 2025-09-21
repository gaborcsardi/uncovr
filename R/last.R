last_test_results <- function(path = ".") {
  withr::local_dir(path)
  setup <- reload_setup(type = "coverage", path = ".")
  test_results_file <- file.path(setup$dir, last_tests_file_name)
  if (!file.exists(test_results_file)) {
    message("No test results yet. Run `test()!")
    return(invisible(NULL))
  }

  tr <- readRDS(test_results_file)
  attr(tr, "at") <- file.mtime(test_results_file)
  tr
}

#' Find and print the last test coverage results
#'
#' @inheritParams reload
#' @return A package_coverage object. If there are no previous results,
#'   then a message is shown and `NULL` is returned.
#'
#' @export

last <- function(path = ".") {
  withr::local_dir(path)
  setup <- reload_setup(type = "coverage", path = ".")
  coverage_results_file <- file.path(setup$dir, last_coverage_file_name)
  if (!file.exists(coverage_results_file)) {
    cli::cli_alert_info("No test coverage yet. Run `test()!")
    return(invisible(NULL))
  }

  cr <- readRDS(coverage_results_file)
  attr(cr, "at") <- file.mtime(coverage_results_file)
  attr(cr, "test_results") <- suppressMessages(last_test_results("."))

  cr
}

#' Re-run the test files that had failing tests in the last test run
#'
#' @inheritParams reload
#' @param types Test result types to re-run, a character vector, possible
#'   elements are `"fail"` (default), `"warning"`, `"skip"`, "`all`".
#'   `"all"` is equivalent to `c("fail", "warning", "skip")`.
#' @param show_coverage Whether to show code coverage results.
#' @param ... Additional arguments are passed to [test()].
#'
#' @export

retest <- function(
  path = ".",
  types = c("fail", "warning", "skip", "all")[1],
  show_coverage = FALSE,
  ...
) {
  tr <- last_test_results(path)
  if (is.null(tr)) {
    return(invisible(NULL))
  }

  by_file <- testthat_results_by_file(tr)
  sel <- rep(FALSE, nrow(by_file))
  if (any(c("fail", "all") %in% types)) {
    sel <- sel | by_file$broken > 0
  }
  if (any(c("warning", "all") %in% types)) {
    sel <- sel | by_file$warning > 0
  }
  if (any(c("skip", "all") %in% types)) {
    sel <- sel | by_file$skip > 0
  }
  rerun <- by_file$context[sel]
  if (length(rerun) == 0) {
    cli::cli_alert_info("Nothing to re-run.")
    return(invisible(NULL))
  }

  # TODO: do a better job than this regex
  rerun <- gsub(".", "[.]", fixed = TRUE, rerun)
  filter <- paste0("^(", paste(rerun, collapse = "|"), ")$")
  test(filter = filter, path = path, show_coverage = show_coverage, ...)
}
