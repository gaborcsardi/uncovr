#' Run package tests and show test coverage results
#'
#' @details
#' Performs the following steps:
#' - Builds the package in a build directory, or updates the most recent
#'   build, if there is one.
#' - Loads the package from the build directory using [pkgload::load_all()].
#' - Installs the package into a local library (by default).
#' - Runs the package tests.
#' - Calculates and shows the test coverage of the package's source files.
#'
#' @param filter Regular expression to filter both the test files (via the
#'   `filter` argument of [testthat::test_dir()]), and the source files
#'   in the coverage results.
#' @param test_dir Test directory to use. Defaults to `tests/testthat`
#'   within the package tree.
#' @param reporter The testthat reporter to use. Passed to
#'   [testthat::test_dir()].
#' @param show_coverage Whether to show code coverage results.
#' @param report Whether to generate a HTML test coverage report.
#' @param show_report Whether to show the HTML test coverage
#'   report (if `report` is `TRUE`).
#' @param lcov_info Whether to create an lcov info file, see
#'   [lcov()]. Defaults to the value of the
#'   `r opt_option_name("lcov_info")` option, then the
#'   `r opt_envvar_name("lcov_info")` environment variable, or if neither
#'   are set, then `r get_option_default("lcov_info")`.
#' @param github_summary Whether to generate a markdown summary in
#'   `$GITHUB_STEP_SUMMARY`. It defaults to `FALSE`, except if running on
#'   GitHub Actions.
#'
#' @inheritParams reload
#'
#' @return A list of class `package_coverage` with entries:
#'   - `setup`: Build setup, see [reload()].
#'   - `plan`: Build plan, see [reload()].
#'   - `load`: Return value of [pkgload::load_all()].
#'   - `coverage`: Code coverage results. Columns:
#'     - `path`: Relative path to the R code file.
#'     - `symbol`: The R variable that is used to collect the coverage for
#'       this file.
#'     - `line_count`: Total number of lines in the file.
#'     - `code_lines`: Number of code lines (that are not excluded).
#'     - `lines_covered`: The number of covered lines will be stored here
#'       after a test coverage run. For `reload()` it is all zero.
#'     - `percent_covered`: The test coverage percentage of the file will
#'       be stored here after a test coverage run. For `reload()` it is
#'       all zero.
#'     - `lines`: A list column with a data frame for each file. The data
#'       frame has columns:
#'       - `lines`: The code line.
#'       - `status`: Whether this line is `"instrumented"`, `"noncode"` or
#'         `"excluded"`.
#'       - `id` The id of the counter that applies to this line. Often the
#'         same as the line number, but not always, e.g. for multi-line
#'         expressions. `NA` for lines that are not `"instrumented"`.
#'       - `coverage`: The number of times the line was covered will be filled
#'         in here after a test coverage run. For `reload()` it is zero,
#'         but `NA` for lines that are not `"instrumented"`.
#'   - `test_results`: Return value of [testthat::test_dir()].
#'
#' @export

test <- function(
  filter = NULL,
  path = ".",
  test_dir = "tests/testthat",
  reporter = NULL,
  clean = FALSE,
  local_install = TRUE,
  show_coverage = TRUE,
  report = FALSE,
  show_report = report && interactive(),
  lcov_info = NULL,
  github_summary = NULL
) {
  lcov_info <- lcov_info %||% get_option("lcov_info", "flag")
  github_summary <- github_summary %||% Sys.getenv("GITHUB_ACTIONS") != ""

  withr::local_dir(path)

  if (Sys.getenv("NOT_CRAN") == "") {
    withr::local_envvar(NOT_CRAN = "true")
  }

  # clean up .gcda files, because pkgbuild wrongly considers them as source
  # files and thinks that the dll is out of data, because they are newer
  setup <- reload_setup(type = "coverage", path = ".")
  pkg_path <- file.path(setup$dir, setup$pkgname)
  if (!clean) {
    gcov_cleanup(pkg_path)
  }

  dev_data <- reload(
    type = "coverage",
    path = ".",
    clean = clean,
    local_install = local_install
  )

  # clean up files from subprocesses
  subprocdir <- file.path(setup$dir, cov_sub_dir_name)
  unlink(subprocdir, recursive = TRUE)
  dir.create(subprocdir)
  subprocdir <- normalizePath(subprocdir)

  if (is_ci()) {
    writeLines(banner_test)
  }

  withr::with_envvar(c(TESTTHAT_COVERAGE = setup$pkgname), {
    dev_data$test_results <- testthat::test_dir(
      test_dir,
      package = setup[["pkgname"]],
      load_package = "installed",
      stop_on_failure = FALSE,
      filter = filter,
      reporter = reporter
    )
  })

  cov_names <- dev_data$coverage$symbol
  counts <- cov_get_counts(cov_names)
  counts <- add_subprocess_coverage(counts, subprocdir)
  for (i in seq_along(counts)) {
    ids <- dev_data$coverage$lines[[i]]$id
    dev_data$coverage$lines[[i]]$coverage[] <- counts[[i]][ids]
    dev_data$coverage$lines_covered[i] <-
      sum(dev_data$coverage$lines[[i]]$coverage > 0, na.rm = TRUE)
    dev_data$coverage$total_hits[i] <-
      sum(as.double(dev_data$coverage$lines[[i]]$coverage), na.rm = TRUE)
    dev_data$coverage$uncovered[[i]] <-
      calculate_uncovered_intervals(dev_data$coverage$lines[[i]])
    funids <- dev_data$coverage$funs[[i]]$id
    dev_data$coverage$funs[[i]]$coverage[] <- counts[[i]][funids]
    dev_data$coverage$functions_hit[i] <-
      sum(dev_data$coverage$funs[[i]]$coverage > 0, na.rm = TRUE)
  }

  if (file.exists("src")) {
    # try to flush the coverage data for the package
    tryCatch(
      gcov_flush_package(dev_data$setup$pkgname),
      error = function(...) {
        stop(cli::format_error(c(
          "Could not run `gcov_flush()` in the {.pkg {dev_data$setup$pkgname}}
          package. You need to add a `gcov_flush()` function to it, that
          calls `__gcov_dump()`, see an example in the {.pkg ps} package."
        )))
      }
    )
    exc <- if (file.exists(".covrignore")) {
      normalizePath((".covrignore"))
    }
    ccoverage <- load_c_coverage(pkg_path, exc)
    dev_data$coverage <- rbind(dev_data$coverage, ccoverage)
  }

  dev_data$coverage$percent_covered <-
    dev_data$coverage$lines_covered / dev_data$coverage$code_lines * 100
  dev_data$coverage$percent_covered[dev_data$coverage$code_lines == 0] <- 100

  dev_data$coverage <- add_coverage_summary(dev_data$coverage)

  dev_data$coverage_filter <- filter

  class(dev_data$coverage) <- c("coverage_table2", class(dev_data$coverage))
  class(dev_data) <- c("package_coverage", class(dev_data))

  test_results_file <- file.path(setup$dir, last_tests_file_name)
  test_results <- prepare_test_results(dev_data)
  quick_save_rds(test_results, test_results_file)
  coverage_results_file <- file.path(setup$dir, last_coverage_file_name)
  coverage_results <- prepare_coverage_results(dev_data)
  quick_save_rds(coverage_results, coverage_results_file)

  if (report) {
    report(coverage = coverage_results, show = show_report)
  }

  if (lcov_info) {
    lcov(coverage = coverage_results)
  }

  if (github_summary) {
    attr(coverage_results, "test_results") <- test_results
    ghout <- markdown(coverage = coverage_results)
    if (Sys.getenv("GITHUB_STEP_SUMMARY") != "") {
      cat(
        readLines(ghout),
        file = Sys.getenv("GITHUB_STEP_SUMMARY"),
        sep = "\n",
        append = TRUE
      )
    }
  }

  if (show_coverage) {
    if (is_ci()) {
      writeLines(banner_coverage)
    }
    dev_data
  } else {
    invisible(dev_data)
  }
}
