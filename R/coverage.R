#' Run package tests and show test coverage results
#'
#' @details
#' Performs the following steps:
#' - Copies the package to a build directory, or updates the most recent
#'   build, if there is one. The build directory is `.cache/uncovr` within
#'   the package tree.
#' - Instruments the R code of the package in the build directory.
#' - Loads the package from the build directory using [pkgload::load_all()].
#' - Runs the package tests.
#' - Calculates and shows the test coverage of the package's R files.
#'
#' Currently only the R code of the package is measured, and only code
#' that runs in the current R process. Code coverage of compiled code and
#' code that runs in subprocesses is not measured.
#'
#' # Excluding code from the coverage results
#'
#' * Lines that contain `# nocov` are excluded.
#' * Code between `# nocov start` and `# nocov end` lines is excluded.
#' * Files listed in a `.covrignore` file at the root of the package tree
#'   are excluded. The file may contain glob patterns, see [Sys.glob()].
#'
#' @param filter Regular expression to filter both the test files (via the
#'   `filter` argument of [testthat::test_dir()]), and the source files
#'   in the coverage results.
#' @param path Path to the package tree.
#' @param test_dir Test directory to use. Defaults to `tests/testthat`
#'   within the package tree.
#' @param reporter The testthat reporter to use. Passed to
#'   [testthat::test_dir()].
#' @param clean Whether to delete the build directory before the build.
#' @param show_coverage Whether to show code coverage results.
#' @param stop_on_failure Whether to throw an error if any tests fail.
#'   Defaults to `TRUE` on CI, `FALSE` otherwise. Coverage output is
#'   always shown before the error is signalled.
#'
#' @return A list of class `package_coverage` with entries:
#'   - `setup`: Build setup. A list.
#'   - `plan`: Build plan, a data frame of the files copied to the build
#'     directory.
#'   - `load`: Return value of [pkgload::load_all()].
#'   - `coverage`: Code coverage results. Columns:
#'     - `path`: Relative path to the R code file.
#'     - `symbol`: The R variable that is used to collect the coverage for
#'       this file.
#'     - `line_count`: Total number of lines in the file.
#'     - `code_lines`: Number of code lines (that are not excluded).
#'     - `lines_covered`: The number of covered lines.
#'     - `percent_covered`: The test coverage percentage of the file.
#'     - `lines`: A list column with a data frame for each file. The data
#'       frame has columns:
#'       - `lines`: The code line.
#'       - `status`: Whether this line is `"instrumented"`, `"noncode"` or
#'         `"excluded"`.
#'       - `id` The id of the counter that applies to this line. Often the
#'         same as the line number, but not always, e.g. for multi-line
#'         expressions. `NA` for lines that are not `"instrumented"`.
#'       - `coverage`: The number of times the line was covered. `NA` for
#'         lines that are not `"instrumented"`.
#'   - `test_results`: Return value of [testthat::test_dir()].
#'
#' @export
#' @examples
#' # Copy the example package that comes with uncovr to a temporary
#' # directory, because `coverage()` writes its build into the package tree
#' src <- system.file("example", "demopkg", package = "uncovr")
#' invisible(file.copy(src, tempdir(), recursive = TRUE))
#' pkg <- file.path(tempdir(), "demopkg")
#'
#' coverage(path = pkg)
#'
#' # Clean up
#' pkgload::unload("demopkg")
#' unlink(pkg, recursive = TRUE)

coverage <- function(
  filter = NULL,
  path = ".",
  test_dir = "tests/testthat",
  reporter = NULL,
  clean = FALSE,
  show_coverage = TRUE,
  stop_on_failure = is_ci()
) {
  withr::local_dir(path)

  if (Sys.getenv("NOT_CRAN") == "") {
    withr::local_envvar(NOT_CRAN = "true")
  }

  dev_data <- load_instrumented(path = ".", clean = clean)
  setup <- dev_data$setup

  withr::with_envvar(c(TESTTHAT_COVERAGE = setup$pkgname), {
    dev_data$test_results <- testthat::test_dir(
      test_dir,
      package = setup[["pkgname"]],
      load_package = "none",
      stop_on_failure = FALSE,
      filter = filter,
      reporter = reporter
    )
  })

  counts <- cov_get_counts(
    dev_data$coverage$symbol,
    asNamespace(setup$pkgname),
    setup
  )
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

  dev_data$coverage$percent_covered <-
    ifelse(
      dev_data$coverage$code_lines == 0,
      100,
      dev_data$coverage$lines_covered / dev_data$coverage$code_lines * 100
    )

  dev_data$coverage <- add_coverage_summary(dev_data$coverage)

  dev_data$coverage_filter <- filter

  class(dev_data$coverage) <- c("coverage_table2", class(dev_data$coverage))
  class(dev_data) <- c("package_coverage", class(dev_data))

  if (show_coverage) {
    print(dev_data)
  }

  if (stop_on_failure) {
    tr <- as.data.frame(dev_data$test_results)
    nfail <- sum(tr$failed) + sum(tr$error)
    if (nfail > 0) {
      stop(nfail, " test(s) failed.", call. = FALSE)
    }
  }

  invisible(dev_data)
}

add_coverage_summary <- function(coverage) {
  # by directory
  dirs <- all_leading_dirs(coverage$path)
  sumdir <- function(x, dir) {
    sum(x[startsWith(coverage$path, dir)], na.rm = TRUE)
  }
  sm <- data.frame(
    name = c("All files", dirs[-1]),
    line_count = map_int(dirs, sumdir, x = coverage$line_count),
    code_lines = map_int(dirs, sumdir, x = coverage$code_lines),
    lines_covered = map_int(dirs, sumdir, x = coverage$lines_covered),
    total_hits = map_dbl(dirs, sumdir, x = as.double(coverage$total_hits)),
    function_count = map_int(dirs, sumdir, x = coverage$function_count),
    functions_hit = map_int(dirs, sumdir, x = coverage$functions_hit)
  )
  class(sm) <- c("tbl", class(sm))

  sm$percent_covered <- ifelse(
    sm$code_lines == 0,
    100,
    sm$lines_covered / sm$code_lines * 100
  )
  attr(coverage, "summary") <- sm

  coverage
}

calculate_uncovered_intervals <- function(lines) {
  keep <- lines$status == "instrumented"
  lineno <- seq_len(nrow(lines))[keep]
  find_zero_ranges(lineno, lines$coverage[keep])
}

apply_covrignore <- function(paths, exclusion_file) {
  if (!is.null(exclusion_file)) {
    if (file.exists(exclusion_file)) {
      excluded <- Sys.glob(readLines(exclusion_file))
      paths <- setdiff(paths, excluded)
      dirs <- excluded[file.exists(excluded) & is_dir(excluded)]
      dirs <- ifelse(endsWith(dirs, "/"), dirs, paste0(dirs, "/"))
      for (d in dirs) {
        paths <- paths[!startsWith(paths, d)]
      }
    }
  }
  paths
}
