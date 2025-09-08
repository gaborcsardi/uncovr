#' Generate a HTML coverage report
#'
#' @inheritParams load_package
#' @param output Output file. This is the HTML index file. The rest of
#'   of the HTML files for the source code files will be placed next
#'   to it. By default it is generated in the dev directory.
#' @param show Whether to open a browser window to show the HTML
#'   report.
#' @return The path of the output file, invisibly.
#'
#' @export

coverage_report <- function(
  path = ".",
  output = NULL,
  show = interactive()
) {
  withr::local_dir(path)
  rm(path)
  coverage <- last_coverage_results(path = ".")
  setup <- load_package_setup("coverage")

  index <- file.path(find.package("testthatlabs"), "inst/report/index.html")
  lns0 <- readLines(index)

  metadata <- attr(coverage, "metadata")
  pkgname <- metadata$setup$pkgname
  data <- with(
    coverage[],
    paste0(
      "[",
      paste0(
        "[ \"",
        path,
        "\", ",
        line_count,
        ", ",
        code_lines,
        ", ",
        lines_covered,
        ", ",
        code_lines - lines_covered,
        ", ",
        round(total_hits / code_lines, 2),
        ", \"",
        format_percent(percent_covered / 100),
        "\" ]",
        collapse = ",\n"
      ),
      "]"
    )
  )

  total_percent <- format_percent(
    sum(coverage$lines_covered) / sum(coverage$code_lines)
  )

  vars = list(
    package = pkgname,
    total_percent = total_percent,
    data = data
  )

  lns <- glue::glue_data(
    vars,
    paste(lns0, collapse = "\n"),
    .open = "{{",
    .close = "}}"
  )

  output <- output %||%
    file.path(setup$dir, "coverage-report", paste0(pkgname, "-report.html"))

  mkdirp(dirname(output))
  writeLines(lns, output)

  for (path in coverage$path) {
    coverage_report_file_(
      path,
      coverage = coverage,
      path = ".",
      output_dir = dirname(output),
      show = FALSE,
      setup = setup
    )
  }

  if (show) {
    utils::browseURL(output)
  }

  invisible(output)
}

format_percent <- function(x) {
  x[is.na(x)] <- 1
  sprintf("%0.2f%%", x * 100)
}

#' Generate a HTML coverage report for a source file
#'
#' [coverage_report()] calls this function for all source files, so
#' you only need this if you want a report for a single file.
#'
#' @param code_file Relative path to the code file to show.
#' @param coverage A coverage object, optionally.
#'   If not present, the output of [last_coverage_results()] is used.
#' @param output_dir Output directory to put the HTML report into.
#' @param show Whether to open a browser window to show the HTML
#'   report.
#' @inheritParams load_package
#'
#' @export

coverage_report_file <- function(
  code_file = NULL,
  coverage = NULL,
  path = ".",
  output_dir = NULL,
  show = interactive()
) {
  coverage_report_file_(code_file, coverage, path, output_dir, show)
}

coverage_report_file_ <- function(
  code_file = NULL,
  coverage = NULL,
  path = ".",
  output_dir = NULL,
  show = interactive(),
  setup = NULL
) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last_coverage_results(path = ".")
  setup <- setup %||% load_package_setup("coverage")

  pathidx <- match(code_file, coverage$path)
  if (is.na(pathidx)) {
    stop("No test coverage data for file '", code_file, "'.")
  }

  metadata <- attr(coverage, "metadata")
  pkgname <- metadata$setup$pkgname
  total_percent <- format_percent(
    sum(coverage$lines_covered) / sum(coverage$code_lines)
  )
  file_percent <-
    sum(coverage$lines_covered[pathidx]) / sum(coverage$code_lines[pathidx])
  file_status <- if (is.na(file_percent)) {
    "na"
  } else if (file_percent < 0.75) {
    "bad"
  } else if (file_percent < 0.95) {
    "ok"
  } else {
    "good"
  }

  code <- escape_source(coverage$lines[[pathidx]][["lines"]])
  cov <- coverage$lines[[pathidx]][["coverage"]]

  vars <- list(
    pkgname = pkgname,
    total_percent = total_percent,
    file_percent = format_percent(file_percent),
    path = code_file,
    lines = paste0("[", paste0("\"", code, "\"", collapse = ",\n"), "]"),
    coverage = paste0(
      "[",
      paste(ifelse(is.na(cov), "NaN", cov), collapse = ", "),
      "]"
    ),
    language = get_language_from_path(code_file),
    file_status = file_status
  )

  srcpath <- file.path(find.package("testthatlabs"), "inst/report/source.html")
  lns0 <- readLines(srcpath)

  lns <- glue::glue_data(
    vars,
    paste(lns0, collapse = "\n"),
    .open = "{{",
    .close = "}}"
  )

  fn <- paste0("file-", utils::URLencode(code_file, reserved = TRUE), ".html")
  output_dir <- output_dir %||% file.path(setup$dir, "coverage-report")
  output <- file.path(output_dir, fn)

  mkdirp(dirname(output))
  writeLines(lns, output)

  if (show) {
    utils::browseURL(output)
  }

  invisible(output)
}

escape_source <- function(x) {
  x <- gsub("\\", "\\\\", fixed = TRUE, x)
  x <- gsub("\"", "\\\"", fixed = TRUE, x)
  x
}

get_language_from_path <- function(path) {
  ext <- tolower(tools::file_path_sans_ext(path))
  res <- rep(NA_character_, length(path))
  res[ext == "r"] <- "r"
  res[ext == "c"] <- "c"
  res[ext %in% c("cc", "c++", "cxx", "hcc", "cpp", "hpp", "h", "hh")] <- "c++"
  res[ext %in% c("f", "f90", "f77")] <- "fortran"
  res[ext %in% c("m", "mm")] <- "objc"
  ifelse(is.na(res), "", paste0("language-", res))
}
