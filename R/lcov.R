#' Write an LCOV coverage report
#'
#' An LCOV coverage report contains information about the code coverage
#' of all files in a project.
#'
#' @inheritParams load_package
#' @param coverage Test coverage results. If `NULL` then the last
#'   results are used via [last_coverage_results()].
#' @param output Path of the output file. If `NULL`, then it is
#'   `_lcov/lcov.info` inside the build directory.
#' @return The path of the output file, invisibly.
#'
#' @export

write_lconv_info <- function(path = ".", coverage = NULL, output = NULL) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last_coverage_results(path = ".")
  setup <- load_package_setup("coverage")

  metadata <- attr(coverage, "metadata")

  output <- output %||% file.path(setup$dir, "_lcov", "lcov.info")

  mkdirp(dirname(output))
  clean_exit <- FALSE
  on.exit(
    {
      try(close(outcon), silent = TRUE)
      if (!clean_exit) unlink(output)
    },
    add = TRUE
  )
  outcon <- file(output, open = "wb")

  # header
  writeLines(
    paste0("# test covegare for the ", setup$pkgname, " R package, by uncovr"),
    outcon
  )
  writeLines(paste0("TN:testthat test suite for ", setup$pkgname), outcon)

  for (i in seq_len(nrow(coverage))) {
    writeLines(paste0("SF:", coverage$path[i]), outcon)
    # TODO: what exactly should VER be? Is it important for us?
    # TODO: functions
    lns <- coverage$lines[[i]]
    lns <- lns[!is.na(lns$coverage), ]
    if (nrow(lns) > 0) {
      writeLines(paste0("DA:", lns$id, ",", lns$coverage), outcon)
    }
    writeLines(paste0("LH:", sum(lns$coverage != 0)), outcon)
    writeLines(paste0("LF:", nrow(lns)), outcon)
    writeLines("end_of_record", outcon)
  }

  close(outcon)
  clean_exit <- TRUE
  invisible(output)
}
