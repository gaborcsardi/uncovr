#' Test the active file
#'
#' Calls [test()] with a `filter` to only run tests and show test coverage
#' for the currently active file in RStudio, Positron or VS Code.
#'
#' @param file Use this file instead of auto-detecting the active file.
#' @param ... Additional arguments are passed to [test()].
#' @return The same as [test()].
#'
#' @export

test_active <- function(file = NULL, ...) {
  stopifnot(is.null(file) || is_string(file))
  filter <- NULL
  if (!is.null(file)) {
    file <- suppressWarnings(normalizePath(file))
    if (!file.exists(file)) {
      # if no such file, then use it as a filter
      filter <- file
    }
  } else {
    file <- get_active_file()
    if (is.null(file) || !file.exists(file)) {
      stop("Could not find the currently active file.")
    }
  }

  if (is.null(filter)) {
    testfile <- find_test_file(file)
    if (is.null(testfile)) {
      # no test file, use file name as a filter
      filter <- tools::file_path_sans_ext(basename(file))
    } else {
      # TODO: do a better job than this regex
      testfile <- tools::file_path_sans_ext(basename(testfile))
      testfile <- sub("^test[-_.]", "", testfile)
      testfile <- gsub(".", "[.]", fixed = TRUE, testfile)
      filter <- paste0("^(", paste(testfile, collapse = "|"), ")$")
    }
  }

  test(filter = filter, ...)
}

get_active_file <- function() {
  if (!rstudioapi::isAvailable()) {
    return(NULL)
  }
  suppressWarnings(normalizePath(rstudioapi::getSourceEditorContext()$path))
}

find_test_file <- function(file) {
  fn <- basename(file)
  dn <- basename(dirname(file))
  dn2 <- basename(dirname(dirname(file)))

  if (dn == "R" || dn == "src") {
    fn0 <- tools::file_path_sans_ext(fn)
    ttd <- file.path(dirname(dirname(file)), "tests", "testthat")
    tfn <- file.path(ttd, paste0("test-", fn0, ".R"))
    if (file.exists(tfn)) {
      tfn
    } else {
      fn0 <- gsub(".", "[.]", fixed = TRUE, fn0)
      m1 <- dir(ttd, pattern = paste0("^test-", fn0), full.names = TRUE)
      if (length(m1) > 0) {
        m1
      } else {
        m2 <- dir(ttd, pattern = fn0, full.names = TRUE)
        if (length(m2) > 0) {
          m2
        } else {
          NULL
        }
      }
    }
  } else if (dn == "testthat" && dn2 == "tests") {
    file
  } else {
    NULL
  }
}
