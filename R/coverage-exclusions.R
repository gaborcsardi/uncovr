parse_line_exclusions <- function(lns, path) {
  sort(unique(c(
    parse_line_exclusions_single(lns, path),
    parse_line_exclusions_ranges(lns, path)
  )))
}

parse_line_exclusions_single <- function(lns, path) {
  which(
    grepl("__NO_COVERAGE__", lns) |
      grepl("#[ ]*nocov", lns) |
      grepl("//[ ]*nocov", lns) |
      grepl("/[*][ ]*nocov[ ]* [*]/", lns) |
      grepl("LCOV_EXCL_LINE", lns)
  )
}

parse_line_exclusions_ranges <- function(lns, path) {
  start <- unique(sort(c(
    grep("#[ ]*nocov[ ]+(start|begin)", lns),
    grep("LCOV_EXCL_START", lns)
  )))
  end <- unique(sort(c(
    grep("#[ ]*nocov[ ]+end", lns),
    grep("LCOV_EXCL_STOP", lns)
  )))

  length(start) <- length(end) <- max(length(start), length(end))
  if (length(start) == 0) {
    return(integer())
  }

  keep <- rep(TRUE, length(lns))
  ranges <- data.frame(stringsAsFactors = FALSE, start = start, end = end)

  for (i in seq_len(nrow(ranges))) {
    if (is.na(ranges$start[i])) {
      stop(cli::format_error(
        "Found {.code # nocov end} without {.code # nocov start} at
        {.path {path}}:{ranges$end[i]}."
      ))
    }
    if (is.na(ranges$end[i]) || ranges$start[i] > ranges$end[i]) {
      stop(cli::format_error(
        "Found {.code # nocov start} without {.code # nocov end} at
        {.path {path}}:{ranges$start[i]}."
      ))
    }
    keep[ranges$start[i]:ranges$end[i]] <- FALSE
  }

  which(!keep)
}
