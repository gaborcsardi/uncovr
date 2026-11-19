parse_line_exclusions <- function(lns, path) {
  sort(unique(c(
    parse_line_exclusions_single(lns, path),
    parse_line_exclusions_ranges(lns, path)
  )))
}

parse_line_exclusions_single <- function(lns, path) {
  which(
    grepl("__NO_COVERAGE__", lns) |
      # `# nocov next` excludes the next expression, not its own line
      grepl("#[ ]*nocov(?![ ]+next\\b)", lns, perl = TRUE) |
      grepl("//[ ]*nocov", lns) |
      grepl("/[*][ ]*nocov[ ]* [*]/", lns) |
      grepl("LCOV_EXCL_LINE", lns)
  )
}

# `# nocov next` excludes the next R expression: the outermost expression
# that starts at the first non-comment token after the comment. `psd` is
# the output of `utils::getParseData()`. Returns the excluded line numbers.

parse_next_expr_exclusions <- function(psd) {
  cmt <- which(
    psd$token == "COMMENT" & grepl("^#[ ]*nocov[ ]+next\\b", psd$text)
  )
  if (length(cmt) == 0) {
    return(integer())
  }

  term <- which(psd$terminal & psd$token != "COMMENT")
  term <- term[order(psd$line1[term], psd$col1[term])]
  expr_tokens <- c("expr", "expr_or_assign_or_help", "equal_assign")
  closing <- c("'}'", "')'", "']'", "','", "RBB")

  res <- integer()
  for (c in cmt) {
    after <- psd$line1[term] > psd$line2[c] |
      (psd$line1[term] == psd$line2[c] & psd$col1[term] > psd$col2[c])
    nxt <- term[after][1]
    if (is.na(nxt) || psd$token[nxt] %in% closing) {
      next
    }
    node <- nxt
    repeat {
      par <- match(psd$parent[node], psd$id)
      if (
        is.na(par) ||
          !psd$token[par] %in% expr_tokens ||
          psd$line1[par] != psd$line1[nxt] ||
          psd$col1[par] != psd$col1[nxt]
      ) {
        break
      }
      node <- par
    }
    res <- c(res, psd$line1[node]:psd$line2[node])
  }

  sort(unique(res))
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
