#' @export

`[.coverage_table2` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "coverage_table2")
  requireNamespace("pillar", quietly = TRUE)
  NextMethod("[")
}

#' @export

format.coverage_table2 <- function(x, filter = NULL, ...) {
  if (is.null(filter)) {
    format_coverage_table2_full(x, ...)
  } else {
    format_coverage_table2_filter(x, filter, ...)
  }
}

format_coverage_table2_filter <- function(x, filter, ...) {
  mch <- grep(filter, sub("[.][rR]$", "", basename(x$path)))
  if (length(mch) == 0) {
    warning(
      "\nNo matching code file for '",
      filter,
      "', showing full coverage table"
    )
    return(format_coverage_table2_full(x, ...))
  }

  x <- x[mch, ]
  bl <- format_pct(x$percent_covered)
  label <- attr(x, "coverage-name") %||% "code coverage"
  cffn <- ffn <- format(c(label, "", x$path))
  cfbl <- fbl <- format(c("% lines", "", bl))

  mid <- 3:(length(ffn))
  cffn[mid] <- cov_col(ffn[mid], x$percent_covered)
  cfbl[mid] <- cov_col(fbl[mid], x$percent_covered)

  lines <- paste0(cffn, " \u2502 ", cfbl, "\u2502 ")
  maxw <- max(cli::ansi_nchar(lines, type = "width"))
  cw <- cli::console_width()

  uc <- mapply(format_uncovered, x$uncovered, file = x$path, width = 1000L)
  uc <- cli::ansi_strwrap(
    uc,
    width = cw - maxw,
    simplify = FALSE
  )
  for (i in seq_along(uc)) {
    if (length(uc[[i]]) > 1) {
      uc[[i]][-1] <- paste0(
        strrep(" ", maxw - 2),
        cli::col_none("\u2502 "),
        uc[[i]][-1]
      )
    }
  }
  ucls <- lengths(uc)
  uc <- map_chr(uc, paste, collapse = "\n")
  if (any(ucls > 1)) {
    cuc <- c("uncovered line #", "", uc)
    cuc[1] <- cli::ansi_align(cuc[1], width = cw - maxw)
  } else {
    cuc <- cli::ansi_align(
      c("uncovered line #", "", uc),
      width = max(cli::ansi_nchar(uc, "width"))
    )
  }
  cuc[mid] <- cov_col(cuc[mid], x$percent_covered)
  lines <- paste0(lines, cuc)

  # top line
  lines[1] <- cli::style_bold(style_bg_grey(cli::col_white(lines[1])))

  lines
}

format_coverage_table2_full <- function(x, ...) {
  sm <- attr(x, "summary")
  fn0 <- c(sm$name[1], sm$name[-1], x$path)
  fn <- c(sm$name[1], dir_ind(sm$name[-1]), dir_ind(x$path))
  rl <- c(sm$percent_covered, x$percent_covered)
  fc <- c(sm$function_count, x$function_count)
  fh <- c(sm$functions_hit, x$functions_hit)
  bl <- format_pct(rl)
  fmiss <- is.na(fc) | is.na(fh)
  fs <- ifelse(fmiss, "", paste0(fh, "/", fc))

  label <- attr(x, "coverage-name") %||% "code coverage"
  cffn <- ffn <- format(c(label, "", fn, "", "total"))
  cfbl <- fbl <- format(c("% lines", "", bl, "", bl[1]), justify = "right")
  cffs <- ffs <- format(c("funs", "", fs, "", fs[1]), justify = "right")

  mid <- 3:(length(ffn) - 2)
  cffn[mid] <- cov_col(ffn[mid], rl)
  cfbl[mid] <- cov_col(fbl[mid], rl)
  cffs[mid] <- cov_col(
    ffs[mid],
    ifelse(fmiss | fc == 0, 100, ifelse(fh < fc, 0, 100))
  )

  lines <- paste0(cffn, " \u2502 ", cfbl, " \u2502 ", cffs, " \u2502 ")
  maxw <- max(cli::ansi_nchar(lines, type = "width"))
  cw <- cli::console_width()

  uc <- mapply(format_uncovered, x$uncovered, file = x$path, width = cw - maxw)
  cuc <- cli::ansi_align(
    c("uncovered line #", "", rep("", nrow(sm)), uc, "", ""),
    width = max(cli::ansi_nchar(uc, "width"))
  )
  tot <- sm$percent_covered
  cuc[mid] <- cov_col(cuc[mid], c(tot, x$percent_covered))
  lines <- paste0(lines, cuc)

  # top line
  lines[1] <- cli::style_bold(style_bg_grey(cli::col_white(lines[1])))

  # bottom line
  if (tot[1] < 75) {
    lines[length(lines)] <- style_bg_orange(lines[length(lines)])
  } else if (tot[1] < 95) {
    lines[length(lines)] <-
      cli::bg_blue(cli::col_white(lines[length(lines)]))
  } else {
    lines[length(lines)] <-
      style_bg_green(cli::col_white(lines[length(lines)]))
  }
  lines[length(lines)] <- cli::style_bold(lines[length(lines)])

  # directory summaries at the right place
  lines[mid] <- lines[mid][c(1, path_order(fn0[-1]) + 1)]

  test_results <- attr(x, "test_results")
  if (!is.null(test_results)) {
    lines <- c(lines, "", format(test_results))
  }

  lines
}

cov_col <- function(txt, val) {
  ifelse(
    val < 75,
    style_orange(txt),
    ifelse(val < 95, cli::col_blue(txt), txt)
  )
}

format_link <- function(text, file, line = NULL) {
  if (Sys.getenv("POSITRON") == "1") {
    scheme <- "positron"
  } else if (Sys.getenv("TERM_PROGRAM") == "vscode") {
    scheme <- "vscode"
  } else {
    return(text)
  }
  cli::style_hyperlink(
    text,
    paste0(
      scheme,
      "://file",
      normalizePath(file, mustWork = FALSE),
      line %&&% paste0(":", line)
    )
  )
}

format_pct <- function(x) {
  paste0(format(x, width = 3, digits = 3, justify = "right"), "%")
}

format_uncovered <- function(ranges, file, width = 80) {
  rstr <- vapply(ranges, FUN.VALUE = character(1), function(r) {
    if (length(r) == 1) as.character(r) else paste0(r[1], "-", r[length(r)])
  })

  iterm <- Sys.getenv("R_CLI_HYPERLINK_STYLE") == "iterm"
  ls <- if (iterm) "#" else ":"
  rstr <- map_chr(
    rstr,
    function(x) {
      format_link(x, file, sub("[-].*$", "", x))
    }
  )

  rstr[-length(rstr)] <- paste0(rstr[-length(rstr)], ", ")

  if (length(rstr) >= 3) {
    cumw <- cumsum(cli::ansi_nchar(rstr, "width"))
    if (cumw[length(cumw)] > width) {
      last <- rev(which(cumw <= width - 3))[1]
      rstr <- if (is.na(last)) {
        "..."
      } else {
        c(rstr[1:last], "...")
      }
    }
  }

  paste(rstr, collapse = "")
}

#' @export

print.coverage_table2 <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.package_coverage <- function(x, ...) {
  c("", format(x$coverage, filter = x$coverage_filter, ...))
}

#' @export

print.package_coverage <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}
