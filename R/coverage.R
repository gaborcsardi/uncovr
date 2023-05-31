
#' @export

test <- function(...) {
  withr::local_envvar(TESTTHAT_PARALLEL = "false")
  if (is_interactive()) {
    test_interactive(...)
  } else {
    test_non_interactive(...)
  }
}

test_interactive <- function(filter = NULL, pr = FALSE, ...) {

  desc <- read.dcf("DESCRIPTION")
  pkg <- desc[, "Package"][[1]]

  asNamespace("covr")$clear_counters()

  reload(covr = TRUE, internals = TRUE, helpers = TRUE)
  gcov_cleanup()

  dev_lib <- dirname(getNamespaceInfo(pkg, "path"))
  withr::local_libpaths(dev_lib, action = "prefix")

  withr::local_envvar(c(R_COVR = "true"))
  withr::local_envvar(c(NOT_CRAN = "true"))

  tryCatch(
    results <- testthat::test_dir(
      "tests/testthat",
      reporter = interactive_reporter$new(pkg),
      filter = filter,
      package = pkg,
      stop_on_failure = FALSE,
      ...
    ),
    interrupt = function(err) {
      msg <- crayon::red(
        "\n\nInterruped test suite, jumping to top level"
      )
      message(msg)
      invokeRestart("abort")
    }
  )

  rcv <- as.list(asNamespace("covr")$.counters)
  class(rcv) <- "coverage"
  trace_dir <- file.path(normalizePath(dev_lib), "_traces")
  trace_files <- list.files(
    path = trace_dir,
    pattern = "^covr_trace_[^/]+$",
    full.names = TRUE
  )
  if (length(trace_files)) rcv <- merge_coverage(rcv, trace_files)
  rcv <- rcv[vapply(rcv, function(x) length(x$value) != 0, logical(1))]

  ccv <- covr:::run_gcov(normalizePath("."))
  rcv <- c(rcv, ccv)
  class(rcv) <- "coverage"

  rcv <- apply_exclusions(rcv)

  if (pr) rcv <- keep_new(rcv)

  coverage <- create_coverage_table(rcv, filter = filter, pr = pr)
  cat("\n")
  print(coverage)

  invisible(structure(
    list(tests = results, coverage = coverage, raw_coverage = rcv),
    class = "testthat_coverage"
  ))
}

apply_exclusions <- function(cov) {
  src <- vcapply(cov, function(x) {
    if (inherits(x, "line_coverage")) as.character(x$srcref) else ""
  })

  # Drop single excluded lines
  drop <- grepl("__NO_COVERAGE__$", src) |
    grepl("# nocov$", src) |
    grepl("// nocov$", src) |
    grepl("/* nocov */$", src)
  if (any(drop)) cov <- cov[!drop]

  # Drop whole files based on .covrignore
  # TODO: we drop the directory names here, they are not in the coverage
  if (file.exists(".covrignore")) {
    dfnms <- Sys.glob(readLines(".covrignore"))
    dfnms <- basename(dfnms[file.exists(dfnms)])
    cfnms <- sub(":.*$", "", names(cov))
    drop <- cfnms %in% dfnms
    if (any(drop)) cov <- cov[!drop]
  }

  # Drop # nocov start -> # nocov end intervals
  sources <- covr:::traced_files(cov)

  # Conditional exclusions
  sources <- sapply(
    simplify = FALSE,
    sources,
    function(src) {
      cndex <- grep("#[ ]+nocovif\\b", src$file_lines)
      should_drop <- vlapply(cndex, function(x) {
        cnd <- sub("^.*#[ ]+nocovif\\b(.*)$", "\\1", src$file_lines[cndex])
        cndval <- eval(parse(text = cnd))
        if (cndval) TRUE else FALSE
      })
      drop <- cndex[should_drop]
      if (any(drop)) {
        src$file_lines[drop] <- paste0(src$file_lines[drop], "# nocov")
      }
      src
    }
  )

  source_exclusions <- lapply(sources, function(x) {
    covr:::parse_exclusions(
      x$file_lines,
      exclude_pattern = "#[ ]+nocov\\b",
      exclude_start = "#[ ]+nocov[ ]+start",
      exclude_end = "#[ ]+nocov[ ]+end"
    )
  })
  filenames <- unname(covr:::display_name(cov))
  linum <- as.integer(vcapply(strsplit(names(cov), ":"), "[[", 2))
  drop <- rep(FALSE, length(cov))
  for (filename in names(source_exclusions)) {
    drop[filenames == filename &
         linum %in% source_exclusions[[filename]]] <- TRUE
  }
  if (any(drop)) cov <- cov[!drop]

  cov
}

keep_new <- function(cov) {
  out <- processx::run("git", c("symbolic-ref", "refs/remotes/origin/HEAD"))
  branch <- trimws(sub(".*/", "", out$stdout))
  out2 <- processx::run(
    "git",
    c("diff", "-U0", "-p", branch, "R", if (file.exists("src")) "src")
  )
  diff <- strsplit(out2$stdout, "\n", fixed = TRUE)[[1]]
  new <- structure(list(), names = character())
  for (line in diff) {
    if (grepl("^[+][+][+] ", line)) {
      filename <- sub("^[+][+][+] b/", "", line)
      new[[filename]] <- integer()
    } else if (grepl("^@@ ", line)) {
      pts <- strsplit(line, " ", fixed = TRUE)[[1]]
      add <- strsplit(sub("^[+]", "", pts[3]), ",", fixed = TRUE)[[1]]
      from <- as.integer(add[1])
      len <- as.integer(na.omit(c(add[2], 1))[1])
      new[[filename]] <- c(new[[filename]], seq(from = from, length.out = len))
    }
  }

  filenames <- sub("^[.]/", "", unname(covr:::display_name(cov)))
  linum <- as.integer(vcapply(strsplit(names(cov), ":"), "[[", 2))

  keep <- filenames %in% names(new) &
    mapply(linum, new[filenames], FUN = function(l, nl) l %in% nl)

  cov <- cov[keep]
  cov
}

merge_coverage <- function(x, files) {
  names <- names(x)
  for (cf in files) {
    y <- suppressWarnings(readRDS(cf))
    for (name in intersect(names, names(y))) {
      x[[name]]$value <- x[[name]]$value + y[[name]]$value
    }
    for (name in setdiff(names(y), names)) {
      x[[name]] <- y[[name]]
    }
    names <- union(names, names(y))
    y <- NULL
  }
  x
}

gcov_cleanup <- function() {
  files <- list.files(pattern = "[.]gcda$", full.names = TRUE)
  unlink(files)
}

# ------------------------------------------------------------------------

interactive_reporter <- R6::R6Class("interactive_reporter",
  inherit = testthat::ProgressReporter,
  public = list(
    package = "<unknown>",
    spin_state = 1L,
    initialize = function(package, ...) {
      self$package <- package
      super$initialize(...)
    },

    show_header = function() {
      self$cat_line(strpad(
        paste(cli::symbol$pointer, self$package, "test suite "),
        chr = "─"
      ))
      self$cat_line()
    },

    add_result = function(context, test, result) {
      self$ctxt_n <- self$ctxt_n + 1L

      if (testthat:::expectation_broken(result)) {
        self$n_fail <- self$n_fail + 1
        self$ctxt_n_fail <- self$ctxt_n_fail + 1
        self$ctxt_issues$push(result)
      } else if (testthat:::expectation_skip(result)) {
        self$n_skip <- self$n_skip + 1
        self$ctxt_n_skip <- self$ctxt_n_skip + 1
        self$ctxt_issues$push(result)
        self$skips$push(result$message)
      } else if (testthat:::expectation_warning(result)) {
        self$n_warn <- self$n_warn + 1
        self$ctxt_n_warn <- self$ctxt_n_warn + 1
        self$ctxt_issues$push(result)
      } else {
        self$n_ok <- self$n_ok + 1
        self$ctxt_n_ok <- self$ctxt_n_ok + 1
      }

      self$local_user_output()
      if (! testthat:::expectation_success(result)) {
        self$cat_tight("\r")
        self$cat_line(paste(issue_summary(result), collapse = "\n"))
      }
      self$show_status()
    },

    end_context = function(context) {
      if (self$ctxt_issues$size() > 0) {
        self$local_user_output()
        self$cat_line("\r", strpad(""))
        self$show_status(complete = TRUE)
      }
    },

    show_status = function(complete = FALSE, ...) {
      if (!complete && !self$should_update()) {
        return()
      }

      self$spin_state <- self$spin_state + 1L
      spin <- spinner(self$frames, self$spin_state)
      sm <- summary_line(self$n_ok, self$n_fail, self$n_warn, self$n_skip)
      self$cat_tight("\r", spin, " ", sm)
    },

    end_reporter = function() {
      line <- summary_line(
        self$n_ok, self$n_fail, self$n_warn, self$n_skip
      )

      time <- proc.time() - self$start_time
      msg <- paste0(
        line, "  ",
        cli::col_grey("[", sprintf("%.1f s", time[[3]]), "]")
      )

      self$cat_line("\r", strpad(msg))
    }
  )
)

summary_line <- function(n_ok, n_fail, n_warn, n_skip) {
  pass <- if (n_ok) format_type("pass") else "PASS"
  fail <- if (n_fail) format_type("error") else "FAIL"
  warn <- if (n_warn) format_type("warning") else "WARN"
  skip <- if (n_skip) format_type("skip") else "SKIP"

  passn <- paste0(pass, " x", n_ok)
  failn <- paste0(fail, " x", n_fail)
  warnn <- paste0(warn, " x", n_warn)
  skipn <- paste0(skip, " x", n_skip)

  paste(
    sep = "  ",
    if (n_ok)   passn else cli::col_grey(passn),
    if (n_fail) failn else cli::col_grey(failn),
    if (n_warn) warnn else cli::col_grey(warnn),
    if (n_skip) skipn else cli::col_grey(skipn)
  )
}

issue_summary <- function(x) {
  type <- asNamespace("testthat")$expectation_type(x)
  loc <- unclass(asNamespace("testthat")$expectation_location(x))
  frm <- unlist(strsplit(format(x), "\n"))
  header <- paste0(
    format_type(type), " › ", loc,
    " » ", x$test
  )

  if (type == "skip") {
    header <- paste0(
      header,
      cli::col_grey(" [", sub("^Reason: ", "", x$message), "]"))
    frm <- character()
  }

  if (type == "error" || type == "failure" || type == "warning") {
    frm <- format_stack(frm)
  }

  paste0(c(header, frm), collapse = "\n")
}

styles <- new.env(parent = emptyenv())

style_orange <- function(...) {
  if (is.null(styles$orange)) {
    styles$orange <- cli::make_ansi_style("orange")
  }
  styles$orange(...)
}

style_bg_blue <- function(...) {
  if (is.null(styles$bg_blue)) {
    styles$bg_blue <- cli::make_ansi_style("#0000ff", bg = TRUE)
  }
  styles$bg_blue(...)
}

style_bg_orange <- function(...) {
  if (is.null(styles$bg_orange)) {
    styles$bg_orange <- cli::make_ansi_style("orange", bg = TRUE)
  }
  styles$bg_orange(...)
}

style_bg_green <- function(...) {
  if (is.null(styles$bg_green)) {
    styles$bg_green <- cli::make_ansi_style("darkgreen", bg = TRUE)
  }
  styles$bg_green(...)
}

style_bg_grey <- function(...) {
  if (is.null(styles$bg_grey)) {
    styles$bg_grey <- cli::make_ansi_style("#404040", bg = TRUE)
  }
  styles$bg_grey(...)
}

format_type <- function(type) {
  switch(type,
    pass = style_bg_green(cli::col_white("PASS")),
    success = style_bg_green(cli::col_white("PASS")),
    skip = style_bg_blue(cli::col_white("SKIP")),
    error = cli::bg_red(cli::col_white("FAIL")),
    failure = cli::bg_red(cli::col_white("FAIL")),
    warning = style_bg_orange(cli::col_white("WARN"))
  )
}

format_stack <- function(lines) {
  bt <- grep("Backtrace:", lines)[1]
  if (! is.na(bt)) {
    lines[bt] <- "Backtrace:"
    if (bt > 1) {
      errmsg <- seq(1, bt - 1, by = 1)
      lines <- c(strwrap(lines[errmsg]), lines[bt:length(lines)])
    }
  }

  lines <- lines[lines != ""]
  paste0("       ", lines)
}

spinner <- function(frames, i) {
  frames[((i - 1) %% length(frames)) + 1]
}

# ------------------------------------------------------------------------

create_coverage_table <- function(rcv, filter = NULL, pr = FALSE) {
  byline <- covr::tally_coverage(rcv, by = "line")
  byexpr <- covr::tally_coverage(rcv, by = "expression")

  numline <- tapply(byline$value, byline$filename, length)
  numexpr <- tapply(byexpr$value, byexpr$filename, length)
  covline <- tapply(byline$value, byline$filename, function(x) sum(x > 0))
  covexpr <- tapply(byexpr$value, byexpr$filename, function(x) sum(x > 0))
  pctfun <- function(x) (sum(x > 0) / length(x)) * 100
  pctline <- tapply(byline$value, byline$filename, pctfun)
  pctexpr <- tapply(byexpr$value, byexpr$filename, pctfun)

  pkgline <- pctfun(byline$value)
  pkgexpr <- pctfun(byexpr$value)

  key <- sort(names(pctline))

  uncov <- lapply(key, function(fn) {
    sel <- byline[byline$filename == fn, , drop = FALSE]
    sel <- sel[order(sel$line), , drop = FALSE]
    find_zero_ranges(sel$line, sel$value)
  })

  sm_nm <- "All files"
  sm_tot_lines <- length(byline$value)
  sm_tot_exprs <- length(byexpr$value)
  sm_cov_lines <- sum(byline$value > 0)
  sm_cov_exprs <- sum(byexpr$value > 0)
  sm_pct_lines <- sm_cov_lines / sm_tot_lines * 100
  sm_pct_exprs <- sm_cov_exprs / sm_tot_exprs * 100

  d_lines <- fs::path_rel(dirname(byline$filename), getwd())
  d_exprs <- fs::path_rel(dirname(byexpr$filename), getwd())
  udirs <- unique(c(d_lines, d_exprs))
  bd_tot_lines <- tapply(byline$value, d_lines, length)
  bd_tot_exprs <- tapply(byexpr$value, d_exprs, length)
  bd_cov_lines <- tapply(byline$value, d_lines, function(x) sum(x > 0))
  bd_cov_exprs <- tapply(byexpr$value, d_exprs, function(x) sum(x > 0))
  bd_pct_lines <- tapply(byline$value, d_lines, pctfun)
  bd_pct_exprs <- tapply(byexpr$value, d_exprs, pctfun)

  fn <- fs::path_rel(key, getwd())
  tab <- data.frame(
    stringsAsFactors = FALSE,
    file = c(sm_nm, paste0(udirs, "/"), fn),
    tot_lines = c(sm_tot_lines, bd_tot_lines, numline[key]),
    tot_exprs = c(sm_tot_exprs, bd_tot_exprs, numexpr[key]),
    cov_lines = c(sm_cov_lines, bd_cov_lines, covline[key]),
    cov_exprs = c(sm_cov_exprs, bd_cov_exprs, covexpr[key]),
    pct_lines = c(sm_pct_lines, bd_pct_lines, pctline[key]),
    pct_exprs = c(sm_pct_exprs, bd_pct_exprs, pctexpr[key]),
    uncovered = I(c(vector(length(udirs) + 1, mode = "list"), uncov))
  )

  # Order again, so the directory summaries are at the right place
  tab <- tab[c(1, order(tab$file[-1]) + 1L), , drop = FALSE]

  if (!is.null(filter) && !pr) {
    keep <- tab$pct_lines > 0 | tab$pct_exprs > 0
    keep[1] <- TRUE
    tab <- tab[keep, ]
  }

  class(tab) <- c("coverage_table", class(tab))
  tab
}

# key is ordered

find_zero_ranges <- function(key, value) {
  out <- list()
  start <- end <- NA_integer_
  for (i in seq_along(key)) {
    if (value[i] == 0 && is.na(start)) {
      start <- end <- key[i]
    } else if (value[i] == 0 && !is.na(start)) {
      end <- key[i]
    } else if (value[i] != 0 && !is.na(start)) {
      out[[length(out) + 1]] <- start:end
      start <- end <- NA_integer_
    } else if (value[i] != 0 && is.na(start)) {
      # Nothing to do
    }
  }

  if (!is.na(start)) out[[length(out) + 1]] <- start:end

  out
}

#' @export

print.coverage_table <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}

cov_col <- function(txt, val) {
  ifelse(
    val < 75,
    style_orange(txt),
  ifelse(val < 95, cli::col_blue(txt), txt)
  )
}

#' @export

format.coverage_table <- function(x, ...) {
  dr <- dirname(x$file)
  fn <- x$file
  fn[dr == "."] <- x$file[dr == "."]
  px <- ifelse(
    fn == "All files",
    "",
    ifelse(last_char(fn) == "/", " ", "  ")
  )
  fn <- paste0(px, fn)

  bl <- format_pct(x$pct_lines)
  be <- format_pct(x$pct_exprs)

  cffn <- ffn <- format(c("code coverage", "", fn, "", "total"))
  cfbl <- fbl <- format(c("% lines", "", bl, "", bl[1]), justify = "right")
  cfbe <- fbe <- format(c("% exprs", "", be, "", be[1]), justify = "right")

  mid <- 3:(length(ffn) - 2)
  cffn[mid] <- cov_col(ffn[mid], pmin(x$pct_lines, x$pct_exprs))
  cfbl[mid] <- cov_col(fbl[mid], x$pct_lines)
  cfbe[mid] <- cov_col(fbe[mid], x$pct_exprs)

  lines <- paste0(
    cffn, " │ ",
    cfbl, " │ ",
    cfbe, " │ "
  )

  maxw <- max(cli::ansi_nchar(lines, type = "width"))
  cw <- cli::console_width()

  uc <- mapply(format_uncovered, x$uncovered, file = x$file, width = cw - maxw)
  cuc <- cli::ansi_align(
    c("uncovered line #", "", uc, "", ""),
    width = max(cli::ansi_nchar(uc, "width"))
  )
  cuc[mid] <- cov_col(cuc[mid], pmin(x$pct_lines, x$pct_exprs))
  lines <- paste0(lines, cuc)

  lines[1] <- cli::style_bold(style_bg_grey(cli::col_white(lines[1])))
  tot <- min(x$pct_lines[1], x$pct_exprs[1])
  if (tot < 75) {
    lines[length(lines)] <- cli::bg_red(lines[length(lines)])
  } else if (tot < 95) {
    lines[length(lines)] <-
      style_bg_orange(cli::col_white(lines[length(lines)]))
  } else {
    lines[length(lines)] <-
      style_bg_green(cli::col_white(lines[length(lines)]))
  }
  lines[length(lines)] <- cli::style_bold(lines[length(lines)])

  lines
}

format_uncovered <- function(ranges, file, width = 80) {
  rstr <- vapply(ranges, FUN.VALUE = character(1), function(r) {
    if (length(r) == 1) as.character(r) else paste0(r[1], "-", r[length(r)])
  })

  ls <- if (Sys.getenv("R_CLI_HYPERLINK_STYLE") == "iterm") "#" else ":"
  rstr <- vapply(rstr, function(x) {
    file <- normalizePath(file, mustWork = FALSE)
    link <- paste0("file://", file, ls, sub("[-].*$", "", x))
    cli::format_inline("{.href [{x}]({link})}")
  }, character(1))

  rstr[- length(rstr)] <- paste0(rstr[- length(rstr)], ", ")

  if (length(rstr) >= 3) {
    cumw <- cumsum(cli::ansi_nchar(rstr, "width"))
    if (cumw[length(cumw)] > width) {
      last <- rev(which(cumw <= width - 3))[1]
      rstr <- c(rstr[1:last], "...")
    }
  }

  paste(rstr, collapse = "")
}

format_pct <- function(x) {
  paste0(format(x, width = 3, digits = 3, justify = "right"), "%")
}
