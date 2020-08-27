
#' @export

test <- function(filter = NULL, ...) {
  pkg <- read.dcf("DESCRIPTION")[, "Package"][[1]]

  on.exit(setHook(packageEvent(pkg, "onLoad"), NULL, "replace"), add = TRUE)
  setHook(packageEvent(pkg, "onLoad"), function(...) {
    ns <- .getNamespace(as.name(pkg))
    asNamespace("covr")$trace_environment(ns)
  })
  pkgload::load_all()

  asNamespace("covr")$clear_counters()
  withr::local_envvar(c(R_COVR = "true"))
  withr::local_envvar(c(NOT_CRAN = "true"))

  cli::ansi_hide_cursor()
  on.exit(cli::ansi_show_cursor(), add = TRUE)

  tryCatch(
    results <- testthat::test_dir(
      "tests/testthat",
      reporter = interactive_reporter$new(pkg),
      filter = filter,
      load_helpers = FALSE,
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

  cv <- as.list(asNamespace("covr")$.counters)
  cv <- cv[vapply(cv, function(x) length(x$value) != 0, logical(1))]
  class(cv) <- "coverage"

  invisible(structure(
    list(tests = results, coverage = cv),
    class = "testthat_coverage"
  ))
}

interactive_reporter <- R6::R6Class("interactive_reporter",
  inherit = testthat::ProgressReporter,
  public = list(
    package = "<unknown>",
    initialize = function(package, ...) {
      self$package <- package
      super$initialize(...)
    },

    show_header = function() {
      self$cat_line(paste("❯", self$package, "test suite"))
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

      sm <- summary_line(self$n_ok, self$n_fail, self$n_warn, self$n_skip)
      self$cat_tight("\r", sm)
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

      self$cat_line("\r", msg)
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
  loc <- strsplit(asNamespace("testthat")$expectation_location(x), ":")[[1]]
  loc[1] <- context_name(loc[1])
  frm <- unlist(strsplit(format(x), "\n"))
  header <- paste0(
    format_type(type), " › ", loc[1], " ", format_loc(loc[2]),
    " » ", x$test
  )

  if (type == "skip") {
    header <- paste0(
      header,
      cli::col_grey(" [", sub("^Reason: ", "", x$message), "]"))
    frm <- character()
  }

  if (type == "error" || type == "warning") {
    frm <- format_stack(frm)
  }

  paste0(c(header, frm), collapse = "\n")
}

styles <- new.env(parent = emptyenv())
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

format_type <- function(type) {
  switch(type,
    pass = style_bg_green(cli::col_white("PASS")),
    skip = style_bg_blue(cli::col_white("SKIP")),
    error = cli::bg_red(cli::col_white("FAIL")),
    warning = style_bg_orange(cli::col_white("WARN"))
  )
}

format_loc <- function(loc) {
  cli::col_grey(
    format(paste0("@", loc), width = 4, justify = "right")
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

  paste0("       ", lines)
}

context_name <- function(filename) {
  # Remove test- prefix
  filename <- sub("^test[-_]", "", filename)
  # Remove terminal extension
  filename <- sub("[.][Rr]$", "", filename)
  filename
}

# We need to import something from covr, otherwise the
# check freaks out
fix_check <- function() covr::package_coverage

strpad <- function(x, width = cli::console_width()) {
  n <- pmax(0, width - crayon::col_nchar(x))
  paste0(x, strrep(" ", n))
}
