CiReporter <- R6::R6Class(
  "CiReporter",
  inherit = testthat::Reporter,
  public = list(
    package = NULL,

    capabilities = list(
      parallel_support = FALSE,
      parallel_updates = FALSE
    ),

    initialize = function(package = NULL, ...) {
      super$initialize(...)
      self$package <- package
    },

    start_reporter = function() {
      self$local_user_output()
      private$started <- proc.time()[["elapsed"]]
      private$n_ok <- 0L
      private$n_fail <- 0L
      private$n_warn <- 0L
      private$n_skip <- 0L

      pkg <- self$package %||% "package"
      head <- paste0(cli::symbol$pointer, " ", pkg, " test suite ")
      width <- max(0, cli::console_width() - cli::ansi_nchar(head))
      private$line(head, strrep("\u2500", width))
      private$blank()
    },

    start_file = function(filename) {
      self$local_user_output()
      if (private$first_file) {
        private$first_file <- FALSE
      } else {
        private$blank()
      }
      private$filename <- filename
      private$context <- test_name(filename)
    },

    start_context = function(context) {
      private$context <- context
    },

    start_test = function(context, test) {
      private$n_success <- 0L
      private$dots <- ""
      private$issues <- list()
      private$test_line <- NA_integer_
      private$test_started <- proc.time()[["elapsed"]]
    },

    add_result = function(context, test, result) {
      type <- expectation_type(result)

      srcref <- result$srcref
      if (inherits(srcref, "srcref")) {
        line <- as.integer(srcref[1])
        if (is.na(private$test_line) || line < private$test_line) {
          private$test_line <- line
        }
      }

      if (expectation_success(result)) {
        private$n_ok <- private$n_ok + 1L
        private$n_success <- private$n_success + 1L
        private$dots <- private$add_dot(private$dots, private$n_success)
      } else {
        if (expectation_broken(result)) {
          private$n_fail <- private$n_fail + 1L
        } else if (type == "warning") {
          private$n_warn <- private$n_warn + 1L
        } else if (type == "skip") {
          private$n_skip <- private$n_skip + 1L
        }
        private$issues <- c(private$issues, list(result))
      }
    },

    end_test = function(context, test) {
      self$local_user_output()
      for (result in private$issues) {
        type <- expectation_type(result)
        if (type == "skip") {
          msg <- sub(
            "^Reason: ",
            "",
            private$first_line(conditionMessage(result))
          )
          private$line(
            paste0(
              private$label(type),
              " \u203a ",
              private$loc_ctx(result),
              " \u00bb ",
              test,
              if (nzchar(msg)) paste0(" [", msg, "]")
            )
          )
          private$prev_skip <- TRUE
        } else {
          private$blank()
          private$line(
            paste0(
              private$label(type),
              " \u203a ",
              private$loc_ctx(result),
              " \u00bb ",
              test
            )
          )
          for (l in strsplit(conditionMessage(result), "\n", fixed = TRUE)[[
            1
          ]]) {
            private$line("       ", l)
          }
          private$blank()
        }
      }

      if (private$n_success > 0L) {
        ctx <- private$context %||% context %||% "?"
        line <- if (is.na(private$test_line)) "" else private$test_line
        dur <- format_duration(proc.time()[["elapsed"]] - private$test_started)
        private$line(
          "     \u203a ",
          ctx,
          " ",
          line,
          " \u00bb ",
          test,
          " ",
          private$dots,
          " ",
          cli::col_grey(paste0("[", dur, "]"))
        )
      }
    },

    end_reporter = function() {
      self$local_user_output()
      dur <- format_duration(proc.time()[["elapsed"]] - private$started)
      private$blank()
      private$line(
        summary_line(
          n_ok = private$n_ok,
          n_fail = private$n_fail,
          n_warn = private$n_warn,
          n_skip = private$n_skip
        ),
        "  ",
        cli::col_grey(paste0("[", dur, "]"))
      )
    }
  ),

  private = list(
    started = NULL,
    test_started = NULL,
    first_file = TRUE,
    prev_blank = TRUE,
    prev_skip = FALSE,
    filename = NULL,
    context = NULL,
    n_ok = 0L,
    n_fail = 0L,
    n_warn = 0L,
    n_skip = 0L,
    n_success = 0L,
    dots = "",
    issues = list(),
    test_line = NA_integer_,

    line = function(...) {
      self$cat_line(...)
      private$prev_blank <- FALSE
      private$prev_skip <- FALSE
    },

    blank = function() {
      if (!private$prev_blank) {
        self$cat_line()
        private$prev_blank <- TRUE
      }
    },

    add_dot = function(dots, n) {
      sep <- if (n > 1L && (n - 1L) %% 5L == 0L) " " else ""
      paste0(dots, sep, ".")
    },

    # 'context line:col', used for the unquoted failure/warning heading.
    loc_ctx = function(result) {
      srcref <- result$srcref
      ctx <- private$context %||% "?"
      if (inherits(srcref, "srcref")) {
        paste0(ctx, " ", srcref[1])
      } else {
        ctx
      }
    },

    first_line = function(x) {
      if (length(x) == 0) {
        return("")
      }
      strsplit(x, "\n", fixed = TRUE)[[1]][1]
    },

    label = function(type) {
      switch(
        type,
        error = cli::bg_red("FAIL"),
        failure = cli::bg_red("FAIL"),
        warning = cli::bg_yellow("WARN"),
        skip = cli::bg_blue("SKIP")
      )
    }
  )
)

format_duration <- function(seconds) {
  if (is.na(seconds) || seconds < 0) {
    seconds <- 0
  }
  if (seconds < 1) {
    paste0(round(seconds * 1000), "ms")
  } else {
    paste0(format(round(seconds, 1), nsmall = 1), "s")
  }
}
