
test_non_interactive <- function(pkg = NULL, filter = NULL, ...) {
  pkg <- pkg %||% read.dcf("DESCRIPTION")[, "Package"][[1]]

  if (file.exists("testthat")) {
    test_directory <- "testthat"
  } else if (file.exists("tests")) {
    test_directory <- "tests/testthat"
  } else {
    stop("Could not find testthat tests")
  }

  results <- testthat::test_dir(
    test_directory,
    reporter = non_interactive_reporter$new(pkg),
    filter = filter,
    load_helpers = TRUE,
    stop_on_failure = TRUE,
    env = new.env(parent = asNamespace(pkg)),
    ...
  )

  saveRDS(results, file.path(test_directory, "results.rds"))

  invisible(results)
}

non_interactive_reporter <- R6::R6Class("non_interactive_reporter",
  inherit = testthat::ProgressReporter,
  public = list(
    package = "<unknown>",
    file_name = NULL,
    test_case = NULL,
    test_line = NULL,
    width = NULL,
    started = FALSE,
    prev_type = "foo",

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

    start_file = function(file) {
      self$file_name <- file
    },

    start_test = function(context, test) {
      self$test_case <- test
      self$test_line <- get_test_file_position(self$file_name)
      self$started <- FALSE
    },

    test_header = function(file, loc, test) {
      fn <- context_name(file)
      header <- paste0("     › ", fn, " ", format_loc(loc), " » ", test, " ")
      self$width <- crayon::col_nchar(header)
      self$cat_tight(header)
      self$started <- TRUE
    },

    add_result = function(context, test, result) {

      type <- asNamespace("testthat")$expectation_type(result)

      if (type == "failure") {
        self$n_fail <- self$n_fail + 1

      } else if (type == "skip") {
        self$n_skip <- self$n_skip + 1

      } else if (type == "warning") {
        self$n_warn <- self$n_warn + 1

      } else {
        self$n_ok <- self$n_ok + 1
        if (!self$started) {
          if (self$prev_type != "success") self$cat_line()
          self$test_header(self$file_name, self$test_line, test)
        }

        if (self$width >= 70L) {
          self$cat_tight("\n       ")
          self$width <- 7L
        }
        self$cat_tight(".")
        self$width <- self$width + 1L
      }

      if (type != "pass" && type != "success") {
        self$end_test()
        ftp <- format_type(type)
        smy <- issue_summary(result)
        # keep skips together
        if (self$prev_type != type || type != "skip") self$cat_line()
        self$cat_line(smy)
        # self$cat_line(strpad(ftp, width = 80 - self$width, align = "right"))
        self$started <- FALSE
      }

      self$prev_type <- type
    },

    end_test = function(context, test) {
      if (self$started) {
        # TODO: timings
        self$cat_line()
        self$started <- FALSE
      }
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

      self$cat_line(strpad(msg))
      self$cat_line()
    },

    end_file = function() {
      if (self$started) self$cat_tight("\n")
      self$cat_tight("\n")
    },

    start_context = function(context) { },
    end_context = function(context) { }
  )
)

format_loc <- function(x) {
  format(x)
}
