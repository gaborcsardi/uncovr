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

  saveRDS(results, file.path(test_directory, "results.rds"), version = 2)

  invisible(results)
}

non_interactive_reporter <- R6::R6Class(
  "non_interactive_reporter",
  inherit = testthat::ProgressReporter,
  public = list(
    package = "<unknown>",
    file_name = NULL,
    test_case = NULL,
    test_line = NULL,
    width = NULL,
    started = FALSE,
    test_start_time = NULL,
    prev_type = "foo",
    cli_num_colors = NULL,
    num_dots = 0,

    initialize = function(package, ...) {
      self$package <- package
      super$initialize(...)
      self$cli_num_colors <- cli::num_ansi_colors()
    },

    show_header = function() {
      withr::local_options(cli.num_colors = self$cli_num_colors)
      self$cat_line(strpad(
        paste(cli::symbol$pointer, self$package, "test suite "),
        chr = "\u2500"
      ))
    },

    start_file = function(file) {
      self$file_name <- file
    },

    start_test = function(context, test) {
      self$test_case <- test
      self$test_line <- get_test_file_position(self$file_name)
      self$started <- FALSE
      self$test_start_time <- proc.time()
      self$num_dots <- 0
    },

    test_header = function(file, loc, test) {
      withr::local_options(cli.num_colors = self$cli_num_colors)
      fn <- context_name(file)
      header <- paste0(
        "     \u203a ",
        fn,
        " ",
        format_loc(loc),
        " \u00bb ",
        test,
        " "
      )
      self$width <- cli::ansi_nchar(header, type = "width")
      self$cat_tight(header)
      self$started <- TRUE
    },

    add_result = function(context, test, result) {
      withr::local_options(cli.num_colors = self$cli_num_colors)

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
          if (self$prev_type != "success") {
            self$cat_line()
          }
          self$test_header(self$file_name, self$test_line, test)
        }

        if (self$num_dots == 5) {
          self$num_dots <- 0
          if (self$width >= 70L) {
            self$cat_tight("\n       ")
            self$width <- 7L
          } else {
            self$cat_tight(" ")
          }
        }

        self$cat_tight(".")
        self$num_dots <- self$num_dots + 1L
        self$width <- self$width + 1L
      }

      if (type != "pass" && type != "success") {
        self$num_dots <- 0
        self$end_test()
        ftp <- format_type(type)
        smy <- issue_summary(result)
        # keep skips together
        if (self$prev_type != type || type != "skip") {
          self$cat_line()
        }
        self$cat_line(smy)
        # self$cat_line(strpad(ftp, width = 80 - self$width, align = "right"))
        self$started <- FALSE
      }

      self$prev_type <- type
    },

    end_test = function(context, test) {
      if (self$started) {
        withr::local_options(cli.num_colors = self$cli_num_colors)
        time <- proc.time() - self$test_start_time
        lbl <- prettyunits::pretty_dt(as.difftime(time[[3]], units = "secs"))
        line <- cli::col_grey(" [", lbl, "]")
        self$cat_line(line)
        self$started <- FALSE
      }
    },

    end_reporter = function() {
      withr::local_options(cli.num_colors = self$cli_num_colors)
      line <- summary_line(
        self$n_ok,
        self$n_fail,
        self$n_warn,
        self$n_skip
      )

      time <- proc.time() - self$start_time
      lbl <- prettyunits::pretty_dt(as.difftime(time[[3]], units = "secs"))
      msg <- paste0(
        line,
        "  ",
        cli::col_grey("[", lbl, "]")
      )

      self$cat_line(strpad(msg))
      self$cat_line()
    },

    end_file = function() {
      withr::local_options(cli.num_colors = self$cli_num_colors)
      if (self$started) {
        self$cat_tight("\n")
      }
      self$cat_tight("\n")
    },

    start_context = function(context) {},
    end_context = function(context) {}
  )
)

format_loc <- function(x) {
  format(x)
}
