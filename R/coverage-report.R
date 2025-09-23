#' Generate a HTML coverage report
#'
#' @inheritParams reload
#' @param coverage Test coverage results. If `NULL` then the last
#'   results are used via [last()].
#' @param output Output file. This is the HTML index file. The rest of
#'   of the HTML files for the source code files will be placed next
#'   to it. By default it is generated in the dev directory.
#' @param show Whether to open a browser window to show the HTML
#'   report.
#' @return The path of the output file, invisibly.
#'
#' @export

report <- function(
  path = ".",
  coverage = NULL,
  output = NULL,
  show = interactive()
) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")
  setup <- reload_setup("coverage")

  # our pkgload::system.file() monkey-patch is buggy, so use find.package()
  # need to try the load_all path as well
  ttl <- find.package(utils::packageName())
  index <- file.path(ttl, "report/index.html")
  if (!file.exists(index)) {
    index <- file.path(ttl, "inst/report/index.html")
  }
  lns0 <- readLines(index)

  sm <- attr(coverage, "summary")
  # only add directories if there are more than just R/
  if (nrow(sm) == 2) {
    sm <- sm[1, ]
  }
  names(sm)[1] <- "path"
  coveragex <- coverage[, names(sm)]
  coveragex <- rbind(sm, coveragex)

  metadata <- attr(coverage, "metadata")
  pkgname <- metadata$setup$pkgname
  data <- with(
    coveragex,
    paste0(
      "[",
      paste0(
        "[ \"",
        path,
        "\", ",
        line_count,
        ", ",
        code_lines,
        ", ",
        lines_covered,
        ", ",
        code_lines - lines_covered,
        ", ",
        round(total_hits / code_lines, 2),
        ", \"",
        functions_hit,
        "/",
        function_count,
        "\", \"",
        format_percent(percent_covered / 100),
        "\" ]",
        collapse = ",\n"
      ),
      "]"
    )
  )

  total_percent <- format_percent(
    sum(coverage$lines_covered) / sum(coverage$code_lines)
  )

  total_funcs_percent <- format_percent(
    sum(coverage$functions_hit) / sum(coverage$function_count)
  )

  status <- if (is.na(total_percent)) {
    "na"
  } else if (total_percent < 0.75) {
    "bad"
  } else if (total_percent < 0.95) {
    "ok"
  } else {
    "good"
  }

  vars = list(
    package = pkgname,
    total_percent = total_percent,
    total_lines_covered = sum(coverage$lines_covered),
    total_lines = sum(coverage$code_lines),
    total_funcs_percent = total_funcs_percent,
    total_funcs_hit = sum(coverage$functions_hit),
    total_funcs = sum(coverage$function_count),
    status = status,
    data = data
  )

  lns <- glue::glue_data(
    vars,
    paste(lns0, collapse = "\n"),
    .open = "{{",
    .close = "}}"
  )

  output <- output %||%
    file.path(setup$dir, report_dir_name, paste0(pkgname, "-report.html"))

  mkdirp(dirname(output))
  writeLines(lns, output)

  for (path in coverage$path) {
    report_file_(
      path,
      coverage = coverage,
      path = ".",
      output_dir = dirname(output),
      show = FALSE,
      setup = setup
    )
  }

  if (show) {
    if (
      requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()
    ) {
      rstudioapi::viewer(output)
    } else {
      utils::browseURL(output)
    }
  }

  invisible(output)
}

format_percent <- function(x) {
  x[is.na(x)] <- 1
  sprintf("%0.2f%%", x * 100)
}

report_file <- function(
  code_file = NULL,
  coverage = NULL,
  path = ".",
  output_dir = NULL,
  show = interactive()
) {
  report_file_(code_file, coverage, path, output_dir, show)
}

report_file_ <- function(
  code_file = NULL,
  coverage = NULL,
  path = ".",
  output_dir = NULL,
  show = interactive(),
  setup = NULL
) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")
  setup <- setup %||% reload_setup("coverage")

  pathidx <- match(code_file, coverage$path)
  if (is.na(pathidx)) {
    stop("No test coverage data for file '", code_file, "'.")
  }

  metadata <- attr(coverage, "metadata")
  pkgname <- metadata$setup$pkgname
  total_percent <- format_percent(
    sum(coverage$lines_covered) / sum(coverage$code_lines)
  )
  file_percent <-
    sum(coverage$lines_covered[pathidx]) / sum(coverage$code_lines[pathidx])
  file_status <- if (is.na(file_percent)) {
    "na"
  } else if (file_percent < 0.75) {
    "bad"
  } else if (file_percent < 0.95) {
    "ok"
  } else {
    "good"
  }

  code <- escape_source(coverage$lines[[pathidx]][["lines"]])
  cov <- coverage$lines[[pathidx]][["coverage"]]
  funtab <- coverage$funs[[pathidx]]
  funcs <- if (nrow(funtab) > 0) {
    paste0(
      "[",
      paste0(
        "[ ",
        funtab$line1,
        ", ",
        funtab$coverage,
        ", ",
        ifelse(is.na(funtab$name), "NaN", paste0("\"", funtab$name, "\"")),
        "]",
        collapse = ",\n"
      ),
      "]"
    )
  }

  vars <- list(
    pkgname = pkgname,
    total_percent = total_percent,
    file_lines = coverage$code_lines[pathidx],
    file_lines_covered = coverage$lines_covered[pathidx],
    file_percent = format_percent(file_percent),
    file_funcs = nrow(funtab),
    file_funcs_hit = sum(funtab$coverage > 0, na.rm = TRUE),
    file_funcs_percent = format_percent(
      sum(funtab$coverage > 0, na.rm = TRUE) / nrow(funtab)
    ),
    path = code_file,
    lines = paste0("[", paste0("\"", code, "\"", collapse = ",\n"), "]"),
    coverage = paste0(
      "[",
      paste(ifelse(is.na(cov), "NaN", cov), collapse = ", "),
      "]"
    ),
    language = get_language_from_path(code_file),
    file_status = file_status,
    funcs = funcs %||% "[]"
  )

  # our pkgload::system.file() monkey-patch is buggy, so use find.package()
  # need to try the load_all path as well
  ttl <- find.package(utils::packageName())
  srcpath <- file.path(ttl, "report/source.html")
  if (!file.exists(srcpath)) {
    srcpath <- file.path(ttl, "inst/report/source.html")
  }
  lns0 <- readLines(srcpath)

  lns <- glue::glue_data(
    vars,
    paste(lns0, collapse = "\n"),
    .open = "{{",
    .close = "}}"
  )

  fn <- paste0("file-", utils::URLencode(code_file, reserved = TRUE), ".html")
  output_dir <- output_dir %||% file.path(setup$dir, report_dir_name)
  output <- file.path(output_dir, fn)

  mkdirp(dirname(output))
  writeLines(lns, output)

  if (show) {
    utils::browseURL(output)
  }

  invisible(output)
}

escape_source <- function(x) {
  x <- gsub("\\", "\\\\", fixed = TRUE, x)
  x <- gsub("\"", "\\\"", fixed = TRUE, x)
  x
}

get_language_from_path <- function(path) {
  ext <- tolower(tools::file_path_sans_ext(path))
  res <- rep(NA_character_, length(path))
  res[ext == "r"] <- "r"
  res[ext == "c"] <- "c"
  res[ext %in% c("cc", "c++", "cxx", "hcc", "cpp", "hpp", "h", "hh")] <- "c++"
  res[ext %in% c("f", "f90", "f77")] <- "fortran"
  res[ext %in% c("m", "mm")] <- "objc"
  ifelse(is.na(res), "", paste0("language-", res))
}
