markdown <- function(path = ".", coverage = NULL, output = NULL) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")
  setup <- reload_setup("coverage")

  ttl <- find.package(utils::packageName())
  index <- file.path(ttl, "report.md")
  if (!file.exists(index)) {
    index <- file.path(ttl, "inst/report.md")
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

  # correct order
  coveragex <- coveragex[c(1, path_order(coveragex$path[-1]) + 1), ]

  # Note original path w/o formatting
  paths <- coveragex$path

  # summaries bold
  is_sum_row <- paths == "All files" | endsWith(paths, "/")
  coveragex$path <- ifelse(
    is_sum_row,
    paste0("**", coveragex$path, "**"),
    coveragex$path
  )

  # Links for file names
  baseurl <- paste0(
    Sys.getenv("GITHUB_SERVER_URL"),
    "/",
    Sys.getenv("GITHUB_REPOSITORY"),
    "/blob/",
    Sys.getenv("GITHUB_SHA"),
    "/"
  )
  coveragex$path <- ifelse(
    is_sum_row,
    coveragex$path,
    paste0("[`", coveragex$path, "`](", baseurl, "/", paths, ")")
  )

  # indent according to directory structure
  coveragex$path <- paste0(
    strrep("&nbsp;", str_count(sub("/$", "", paths), "/")),
    coveragex$path
  )

  data <- with(
    coveragex,
    paste0(
      "|",
      path,
      "|",
      lines_covered,
      "/",
      code_lines,
      "|",
      format_funcs(functions_hit, function_count),
      "|",
      format_percent(percent_covered / 100),
      " ",
      format_emoji(percent_covered),
      "|",
      collapse = "\n"
    )
  )

  total <- sum(coverage$lines_covered) / sum(coverage$code_lines)
  total_percent <- format_percent(total)
  total_emoji <- format_emoji(total)

  total_funcs <- sum(coverage$functions_hit) / sum(coverage$function_count)
  total_funcs_percent <- format_percent(total_funcs)
  total_funcs_emoji <- format_emoji(total_funcs)

  tests <- attr(coverage, "test_results")
  test_data <- markdown_test_summary(tests)
  byfile <- testthat_results_by_file(test_data)

  totals <- list(
    ok = sum(byfile$success),
    fail = sum(byfile$broken),
    warn = sum(byfile$warning),
    skip = sum(byfile$skip)
  )

  test_files <- with(
    byfile,
    paste0(
      "|[",
      file,
      "](",
      baseurl,
      "/tests/testthat/",
      file,
      ")|",
      broken,
      "|",
      warning,
      "|",
      skip,
      "|",
      success,
      "|",
      collapse = "\n"
    )
  )

  test_details <- ""

  vars <- list(
    package = pkgname,
    total_percent = total_percent,
    total_lines_covered = sum(coverage$lines_covered),
    total_lines = sum(coverage$code_lines),
    total_funcs_percent = total_funcs_percent,
    total_funcs_hit = sum(coverage$functions_hit),
    total_funcs = sum(coverage$function_count),
    data = data,
    test_files = test_files,
    test_details = test_details,
    total_emoji = total_emoji,
    total_funcs_emoji = total_funcs_emoji,
    emoji = list(ok = "✅", fail = "❌", warn = "⚠️", skip = "🦘"),
    n = totals
  )

  lns <- glue::glue_data(
    vars,
    paste(lns0, collapse = "\n"),
    .open = "{{",
    .close = "}}"
  )

  output <- output %||%
    file.path(setup$dir, markdown_dir_name, paste0(pkgname, ".md"))

  mkdirp(dirname(output))
  writeLines(lns, output)

  invisible(output)
}

format_funcs <- function(hit, count) {
  percent <- ifelse(count == 0, 100, hit / count * 100)
  paste0(hit, "/", count, " ", format_emoji(percent))
}

format_emoji <- function(x) {
  ifelse(x == 100, "⭐", ifelse(x >= 95, "✅", ifelse(x >= 75, "🛠️", "❌")))
}

markdown_test_summary <- function(results) {
  results <- lapply(results, gha_summarize_test)
  totals <- list(
    n_fail = sum(vapply(results, "[[", integer(1), "n_fail")),
    n_warn = sum(vapply(results, "[[", integer(1), "n_warn")),
    n_skip = sum(vapply(results, "[[", integer(1), "n_skip")),
    n_ok = sum(vapply(results, "[[", integer(1), "n_ok")),
    real = sum(vapply(results, "[[", double(1), "real"))
  )

  # summary -----------------------------------------------------------------
  gha_summary_write("### Test summary")
  gha_summary_write()
  gha_summary_write("| FAIL | WARN | SKIP | PASS | Time |")
  gha_summary_write("|-----:|-----:|-----:|-----:|:-----|")
  gha_summary_write(
    c("|", if (totals$n_fail > 0) totals$n_fail),
    c("|", if (totals$n_warn > 0) totals$n_warn),
    c("|", if (totals$n_skip > 0) totals$n_skip),
    c("|", totals$n_ok),
    c("|", num_exact(totals$real, 2), "|")
  )

  # issue details -----------------------------------------------------------
  gha_summary_write()
  gha_summary_write("<details>")
  gha_summary_write("<summary>Test details</summary>")

  gha_summary_write("")
  gha_summary_write()
  gha_summary_write("| File | Test | FAIL | WARN | SKIP | PASS | Time |")
  gha_summary_write("|:-----|:-----|-----:|-----:|-----:|-----:|:-----|")

  issues <- Filter(function(x) length(x$results) != x$n_ok, results)
  for (issue in issues) {
    gha_summary_write(
      c("|", issue$file),
      c("|", md_escape(issue$test)),
      c("|", if (totals$n_fail > 0) issue$n_fail),
      c("|", if (totals$n_warn > 0) issue$n_warn),
      c("|", if (totals$n_skip > 0) issue$n_skip),
      c("|", issue$n_ok),
      c("|", num_exact(issue$real, 2), "|")
    )
  }
  gha_summary_write()
  gha_summary_write("</details>")
  gha_summary_write()

  invisible(results)
}

# Helpers ----------------------------------------------------------------------

gha_summarize_test <- function(test) {
  test$n_fail <- test$n_skip <- test$n_warn <- test$n_ok <- 0L
  for (exp in test$results) {
    if (expectation_broken(exp)) {
      test$n_fail <- test$n_fail + 1L
    } else if (expectation_skip(exp)) {
      test$n_skip <- test$n_skip + 1L
    } else if (expectation_warning(exp)) {
      test$n_warn <- test$n_warn + 1L
    } else {
      test$n_ok <- test$n_ok + 1L
    }
  }

  test
}

gha_path <- function() {
  nope <- c("false", "no", "off", "n", "0", "nope", "nay")
  if (tolower(Sys.getenv("TESTTHAT_GHA_SUMMARY")) %in% nope) {
    return()
  }

  if ((out <- Sys.getenv("GITHUB_STEP_SUMMARY")) == "") {
    return()
  }
  out
}


gha_summary_write <- function(...) {
  path <- gha_path()
  if (is.null(path)) {
    return()
  }

  string <- paste0(c(..., "\n"), collapse = "")
  Encoding(string) <- "unknown"
  cat(string, file = path, append = TRUE)
}

md_escape <- function(x) {
  x <- gsub("|", "\\|", x, fixed = TRUE)
  x <- gsub("\n", " ", x, fixed = TRUE)
  x
}
