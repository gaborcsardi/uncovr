# https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-commands#adding-a-job-summary
gha_job_summary_size_limit <- 1048576
gha_job_summary_trunc_note <- "\n\n`[truncated for GitHub job summary limit]`"

#' Generate a markdown test coverage report
#'
#' This is to create a [GitHub Actions](https://github.com/features/actions)
#' [job summary](
#'   https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-commands#adding-a-job-summary).
#'
#' @inheritParams reload
#' @param coverage Test coverage results. If `NULL` then the last
#'   results are used via [last()].
#' @param output Output file. By default it is placed in the dev directory.
#' @param truncate Whether to truncate the file if it is longer than the
#'   GitHub Actions limit (currently `r gha_job_summary_size_limit` bytes).
#' @return The path of the output file, invisibly.
#'
#' @export

markdown <- function(
  path = ".",
  coverage = NULL,
  output = NULL,
  truncate = TRUE
) {
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
  by_file <- testthat_results_by_file(tests)
  by_test <- testthat_results_by_test(tests)

  totals <- list(
    ok = sum(by_file$success),
    fail = sum(by_file$broken),
    warn = sum(by_file$warning),
    skip = sum(by_file$skip)
  )

  test_files <- format_results(by_file, baseurl)
  test_details <- format_results(by_test, baseurl, tests = TRUE)

  if (nrow(by_file) == 0) {
    test_files <- ""
    test_details <- ""
  }

  emoji <- list(emo_ok = "✅", emo_fail = "❌", emo_warn = "⚠️", emo_skip = "🦘")
  vars <- c(
    list(
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
      n = totals
    ),
    emoji
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

  # If longer than the current limit, then truncate it
  if (truncate && file.size(output) > gha_job_summary_size_limit) {
    cnt <- readBin(output, what = "raw", n = gha_job_summary_size_limit - 100)
    cnt <- c(cnt, charToRaw(gha_job_summary_trunc_note))
    writeBin(cnt, output)
  }

  invisible(output)
}

zeroemo <- function(x, emoji) {
  ifelse(x == 0, "", paste0(emoji, " ", x))
}

format_results <- function(results, baseurl, tests = FALSE) {
  with(
    results,
    paste0(
      "|[`",
      file,
      "`](",
      baseurl,
      "/tests/testthat/",
      file,
      ")|",
      if (tests) {
        paste0(gsub("|", "\\|", fixed = TRUE, test), "|")
      },
      zeroemo(broken, "❌"),
      "|",
      zeroemo(warning, "⚠️"),
      "|",
      zeroemo(skip, "🦘"),
      "|",
      success,
      "|",
      collapse = "\n"
    )
  )
}

format_funcs <- function(hit, count) {
  percent <- ifelse(count == 0, 100, hit / count * 100)
  paste0(hit, "/", count, " ", format_emoji(percent))
}

format_emoji <- function(x) {
  ifelse(x == 100, "⭐", ifelse(x >= 95, "✅", ifelse(x >= 75, "🛠️", "❌")))
}
