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

  # indent according to directory structure
  coveragex$path <- paste0(
    strrep("&nbsp;", str_count(sub("/$", "", coveragex$path), "/")),
    coveragex$path
  )

  # summaries bold
  coveragex$path <- ifelse(
    coveragex$path == "All files" | endsWith(coveragex$path, "/"),
    paste0("**", coveragex$path, "**"),
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

  vars = list(
    package = pkgname,
    total_percent = total_percent,
    total_lines_covered = sum(coverage$lines_covered),
    total_lines = sum(coverage$code_lines),
    total_funcs_percent = total_funcs_percent,
    total_funcs_hit = sum(coverage$functions_hit),
    total_funcs = sum(coverage$function_count),
    data = data,
    total_emoji = total_emoji,
    total_funcs_emoji = total_funcs_emoji
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
