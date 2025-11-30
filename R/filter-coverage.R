#' Show test coverage for new code, relative to a git branch or commit
#'
#' @param mode The type of comparison to perform to determine the new
#'   code that is analyzed. Possible values:
#'   - `auto`: will choose `branch` if `path` is on the non-default (`main`
#'     or `master`) branch, and `diff` otherwise.
#'   - `diff`: compare the current code to `HEAD`.
#'   - `branch`: compare the current code to the default branch, `main` or
#'     `master`.
#'   - `commit`: analyze the last commit, i.e. compare `HEAD` to `HEAD~1`.
#'   - `pr`: analyze the current pull request. This only works if called
#'     on Github Actions.
#' @param coverage Test coverage results. If `NULL` then the last
#'   results are used via [last()].
#' @inheritParams reload
#'
#' @export

diff <- function(
  mode = c("auto", "diff", "branch", "commit", "pr"),
  path = ".",
  coverage = NULL
) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")
  mode <- match.arg(mode)

  if (mode == "auto") {
    mode <- if (should_filter_branch(coverage = coverage)) {
      "branch"
    } else {
      "diff"
    }
  }

  if (is.null(coverage[["filters"]][[mode]])) {
    coverage <- filter_coverage(mode, coverage = coverage)
  }

  keep <- !map_lgl(coverage[["filters"]][[mode]], is.null)
  diff <- coverage[["filters"]][[mode]][keep]
  c2 <- coverage[keep, ]

  for (i in seq_len(nrow(c2))) {
    keep <- logical(nrow(c2$lines[[i]]))
    for (iv in diff[[i]]) {
      keep[iv] <- TRUE
    }
    if (all(keep)) {
      next
    }
    c2$lines[[i]]$status[!keep] <- "noncode"
    c2$lines[[i]]$id[!keep] <- NA_integer_
    c2$lines[[i]]$coverage[!keep] <- NA_integer_
    c2$code_lines[i] <- sum(c2$lines[[i]]$status == "instrumented")
    c2$lines_covered[i] <- sum(c2$lines[[i]]$coverage > 0, na.rm = TRUE)
    c2$total_hits[i] <- sum(c2$lines[[i]]$coverage, na.rm = TRUE)
    c2$percent_covered[i] <- c2$lines_covered[i] / c2$code_lines[i] * 100
    c2$uncovered[[i]] <- calculate_uncovered_intervals(c2$lines[[i]])

    # functions
    fkeep <- keep[c2$funs[[i]]$line1]
    c2$funs[[i]] <- c2$funs[[i]][fkeep, ]
    c2$function_count[i] <- nrow(c2$funs[[i]])
    c2$functions_hit[i] <- sum(c2$funs[[i]]$coverage > 0, na.rm = TRUE)
  }

  c2 <- add_coverage_summary(c2)
  class(c2) <- c("coverage_table2", "tbl", class(c2))
  attr(c2, "coverage-name") <- paste(mode, "coverage")
  c2
}

show_diff <- function(coverage) {
  if (should_filter_pr(coverage = coverage)) {
    # on GHA only
    writeLines("")
    print(diff("pr", coverage = coverage))
  }
  # TODO: this does not work because of the shallow clone on GHA,
  # there is no HEAD~1 commit to compare against.
  if (FALSE && should_filter_commit(coverage = coverage)) {
    # on GHA only
    writeLines("")
    print(diff("commit", coverage = coverage))
  } else if (should_filter_branch(coverage = coverage)) {
    writeLines("")
    print(diff("branch", coverage = coverage))
  } else if (should_filter_diff(coverage = coverage)) {
    writeLines("")
    print(diff("diff", coverage = coverage))
  }
}

filter_coverage <- function(
  filters = c("auto", "branch", "commit", "diff", "pr"),
  path = ".",
  coverage = NULL
) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")
  if (is.character(filters)) {
    if (missing(filters)) {
      filters <- "auto"
    } else {
      stopifnot(all(filters %in% c("auto", "branch", "commit", "diff", "pr")))
    }
    if ("auto" %in% filters) {
      filters <- setdiff(unique(c(filters, auto_filter(".", coverage))), "auto")
    }
  }
  coverage <- coverage %||% last(path = ".")

  flines <- I(empty_data_frame(nrow = nrow(coverage)))

  if (is.character(filters)) {
    if ("branch" %in% filters) {
      flines$branch <- get_filter_branch(".", coverage)
    }
    if ("commit" %in% filters) {
      flines$commit <- get_filter_commit(".", coverage)
    }
    if ("diff" %in% filters) {
      flines$diff <- get_filter_diff(".", coverage)
    }
    if ("pr" %in% filters) {
      flines$pr <- get_filter_pr(".", coverage)
    }
  } else {
    # TODO: support custom filtels by specifying line intervals
  }

  coverage$filters <- flines
  coverage
}

auto_filter <- function(path = ".", coverage = NULL) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")

  c(
    if (should_filter_branch(".", coverage)) "branch",
    if (should_filter_commit(".", coverage)) "commit",
    if (should_filter_diff(".", coverage)) "diff",
    if (should_filter_pr(".", coverage)) "pr"
  )
}

# if we are on a branch that is not 'master' and 'main'

should_filter_branch <- function(path = ".", coverage = NULL) {
  if (!has_git()) {
    return(FALSE)
  }
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")
  !git_current_branch() %in% c("master", "main") &&
    (git_branch_exists("main") || git_branch_exists("master")) &&
    !should_filter_pr()
}

# only on GHA?

should_filter_commit <- function(path = ".", coverage = NULL) {
  if (!has_git()) {
    return(FALSE)
  }
  ccprov_actions$detect()
}

# any covered files have changed or are untracked

should_filter_diff <- function(path = ".", coverage = NULL) {
  if (!has_git()) {
    return(FALSE)
  }
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")

  git_has_changed(coverage$path) || any(git_is_untracked(coverage$path))
}

# only on GHA, if we are in a pr

should_filter_pr <- function(path = ".", coverage = NULL) {
  if (!has_git()) {
    return(FALSE)
  }
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")

  ccprov_actions$detect() && !is.null(ccprov_actions$get_pr())
}

get_filter_git_diff <- function(ref, path) {
  ret <- processx::run("git", c("diff", "-U0", ref, "--", path))
  lns <- strsplit(ret$stdout, "\n", fixed = TRUE)[[1]]
  chd <- grep("^@@", lns, value = TRUE)
  lapply(strsplit(chd, " ", fixed = TRUE), function(x) {
    ns <- as.numeric(strsplit(x[3], ",", fixed = TRUE)[[1]])
    if (length(ns) == 1) {
      as.integer(ns)
    } else {
      ns[1]:(ns[1] + ns[2] - 1L)
    }
  })
}

get_filter_git <- function(ref, paths, untracked = TRUE) {
  out <- processx::run("git", c("diff", "--raw", ref, "--", paths))
  ret <- vector("list", length(paths))
  if (out$stdout == "") {
    return(ret)
  }
  mod <- paths %in% utils::read.table(text = out$stdout)[, 6]
  unt <- git_is_untracked(paths)
  for (i in seq_along(paths)) {
    if (untracked && unt[i]) {
      lns <- readLines(paths[i])
      if (length(lns) > 0) {
        ret[[i]] <- list(seq_along(lns))
      }
    } else if (mod[i]) {
      ret[[i]] <- get_filter_git_diff(ref, paths[i])
    }
  }
  ret
}

get_filter_branch <- function(path = ".", coverage = NULL) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")

  get_filter_git(git_default_branch(), coverage$path, untracked = TRUE)
}

get_filter_commit <- function(path = ".", coverage = NULL) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")

  get_filter_git("HEAD^..HEAD", coverage$path, untracked = FALSE)
}

get_filter_diff <- function(path = ".", coverage = NULL) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")
  get_filter_git("HEAD", coverage$path, untracked = TRUE)
}

get_filter_pr <- function(path = ".", coverage = NULL) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")

  basebranch <- git_default_branch()
  base <- basebranch
  tryCatch(
    {
      ret <- processx::run(
        "git",
        c("merge-base", basebranch, "HEAD")
      )
      base <- trimws(ret$stdout)
    },
    error = function(e) NULL
  )
  get_filter_git(base, coverage$path, untracked = FALSE)
}
