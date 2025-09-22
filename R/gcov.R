gcov_cleanup <- function(path) {
  path <- file.path(path, "src")
  gcda <- dir(path, pattern = "[.]gcda$", full.names = TRUE, recursive = TRUE)
  gcov <- dir(path, pattern = "[.]gcov$", full.names = TRUE, recursive = TRUE)
  unlink(c(gcda, gcov))
}


load_c_coverage <- function(path, exclusion_file = NULL) {
  withr::local_dir(path)
  fnms <- dir("src", recursive = TRUE, pattern = "[.]gcno$")
  processx::run(
    "gcov",
    c("-m", "-l", "-b", "-r", "-s", ".", fnms),
    wd = "src"
  )

  ccov <- parse_gcov(".", exclusion_file)
  # header files might appear multiple times (maybe others as well)
  # we need to merge these. Functions are sometimes missing from them,
  # and they may mark code lines as unreachable.
  if (anyDuplicated(names(ccov))) {
    dup <- duplicated(names(ccov))
    for (d1 in which(dup)) {
      pri <- which(names(ccov) == names(ccov)[d1])[1]
      # if both are NA, then keep NA, not code, as far as we know it until now
      cpri <- ccov[[pri]]$lines$coverage
      cd1 <- ccov[[d1]]$lines$coverage
      ccov[[pri]]$lines$coverage <- ifelse(
        is.na(cpri),
        ifelse(is.na(cd1), cpri, cd1),
        ifelse(is.na(cd1), cpri, cpri + cd1)
      )
      # functions are sometimes missing
      fm <- setdiff(ccov[[d1]]$functions$name, ccov[[pri]]$functions$name)
      if (length(fm) > 0) {
        ccov[[pri]]$functions <- rbind(
          ccov[[pri]]$functions,
          ccov[[d1]]$functions[ccov[[d1]]$functions$name %in% fm, ]
        )
        ccov[[pri]]$functions <- order_df(ccov[[pri]]$functions, "line")
      }
      # sum up functions
      fmap <- match(ccov[[d1]]$functions$name, ccov[[pri]]$functions$name)
      wpri <- as.double(ccov[[pri]]$functions$coverage[fmap])
      wd1 <- as.double(ccov[[d1]]$functions$coverage)
      factpri <- wpri / (wpri + wd1)
      factd1 <- wd1 / (wpri + wd1)
      rpri <- ccov[[pri]]$functions$returned[fmap]
      rd1 <- wd1 * ccov[[d1]]$functions$returned
      bpri <- ccov[[pri]]$functions$blocks[fmap]
      bd1 <- wd1 * ccov[[d1]]$functions$blocks
      ccov[[pri]]$functions$returned[fmap] <- (factpri * rpri + factd1 * rd1)
      ccov[[pri]]$functions$blocks[fmap] <- (factpri * bpri + factd1 * bd1)
      ccov[[pri]]$functions$coverage[fmap] <- wpri + wd1
    }
    ccov <- ccov[!dup]
  }

  ccov_funs <- lapply(ccov, "[[", "functions")
  ccov <- lapply(ccov, "[[", "lines")
  keep <- map_int(ccov, nrow) > 0
  ccov <- ccov[keep]
  ccov_funs <- ccov_funs[keep]

  # line exclusions
  for (i in seq_along(ccov)) {
    ccov[[i]]$status <- ifelse(
      is.na(ccov[[i]]$coverage),
      "noncode",
      "instrumented"
    )
    drop <- parse_line_exclusions(ccov[[i]]$code, res$path[i])
    if (length(drop)) {
      ccov[[i]]$status[drop] <- "excluded"
      ccov[[i]]$coverage[drop] <- NA_integer_
    }
  }

  # exclude functions whose first line was excluded
  for (i in seq_along(ccov)) {
    flex <- which(ccov[[i]]$status[ccov_funs[[i]]$line] == "excluded")
    if (length(flex) > 0) {
      ccov_funs[[i]] <- ccov_funs[[i]][-flex, ]
    }
  }

  res <- data.frame(
    stringsAsFactors = FALSE,
    path = names(ccov),
    symbol = NA_character_,
    num_markers = NA_integer_,
    line_count = map_int(ccov, nrow),
    code_lines = map_int(ccov, function(x) sum(!is.na(x$coverage))),
    lines_covered = map_int(ccov, function(x) {
      sum(x$coverage > 0, na.rm = TRUE)
    }),
    total_hits = map_dbl(ccov, function(x) {
      sum(as.double(x$coverage), na.rm = TRUE)
    }),
    percent_covered = NA_real_,
    function_count = map_int(ccov_funs, nrow),
    functions_hit = map_int(ccov_funs, function(x) {
      sum(x$coverage > 0, na.rm = TRUE)
    }),
    lines = I(replicate(length(ccov), NULL, simplify = FALSE)),
    funs = I(replicate(length(ccov), NULL, simplify = FALSE)),
    uncovered = I(replicate(length(ccov), NULL, simplify = FALSE)),
    filters = I(empty_data_frame(nrow = length(ccov)))
  )
  res$percent_covered <- ifelse(
    res$code_lines == 0,
    100,
    res$lines_covered / res$code_lines * 100
  )
  for (i in seq_along(ccov)) {
    res$lines[[i]] <- data.frame(
      lines = ccov[[i]]$code,
      status = ccov[[i]]$status,
      id = ccov[[i]]$line,
      coverage = ccov[[i]]$coverage
    )
    res$uncovered[[i]] <-
      calculate_uncovered_intervals(res$lines[[i]])
    nacol <- rep(NA_integer_, nrow(ccov_funs[[i]]))
    res$funs[[i]] <- data.frame(
      name = ccov_funs[[i]]$name,
      aliases = I(replicate(nrow(ccov_funs[[i]]), NULL, simplify = FALSE)),
      line1 = ccov_funs[[i]]$line,
      col1 = nacol,
      line2 = nacol,
      col2 = nacol,
      brace_line = nacol,
      brace_col = nacol,
      status = rep("instrumented", nrow(ccov_funs[[i]])),
      id = nacol,
      coverage = ccov_funs[[i]]$coverage
    )
  }

  class(res) <- c("tbl", class(res))
  res
}

parse_gcov <- function(root = ".", exclusion_file = NULL) {
  exclusion_file <- exclusion_file %&&% normalizePath(exclusion_file)
  withr::local_dir(root)
  gcov <- dir(
    "src",
    recursive = TRUE,
    pattern = "[.]gcov$",
    full.names = TRUE
  )

  # drop files that have an absolute path, typically system headers
  gcov <- gcov[!grepl("###", gcov, fixed = TRUE)]
  codepaths <- file.path(
    "src",
    sub("^.* 0:Source:", "", map_chr(gcov, readLines, n = 1))
  )

  # drop gcov files for ignored files
  if (!is.null(exclusion_file)) {
    cpinc <- apply_covrignore(codepaths, exclusion_file)
    keep <- codepaths %in% cpinc
    gcov <- gcov[keep]
    codepaths <- codepaths[keep]
  }
  ps <- structure(lapply(gcov, parse_gcov_file), names = codepaths)
  ps
}

parse_gcov_file <- function(path) {
  gcov <- .Call(c_cov_parse_gcov, path)
  class(gcov$lines) <- c("tbl", "data.frame")
  attr(gcov$lines, "row.names") <- seq_len(length(gcov$lines[[1]]))
  class(gcov$functions) <- c("tbl", "data.frame")
  # drop functions that we could not parse
  gcov$functions <- gcov$functions[!is.na(gcov$functions$line), ]
  # drop duplicated functions
  gcov$functions <- gcov$functions[!duplicated(gcov$functions$name), ]
  attr(gcov$functions, "row.names") <- seq_len(length(gcov$functions[[1]]))
  gcov
}

add_subprocess_coverage <- function(counts, subprocdir) {
  fls <- list.files(subprocdir, full.names = TRUE)
  for (fl in fls) {
    e <- new.env(parent = emptyenv())
    tryCatch(
      {
        load(fl, envir = e)
        for (n in ls(e, all.names = TRUE)) {
          counts[[n]] <- counts[[n]] + e[[n]]
        }
      },
      error = function(e) message("Failed to process subprocess code coverage")
    )
  }
  counts
}

gcov_flush_package <- function(package) {
  # First try a function from the package itself, specifically made to
  # do this
  tryCatch(
    {
      asNamespace(package)$gcov_flush()
      return()
    },
    error = function(...) {}
  )

  # Otherwise try to find the symbols in the DLL
  dll <- getNamespaceInfo(package, "DLLs")[[1]]
  handle <- unclass(dll)$handle
  .Call(c_cov_gcov_flush_package, handle)
}
