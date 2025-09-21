reload_setup <- function(
  type = c("debug", "release", "coverage"),
  path = ".",
  makeflags = NULL
) {
  withr::local_dir(path)
  type <- match.arg(type)
  makeflags <- makeflags %||% get_makeflags(type)

  # setup build options
  setup <- list(
    version = 1L,
    rver = as.character(getRversion()[, 1:2]),
    platform = R.Version()$platform,
    type = type
  )
  setup[["hash"]] = cli::hash_obj_sha1(setup)
  setup[["compiler_flags"]] <- makeflags
  setup[["dir"]] <- get_dev_dir(setup)
  setup[["pkgname"]] <- unname(desc::desc_get("Package", "."))
  setup[["pkgversion"]] <- unname(desc::desc_get("Version", "."))

  withr::local_options(structure(list(setup), names = opt_setup))

  setup
}

#' Load a package tree for development
#'
#' A wrapper on [pkgload::load_all()] to load a package tree from a
#' dev build.
#'
#' @param type Build type. One of:
#'   - `"debug"`: Compiled with debug flags. (Passes `debug = TRUE` to
#'     [pkgbuild::compiler_flags()].)
#'   - `"release"`: Compiled without debug flags. (Passes `debug = FALSE` to
#'     [pkgbuild::compiler_flags()].)
#'   - `"coverage"`: Compiled with debug flags and code coverage support.
#' @param path Path to the package tree.
#' @param makeflags Named character vector to override the default make flags.
#' @param clean Whether to delete the build directory before the build.
#' @param local_install Whether to install the built package into a local
#'   library and add that library to the beginning of the library path.
#'
#' @return
#' A list with entries:
#' - `setup`: List of the build setup. Entries:
#'   - `version`: Version of format of the build directory.
#'   - `rver`: R version. (First two digits only.)
#'   - `platform`: Build platform triplet.
#'   - `type`: The build type.
#'   - `hash`: The hash of the setup that is used as the build directory
#'     name.
#'   - `compiler_flags`: Nemed character vector of extra `Makevars` flags
#'     to use.
#'   - `dir`: The build directory.
#'   - `pkgname`: The name of the package.
#' - `plan`: Data frame of the plan to create the build directory. Columns:
#'   - `path`: Relative path of the file.
#'   - `isdir`: Whether it is a directory.
#'   - `action`: How to create the path in the build directory: `"link"` or
#'     `"copy"`.
#'   - `target`: Relative path of the file or directory in the build
#'     directory.
#'   - `hash`: Hash of the file for files, it is `NA` for directories.
#' - `load`: The return value of [pkgload::load_all()].
#' - `coverage`: For `type = "coverage"` builds a data frame with the
#'   code coverage instrumentation data for R files. See
#'   [test()] for the structure.
#' @export

reload <- function(
  type = c("debug", "release", "coverage"),
  path = ".",
  makeflags = NULL,
  clean = FALSE,
  local_install = TRUE
) {
  if (is_ci()) {
    writeLines(banner_load)
  }
  withr::local_dir(path)
  type <- match.arg(type)
  setup <- reload_setup(type, ".", makeflags)
  withr::local_options(structure(list(setup), names = opt_setup))

  if (clean) {
    unlink(setup[["dir"]], recursive = TRUE)
  }

  cli::cli_alert_info("Updating dev tree ({type})")
  copy <- c("src", if (type == "coverage" || local_install) "R")
  plan <- update_package_tree(
    ".",
    setup$dir,
    pkgname = setup$pkgname,
    copy = copy
  )

  pkg_dir <- file.path(setup$dir, setup$pkgname)

  cov_data <- parsed <- NULL
  if (type == "coverage") {
    # It is probably ignored by R CMD build, so need to use the master file
    exc <- if (file.exists(".covrignore")) {
      normalizePath((".covrignore"))
    }
    withr::with_dir(pkg_dir, {
      cov_data <- cov_instrument_dir("R", setup$pkgname, exclusion_file = exc)
      parsed <- cov_data$parsed
      cov_data <- cov_data$files
      setup_cov_env(cov_data)
    })
  }

  withr::local_options(pkg.build_extra_flags = FALSE)
  withr::local_makevars(setup$compiler_flags, .assignment = "+=")

  # need to patch first code file to create counters (if coverage)
  code_files <- find_code(pkg_dir)
  if (type == "coverage") {
    fn1 <- code_files[1]
    setup$covxxso <- inject_covxxso(setup$dir)
    cclines <- create_counters_lines(setup, cov_data)
    attr(cov_data, "metadata")$ccshift <- length(cclines)
    attr(cov_data, "metadata")$ccfile <- fn1
    writeLines(c(cclines, readLines(fn1)), fn1)
  }

  if (local_install) {
    lib <- file.path(setup[["dir"]], "__dev_lib__")
    inject_script <- if (type == "coverage") {
      setup_cov_inject_script(file.path(lib, setup$pkgname), cov_data)
    }
    fnx <- code_files[length(code_files)]
    # Need to run the quick_install_loaded() script from a fake .onLoad,
    # before the actualy .onLoad, in case .onLoad manipulates the namespace,
    # e.g. like in the pillar package:
    # https://github.com/r-lib/pillar/blob/d7e85eddd826da733c5aec12ccfb4d274c2eb5a6/R/zzz.R#L53
    writeLines(
      c(
        readLines(fnx),
        inject_onload_lines(setup, pkg_dir, lib, inject_script, fnx)
      ),
      fnx
    )
    withr::local_options("uncovr_parsed_data" = parsed)
    setup$dev_lib <- lib
  }

  loaded <- pkgload::load_all(pkg_dir)

  if (local_install) {
    update_libpath(lib, setup[["pkgname"]])
  }

  if (type == "coverage") {
    cov_data <- find_function_names(cov_data, loaded$env, setup)
  }

  invisible(list(
    setup = setup,
    plan = plan,
    load = loaded,
    coverage = if (type == "coverage") cov_data
  ))
}

find_function_names <- function(cov_data, env, setup) {
  pkgdir <- paste0(file.path(setup$dir, setup$pkgname), "/")
  meta <- attr(cov_data, "metadata")
  ccfile <- rel_path(meta$ccfile, pkgdir)
  for (on in names(env)) {
    if (is.function(env[[on]]) && !is.primitive(env[[on]])) {
      fun <- env[[on]]
      if (is.null(utils::getSrcref(fun))) {
        next
      }
      if (!identical(environmentName(environment(fun)), setup$pkgname)) {
        next
      }
      odr <- rel_path(utils::getSrcDirectory(fun), pkgdir)
      ofn <- utils::getSrcFilename(fun)
      oph <- file.path(odr, ofn)
      fidx <- match(oph, cov_data$path)
      if (is.na(fidx)) {
        next
      }
      ccshift <- if (oph == ccfile) meta$ccshift else 0L
      ol1 <- utils::getSrcLocation(fun, "line", first = TRUE) - ccshift
      ol2 <- utils::getSrcLocation(fun, "line", first = FALSE) - ccshift
      mch <- which(
        cov_data$funs[[fidx]]$line1 == ol1 & cov_data$funs[[fidx]]$line2 == ol2
      )
      if (length(mch) != 1) {
        next
      }
      if (is.na(cov_data$funs[[fidx]]$name[mch])) {
        cov_data$funs[[fidx]]$name[mch] <- on
      } else {
        cov_data$funs[[fidx]]$aliases[[mch]] <- c(
          cov_data$funs[[fidx]]$aliases[[mch]],
          on
        )
      }
    }
  }
  cov_data
}

inject_onload_lines <- function(setup, pkg_dir, lib, inject_script, fnx) {
  mkdirp(lib)
  subs <- list(
    type_ = setup$type,
    pkgname_ = setup$pkgname,
    pkg_dir_ = normalizePath(pkg_dir),
    lib_ = normalizePath(lib),
    inject_script_ = if (!is.null(inject_script)) normalizePath(inject_script),
    fnx_ = normalizePath(fnx)
  )
  deparse(
    substitute(
      {
        "__COV__ DELETE FROM HERE"
        `.__cov_has_onload` <- base::exists(
          ".onLoad",
          inherits = FALSE,
          mode = "function"
        )
        if (`.__cov_has_onload`) {
          .__cov__onload <- .onLoad
        }
        .onLoad <- function(libname, pkgname) {
          ns <- base::asNamespace(pkgname_)
          # restore old .onLoad (if any) and clean up namespace
          # need to do this before the quick install
          has_onload <- ns$`.__cov_has_onload`
          if (has_onload) {
            base::assign(".onLoad", ns$`.__cov__onload`, envir = ns)
            base::rm(list = ".__cov__onload", envir = ns)
          } else {
            base::rm(list = ".onLoad", envir = ns)
          }
          base::rm(list = ".__cov_has_onload", envir = ns)

          if (type_ == "coverage") {
            base::asNamespace("uncovr")$fix_src_refs(ns)
          }

          # install the package
          loaded <- base::list(
            dll = ns$.__NAMESPACE__.$DLLs,
            env = ns
          )
          base::asNamespace("uncovr")$quick_install_loaded(
            pkgname_,
            pkg_dir_,
            lib_,
            loaded,
            inject_script_
          )
          # clean up source file, in case this file is loaded with `load_all()`
          lns <- base::readLines(fnx_)
          d1 <- base::grep("__COV__ DELETE FROM HERE", lns, fixed = TRUE)[1]
          d2 <- base::grep("__COV__ DELETE UNTIL HERE", lns, fixed = TRUE)[2]
          lns <- lns[-(d1:d2)]
          base::writeLines(lns, fnx_)

          # call original .onLoad
          # TODO: is this ok, or needs Tailcall()?
          if (has_onload) {
            ns$.onLoad(libname, pkgname)
          }
        }
        "__COV__ DELETE UNTIL HERE"
      },
      subs
    )
  )
}

fix_src_refs <- function(ns) {
  # this functions intentionally does not care about reference counting!
  set_attr <- function(x, name, value) {
    .Call(c_cov_set_attr, x, name, value)
  }

  parsed <- getOption("uncovr_parsed_data")
  if (is.null(parsed)) {
    return()
  }
  for (pf in parsed) {
    for (i in seq_along(pf)) {
      sr <- utils::getSrcref(pf)[[i]]
      if (is.null(sr)) {
        next
      }
      if (
        length(pf[[i]]) == 3 &&
          (identical(pf[[i]][[1]], quote(`<-`)) ||
            identical(pf[[i]][[1]], quote(`=`))) &&
          typeof(pf[[i]][[2]]) == "symbol"
      ) {
        nm <- as.character(pf[[i]][[2]])
        if (nm %in% names(ns)) {
          fn <- ns[[nm]]
          if (is.null(utils::getSrcref(fn))) {
            next
          }
          set_attr(ns[[nm]], "srcref", sr)
        }
      }
    }
  }
}

update_libpath <- function(lib, pkgname) {
  # remove exixting dev library for the same package, if any
  current <- .libPaths()
  current <- current[!grepl(paste0("/__dev_lib__$"), current)]
  .libPaths(c(lib, current))
}

clean_libpath <- function(pkgname) {
  update_libpath(NULL, pkgname)
}


prepare_test_results <- function(dd) {
  tr <- dd$test_results
  for (i in seq_along(tr)) {
    for (j in seq_along(tr[[i]]$results)) {
      if (!is.null(tr[[i]]$results[[j]]$srcref)) {
        tr[[i]]$results[[j]]$srcref <- trim_srcref(tr[[i]]$results[[j]]$srcref)
      }
    }
  }
  class(tr) <- unique(c("cov_testthat_results", class(tr)))
  tr
}

prepare_coverage_results <- function(dd) {
  cr <- dd$coverage
  meta <- list(
    setup = dd$setup
  )
  attr(cr, "metadata") <- c(attr(cr, "metadata"), meta)
  cr
}

trim_srcref <- function(s) {
  list(
    chr = as.character(s),
    directory = utils::getSrcDirectory(s),
    filename = utils::getSrcFilename(s),
    start_row = utils::getSrcLocation(s, "line"),
    end_row = utils::getSrcLocation(s, "line", first = FALSE),
    start_column = utils::getSrcLocation(s, "column"),
    end_column = utils::getSrcLocation(s, "column", first = FALSE),
    start_byte = utils::getSrcLocation(s, "byte"),
    end_byte = utils::getSrcLocation(s, "byte", first = FALSE)
  )
}

add_coverage_summary <- function(coverage) {
  # by directory
  dirs <- all_leading_dirs(coverage$path)
  sumdir <- function(x, dir) {
    sum(x[startsWith(coverage$path, dir)], na.rm = TRUE)
  }
  sm <- data.frame(
    name = c("All files", dirs[-1]),
    line_count = map_int(dirs, sumdir, x = coverage$line_count),
    code_lines = map_int(dirs, sumdir, x = coverage$code_lines),
    lines_covered = map_int(dirs, sumdir, x = coverage$lines_covered),
    total_hits = map_dbl(dirs, sumdir, x = as.double(coverage$total_hits)),
    function_count = map_int(dirs, sumdir, x = coverage$function_count),
    functions_hit = map_int(dirs, sumdir, x = coverage$functions_hit)
  )
  class(sm) <- c("tbl", class(sm))

  sm$percent_covered <- sm$lines_covered / sm$code_lines * 100
  attr(coverage, "summary") <- sm

  coverage
}

calculate_uncovered_intervals <- function(lines) {
  keep <- lines$status == "instrumented"
  lineno <- seq_len(nrow(lines))[keep]
  find_zero_ranges(lineno, lines$coverage[keep])
}

cov_get_counts <- function(names) {
  env <- if (length(names) > 1 && exists(names[1], envir = baseenv())) {
    baseenv()
  } else {
    globalenv()
  }
  structure(
    lapply(names, function(name) {
      .Call(c_cov_get_counts, get(name, envir = env))
    }),
    names = names
  )
}

get_dev_dir <- function(setup = list(hash = "")) {
  file.path(".dev", setup$hash)
}

# specific for us, it should be in .Rbuildignore but have it here as well
ignored_extra <- c("^[.]dev$")

exclude_build_ignored <- function(
  plan,
  src = ".",
  pkgname = desc::desc_get("Package", src)
) {
  if (nrow(plan) == 0) {
    return(plan)
  }
  ign_file <- file.path(src, ".Rbuildignore")
  ign <- if (file.exists(ign_file)) {
    readLines(ign_file, warn = FALSE)
  }
  ign <- c(ignored_extra, ign)
  ign <- ign[nzchar(ign)]
  # case insensitive!
  ign <- paste0("(?i)", ign)

  ptrn <- c(ign, re_exclude(pkgname))
  ptrn_dir <- re_exclude_dir(pkgname)

  plan$exclude <- FALSE
  plan <- exclude_paths(plan, ptrn)
  plan <- exclude_dirs(plan, ptrn_dir)
  plan <- exclude_downstream(plan)

  plan[!plan$exclude, ]
}

exclude_paths <- function(plan, ptrn) {
  for (p in ptrn) {
    plan$exclude <- plan$exclude | grepl(p, plan$path, perl = TRUE)
  }
  plan
}

exclude_dirs <- function(plan, ptrn) {
  for (p in ptrn) {
    plan$exclude <- plan$exclude |
      (plan$isdir & grepl(p, plan$path, perl = TRUE))
  }
  plan
}

exclude_downstream <- function(plan) {
  # We don't actually need this now, but we could use it to optimize,
  # because we could trim only the subsequent elements after a directory.
  plan <- plan[order(plan$path), ]

  # We need to take each excluded directory, and remove all paths in them
  exdirs <- paste0(plan$path[plan$isdir & plan$exclude], "/")
  del <- logical(nrow(plan))
  for (ed in exdirs) {
    del <- del | startsWith(plan$path, ed)
  }
  plan <- plan[!del, ]
  plan
}

apply_covrignore <- function(paths, exclusion_file) {
  if (!is.null(exclusion_file)) {
    if (file.exists(exclusion_file)) {
      excluded <- Sys.glob(readLines(exclusion_file))
      paths <- setdiff(paths, excluded)
      dirs <- excluded[file.exists(excluded) & is_dir(excluded)]
      dirs <- ifelse(endsWith(dirs, "/"), dirs, paste0(dirs, "/"))
      for (d in dirs) {
        paths <- paths[!startsWith(paths, d)]
      }
    }
  }
  paths
}

cov_instrument_dir <- function(
  path = "R",
  pkgname = desc::desc_get("Package", dirname(path)),
  exclusion_file = ".covrignore"
) {
  cli::cli_alert_info("Instrumenting {pkgname}")
  rfiles <- apply_covrignore(
    dir(path, pattern = "[.][rR]$", full.names = TRUE),
    exclusion_file
  )
  symbol <- paste0(
    ".__cov_",
    pkgname,
    "_",
    tools::file_path_sans_ext(basename(rfiles))
  )
  fls <- data.frame(
    path = rfiles,
    symbol = symbol,
    num_markers = 0L,
    line_count = NA_integer_,
    code_lines = NA_integer_,
    lines_covered = 0L,
    total_hits = 0,
    percent_covered = 0,
    function_count = 0L,
    functions_hit = 0L,
    lines = I(replicate(length(rfiles), NULL, simplify = FALSE)),
    funs = I(replicate(length(rfiles), NULL, simplify = FALSE)),
    uncovered = I(replicate(length(rfiles), list(), simplify = FALSE))
  )
  class(fls) <- c("tbl", class(fls))

  parsed <- structure(vector("list", nrow(fls)), names = fls$path)
  for (i in seq_along(rfiles)) {
    cifile <- cov_instrument_file(fls$path[i], fls$symbol[i])
    parsed[[i]] <- cifile$parsed
    fls$lines[[i]] <- cifile$lines
    fls$line_count[i] <- nrow(fls$lines[[i]])
    fls$code_lines[i] <- sum(fls$lines[[i]]$status == "instrumented")
    fls$funs[[i]] <- cifile$funs
    fls$function_count[i] <- nrow(cifile$funs)
    fls$num_markers[i] <- nrow(cifile$lines) + nrow(cifile$funs)
  }

  list(files = fls, parsed = parsed)
}

cov_instrument_file <- function(path, cov_symbol) {
  hash <- cli::hash_file_xxhash(path)
  cached <- get_cached_file(path, hash)
  if (!is.null(cached)) {
    base::writeLines(cached$code, path)
    return(cached)
  }
  ps <- parse(path, keep.source = TRUE)
  psd <- utils::getParseData(ps)
  brc_poss <- which(psd$token == "'{'")

  # drop rlang's {{ embrace operator up front
  drop <- which(diff(psd$line1[brc_poss]) == 0 & diff(psd$col1[brc_poss]) == 1)
  drop <- c(drop, drop + 1L)
  if (length(drop)) {
    brc_poss <- brc_poss[-drop]
  }

  # calculcate insertion position for each subexpression of every '{'
  # plus prepend the top level exprssions
  toplevel <- psd[
    which(psd$parent == 0 & psd$token != "COMMENT"),
    c("line1", "col1", "line2", "col2")
  ]
  inj_posl <- c(list(toplevel), lapply(brc_poss, get_inject_positions, psd))
  injx <- data.frame(
    line1 = unlist(lapply(inj_posl, "[[", "line1")),
    col1 = unlist(lapply(inj_posl, "[[", "col1")),
    line2 = unlist(lapply(inj_posl, "[[", "line2")),
    col2 = unlist(lapply(inj_posl, "[[", "col2"))
  )
  # work around files without '{'
  if (nrow(injx)) {
    injx$code <- paste0("`", cov_symbol, "`[", injx$line1, "]; ")
  } else {
    injx$code <- character()
  }

  # Drop duplicate lines, only the first expression is counted.
  # TODO: improve this and count every expression individually
  inj <- injx[!duplicated(injx$line1), , drop = FALSE]
  # we note this before adding the function coverage markers, because
  # those typically go into non-instrumented lines
  instrumented_lines <- inj$line1

  lns0 <- lns <- untabify(readLines(path))

  # now do the functions as well
  # TODO: \()? Or maybe not?
  fun_poss <- which(psd$token == "FUNCTION")
  par_poss <- match(psd$parent[fun_poss], psd$id)
  # need an opening brace, whose grandparent's position is par_poss
  par_brc_poss <- match(psd$parent[brc_poss], psd$id)
  ppar_brc_poss <- match(psd$parent[par_brc_poss], psd$id)
  obr_poss <- brc_poss[match(par_poss, ppar_brc_poss)]
  bad <- is.na(obr_poss)
  fun_poss <- fun_poss[!bad]
  par_poss <- par_poss[!bad]
  obr_poss <- obr_poss[!bad]
  funres <- data.frame(
    name = rep(NA_character_, length(fun_poss)),
    aliases = I(replicate(length(fun_poss), NULL, simplify = FALSE)),
    line1 = psd$line1[fun_poss],
    col1 = psd$col1[fun_poss],
    line2 = psd$line2[par_poss],
    col2 = psd$col2[par_poss],
    brace_line = psd$line1[obr_poss],
    brace_col = psd$col1[obr_poss],
    status = rep("instrumented", length(fun_poss)),
    id = seq_along(fun_poss) + length(lns0),
    coverage = rep(NA_integer_, length(fun_poss))
  )

  if (nrow(funres)) {
    injf <- data.frame(
      line1 = funres$brace_line,
      col1 = funres$brace_col + 1L,
      line2 = funres$brace_line,
      col2 = funres$brace_col + 1L,
      code = paste0("`", cov_symbol, "`[", funres$id, "]; ")
    )
    inj <- rbind(inj, injf)
  }

  # need to insert code concurrently to the same line, for future
  # multi-expression per line support
  injs <- split(inj, inj$line1, drop = TRUE)
  ils <- as.integer(names(injs))
  for (i in seq_along(injs)) {
    il <- ils[i]
    lns[il] <- str_insert_parallel(lns[il], injs[[i]]$col1, injs[[i]]$code)
  }
  base::writeLines(lns, path)

  res <- data.frame(
    lines = lns0,
    status = "noncode",
    id = NA_integer_,
    coverage = NA_integer_
  )

  # lines where there is no terminal node are definitely not code
  # apart from long strings, which are code
  notterm <- which(psd$text != "")
  res$status[unique(c(psd$line1[notterm], psd$line2[notterm]))] <- NA_character_
  strconst <- which(psd$token == "STR_CONST")
  for (str in strconst) {
    res$status[psd$line1[str]:psd$line2[str]] <- NA_character_
  }

  res$status[instrumented_lines] <- "instrumented"
  res$id[instrumented_lines] <- instrumented_lines

  # Handle multi-line expressions. We need to do this backwards, so nested
  # braces work out correctly.
  for (i in rev(seq_len(nrow(inj)))) {
    li <- inj$line1[i]:inj$line2[i]
    if (length(li) == 1) {
      next
    }
    cnt <- li[is.na(res$status[li])]
    res$status[cnt] <- "instrumented"
    res$id[cnt] <- inj$line1[i]
  }
  comment_lines <- setdiff(
    psd$line1[psd$token == "COMMENT"],
    psd$line1[psd$token != "COMMENT"]
  )
  res$status[comment_lines] <- "noncode"
  res$id[comment_lines] <- NA_integer_
  res$coverage[res$status == "instrumented"] <- 0L
  res$status[is.na(res$status)] <- "noncode"

  # we add line exclusions here, so it is theoretically possible
  # to only exclude parts of a multi-line expression
  drop <- parse_line_exclusions(lns0, path)
  if (length(drop)) {
    res$status[drop] <- "excluded"
    res$id[drop] <- NA_integer_
    res$coverage[drop] <- NA_integer_
  }

  # drop functions that are completely excluded
  firstlineex <- which(res$status[funres$line1] == "excluded")
  for (fex in firstlineex) {
    l1 <- funres$line1[fex]
    l2 <- funres$line2[fex]
    if (!any(res$status[l1:l2] == "instrumented")) {
      funres$status[fex] <- "excluded"
    }
  }
  if (any(funres$status == "excluded")) {
    funres <- funres[funres$status != "excluded", ]
    funres$id <- seq_len(nrow(funres)) + length(lns0)
  }

  res <- list(lines = res, funs = funres, parsed = ps, code = lns)
  set_cached_file(path, hash, res)
  res
}

# `getParseData()` gives te wrong coordinates for lines with TAB characters,
# it counts each TAB as 8 spaces, so we need to circumvent that.
# But prepare for this to be fixed at some point.

untabify <- local({
  opt <- options(
    keep.parse.data = TRUE,
    keep.parse.data.pkgs = TRUE,
    keep.source = TRUE,
    keep.source.pkgs = TRUE
  )
  on.exit(options(opt), add = TRUE)
  badtab <- getParseData(parse(text = "\tx", keep.source = TRUE))$col1[1] == 9
  function(x) {
    if (badtab) {
      x <- gsub("\t", strrep(" ", 8), fixed = TRUE, x)
    }
    x
  }
})

get_inject_positions <- function(brc_pos, psd) {
  parent_id <- psd$parent[brc_pos]
  expr_pos <- which(psd$parent == parent_id)
  # drop comments, no need to instrument them and cause issues
  expr_pos <- expr_pos[psd$token[expr_pos] != "COMMENT"]
  # first one is '{', last one is '}', drop them
  expr_pos <- expr_pos[c(-1, -length(expr_pos))]
  psd[expr_pos, c("line1", "col1", "line2", "col2")]
}

str_insert_parallel <- function(str, pos, insert) {
  text_starts <- c(1, pos)
  text_ends <- c(pos - 1, nchar(str))

  text_pieces <- substring(str, text_starts, text_ends)

  res <- character(length(text_pieces) + length(insert))
  res[myseq(1, length(res), by = 2)] <- text_pieces
  res[myseq(2, length(res) - 1, by = 2)] <- insert

  paste(res, collapse = "")
}

myseq <- function(from, to, by = 1) {
  stopifnot(by != 0)
  if (by > 0) {
    if (to < from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  } else {
    if (to > from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  }
}

last_test_results <- function(path = ".") {
  withr::local_dir(path)
  setup <- reload_setup(type = "coverage", path = ".")
  test_results_file <- file.path(setup$dir, last_tests_file_name)
  if (!file.exists(test_results_file)) {
    message("No test results yet. Run `test()!")
    return(invisible(NULL))
  }

  tr <- readRDS(test_results_file)
  attr(tr, "at") <- file.mtime(test_results_file)
  tr
}

#' Find and print the last test coverage results
#'
#' @inheritParams reload
#' @return A package_coverage object. If there are no previous results,
#'   then a message is shown and `NULL` is returned.
#'
#' @export

last <- function(path = ".") {
  withr::local_dir(path)
  setup <- reload_setup(type = "coverage", path = ".")
  coverage_results_file <- file.path(setup$dir, last_coverage_file_name)
  if (!file.exists(coverage_results_file)) {
    cli::cli_alert_info("No test coverage yet. Run `test()!")
    return(invisible(NULL))
  }

  cr <- readRDS(coverage_results_file)
  attr(cr, "at") <- file.mtime(coverage_results_file)
  attr(cr, "test_results") <- suppressMessages(last_test_results("."))

  cr
}

#' Re-run the test files that had failing tests in the last test run
#'
#' @inheritParams reload
#' @param types Test result types to re-run, a character vector, possible
#'   elements are `"fail"` (default), `"warning"`, `"skip"`, "`all`".
#'   `"all"` is equivalent to `c("fail", "warning", "skip")`.
#' @param show_coverage Whether to show code coverage results.
#' @param ... Additional arguments are passed to [test()].
#'
#' @export

retest <- function(
  path = ".",
  types = c("fail", "warning", "skip", "all")[1],
  show_coverage = FALSE,
  ...
) {
  tr <- last_test_results(path)
  if (is.null(tr)) {
    return(invisible(NULL))
  }

  by_file <- testthat_results_by_file(tr)
  sel <- rep(FALSE, nrow(by_file))
  if (any(c("fail", "all") %in% types)) {
    sel <- sel | by_file$broken > 0
  }
  if (any(c("warning", "all") %in% types)) {
    sel <- sel | by_file$warning > 0
  }
  if (any(c("skip", "all") %in% types)) {
    sel <- sel | by_file$skip > 0
  }
  rerun <- by_file$context[sel]
  if (length(rerun) == 0) {
    cli::cli_alert_info("Nothing to re-run.")
    return(invisible(NULL))
  }

  # TODO: do a better job than this regex
  rerun <- gsub(".", "[.]", fixed = TRUE, rerun)
  filter <- paste0("^(", paste(rerun, collapse = "|"), ")$")
  test(filter = filter, path = path, show_coverage = show_coverage, ...)
}
