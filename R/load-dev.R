opt_setup <- "testthatlabs.setup"

get_makeflags <- function(type = c("debug", "release", "coverage")) {
  type <- match.arg(type)
  if (type == "debug") {
    pkgbuild::compiler_flags(debug = TRUE)
  } else if (type == "release") {
    pkgbuild::compiler_flags(debug = FALSE)
  } else if (type == "coverage") {
    paste_named(pkgbuild::compiler_flags(debug = TRUE), covr_flags())
  }
}

paste_named <- function(orig, new) {
  for (nm in names(new)) {
    orig[[nm]] <- if (nm %in% names(orig)) {
      paste(orig[[nm]], new[[nm]])
    } else {
      new[[nm]]
    }
  }
  orig
}

load_package_setup <- function(
  type = c("debug", "release", "coverage"),
  path = ".",
  makeflags = NULL
) {
  withr::local_dir(path)
  type <- match.arg(type)
  makeflags <- makeflags %||% get_makeflags(type)

  # setup build options
  makeflags <- makeflags
  setup <- list(
    version = 1L,
    rver = as.character(getRversion()[, 1:2]),
    platform = R.Version()$platform,
    type = type
  )
  setup[["hash"]] = cli::hash_obj_sha1(setup)
  setup[["compiler_flags"]] <- get_makeflags(type)
  setup[["dir"]] <- get_dev_dir(setup)
  setup[["pkgname"]] <- desc::desc_get("Package", ".")

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
#'   [package_coverage()] for the structure.
#' @export

load_package <- function(
  type = c("debug", "release", "coverage"),
  path = ".",
  makeflags = NULL,
  clean = FALSE,
  local_install = FALSE
) {
  withr::local_dir(path)
  type <- match.arg(type)
  setup <- load_package_setup(type, path, makeflags)
  withr::local_options(structure(list(setup), names = opt_setup))

  if (clean) {
    unlink(setup[["dir"]], recursive = TRUE)
  }

  copy <- c("src", if (type == "coverage") "R")
  plan <- update_package_tree(
    ".",
    setup$dir,
    pkgname = setup$pkgname,
    copy = copy
  )

  cov_data <- NULL
  if (type == "coverage") {
    # It is probably ignored by R CMD build, so need to use the master file
    exc <- if (file.exists(".covrignore")) {
      normalizePath((".covrignore"))
    }
    withr::with_dir(file.path(setup$dir, setup$pkgname), {
      cov_data <- cov_instrument_dir("R", exclusion_file = exc)
      setup_cov_env(cov_data)
    })
  }

  withr::local_options(pkg.build_extra_flags = FALSE)
  withr::local_makevars(setup$compiler_flags, .assignment = "+=")
  pkg_dir <- file.path(setup$dir, setup$pkgname)
  loaded <- pkgload::load_all(pkg_dir)

  if (local_install) {
    lib <- file.path(setup[["dir"]], "__dev_lib__")
    inject_script <- if (type == "coverage") {
      setup_cov_inject_script(file.path(lib, setup$pkgname), cov_data)
    }
    quick_install_loaded(
      setup[["pkgname"]],
      pkg_dir,
      lib,
      loaded,
      inject_script = inject_script
    )
    update_libpath(lib, setup[["pkgname"]])
    setup$dev_lib <- lib
  }

  invisible(list(
    setup = setup,
    plan = plan,
    load = loaded,
    coverage = if (type == "coverage") cov_data
  ))
}

#' @details
#' `l()` is an alias of `load_package()`.
#' @rdname load_package
#' @export

l <- load_package

setup_cov_inject_script <- function(target, cov_data) {
  script <- file.path(target, "R", "inject.R")
  ttlsoname <- paste0("testthatlabs", .Platform$dynlib.ext)
  ttlpkg <- find.package("testthatlabs")
  ttlso <- file.path(ttlpkg, "libs", ttlsoname)
  if (!file.exists(ttlso)) {
    ttlso <- file.path(ttlpkg, "src", ttlsoname)
  }
  stopifnot(file.exists(ttlso))
  mkdirp(file.path(target, "libs"))
  tgtsoname <- paste0("covxx", .Platform$dynlib.ext)
  file.copy(ttlso, file.path(target, "libs", tgtsoname))

  lns <- c(
    "# coverage counter injection by testthatlabs",
    "local({",
    "  info <- loadingNamespaceInfo()",
    "  pkg <- info$pkgname",
    "  tgtsoname <- paste0('covxx', .Platform$dynlib.ext)",
    "  dl <- dyn.load(file.path(info$libname, pkg, 'libs', tgtsoname))",
    "  mc <- getNativeSymbolInfo('cov_make_counter', dl)",
    sprintf(
      "  assign('%s', .Call(mc, %dL), envir = .GlobalEnv)",
      cov_data$symbol,
      cov_data$line_count
    ),
    "  output <- file.path(",
    "    dirname(info$libname), ",
    "    'cov',",
    "    paste0(Sys.getpid(), '.rda')",
    "  )",
    "  dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)",
    "  reg.finalizer(",
    "    .GlobalEnv,",
    "    function(e) {",
    "      save(",
    "        list = ls(all.names = TRUE, pattern = '^[.]__cov_', envir = e),",
    "        file = output,",
    "        compression_level = 0,",
    "        envir = e",
    "     )",
    "    },",
    "    onexit = TRUE",
    "  )",
    "})"
  )

  mkdirp(dirname(script))
  writeLines(lns, script)
  script
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

quick_install_loaded <- function(
  pkgname,
  dir,
  lib,
  loaded,
  inject_script = NULL
) {
  # relative path, so do it first
  tgt <- file.path(lib, pkgname)
  mkdirp(tgt)
  tgt <- normalizePath(tgt)

  # detto
  if (!is.null(inject_script)) {
    inject_script <- normalizePath(inject_script)
  }

  withr::local_dir(dir)

  asNamespace("tools")$.install_package_description(".", tgt)
  asNamespace("tools")$.install_package_namespace_info(".", tgt)
  asNamespace("tools")$.install_package_indices(".", tgt)

  # copy libs
  arch <- .Platform[["r_arch"]]
  ilr <- "install.libs.R"
  if (file.exists(file.path("src", ilr))) {
    local.env <- local({
      SHLIB_EXT <- .Platform$dynlib.ext
      R_PACKAGE_DIR <- tgt
      R_PACKAGE_NAME <- pkgname
      R_PACKAGE_SOURCE <- "."
      R_ARCH <- arch
      WINDOWS <- is_windows()
      environment()
    })
    parent.env(local.env) <- .GlobalEnv
    withr::with_dir("src", {
      source(ilr, local = local.env)
    })
  } else {
    shlib_ext <- .Platform[["dynlib.ext"]]
    dlls <- Sys.glob(paste0("src/*", shlib_ext))
    if (length(dlls)) {
      libs <- file.path(tgt, "libs")
      if (arch != "") {
        libs <- paste0(libs, arch)
      }
      mkdirp(libs)
      file.copy(dlls, libs, overwrite = TRUE)
    }
  }

  # R code
  # First remove native routines from the list of symbols
  native <- if (length(loaded$dll)) {
    native_info <- readRDS(file.path(tgt, "Meta", "nsInfo.rds"))$nativeRoutines
    unlist(lapply(names(loaded$dll), function(dllname) {
      fixes <- native_info[[dllname]][["registrationFixes"]]
      paste0(
        fixes[1L],
        unlist(lapply(getDLLRegisteredRoutines(loaded$dll[[dllname]]), names)),
        fixes[2L]
      )
    }))
  }
  # also remove .__DEVTOOLS__, this is a not a load_all()-d package
  variables <- setdiff(
    ls(loaded$env, all.names = TRUE),
    c(native, ".__DEVTOOLS__")
  )

  filebase <- file.path(tgt, "R", pkgname)
  mkdirp(dirname(filebase))
  file.create(filebase)
  if (!is.null(inject_script)) {
    file.append(filebase, inject_script)
  }
  file.append(filebase, file.path(R.home("share"), "R", "nspackloader.R"))

  asNamespace("tools")$makeLazyLoadDB(
    loaded$env,
    filebase = filebase,
    variables = variables,
    compress = FALSE
  )

  # some potential extra files
  extra <- c("NAMESPACE", "LICENCE", "LICENSE", "NEWS", "NEWS.md")
  for (fn in extra) {
    if (file.exists(fn)) {
      file.copy(fn, tgt, overwrite = TRUE)
    }
  }

  # inst/
  if (file.exists("inst")) {
    copy_inst_files(".", tgt)
  }
}

# TODO: this is much simpler than base e.g. we don't use .Rinstignore
copy_inst_files <- function(src, tgt) {
  inst <- file.path(src, "inst")
  if (!file.exists(inst)) {
    return()
  }

  fls <- dir(inst, full.names = TRUE, include.dirs = TRUE, no.. = TRUE)
  for (fl in fls) {
    file.copy(fl, tgt, recursive = TRUE)
  }
}

#' Run package tests and show test coverage results
#'
#' @details
#' Performs the following steps:
#' - Builds the package in a build directory, or updates the most recent
#'   build, if there is one.
#' - Loads the package from the build directory using [pkgload::load_all()].
#' - Installs the package into a local library (by default).
#' - Runs the package tests.
#' - Calculates and shows the test coverage of the package's source files.
#'
#' @param filter Regular expression to filter both the test files (via the
#'   `filter` argument of [testthat::test_dir()]), and the source files
#'   in the coverage results.
#' @param test_dir Test directory to use. Defaults to `tests/testthat`
#'   within the package tree.
#' @param reporter The testthat reporter to use. Passed to
#'   [testthat::test_dir()].
#' @inheritParams load_package
#'
#' @return A list of class `package_coverage` with entries:
#'   - `setup`: Build setup, see [load_package()].
#'   - `plan`: Build plan, see [load_package()].
#'   - `load`: Return value of [pkgload::load_all()].
#'   - `coverage`: Code coverage results. Columns:
#'     - `path`: Relative path to the R code file.
#'     - `symbol`: The R variable that is used to collect the coverage for
#'       this file.
#'     - `line_count`: Total number of lines in the file.
#'     - `code_lines`: Number of code lines (that are not excluded).
#'     - `lines_covered`: The number of covered lines will be stored here
#'       after a test coverage run. For `load_package()` it is all zero.
#'     - `percent_covered`: The test coverage percentage of the file will
#'       be stored here after a test coverage run. For `load_package()` it is
#'       all zero.
#'     - `lines`: A list column with a data frame for each file. The data
#'       frame has columns:
#'       - `lines`: The code line.
#'       - `status`: Whether this line is `"instrumented"`, `"noncode"` or
#'         `"excluded"`.
#'       - `id` The id of the counter that applies to this line. Often the
#'         same as the line number, but not always, e.g. for multi-line
#'         expressions. `NA` for lines that are not `"instrumented"`.
#'       - `coverage`: The number of times the line was covered will be filled
#'         in here after a test coverage run. For `load_package()` it is zero,
#'         but `NA` for lines that are not `"instrumented"`.
#'   - `test_results`: Return value of [testthat::test_dir()].
#'
#' @export

package_coverage <- function(
  filter = NULL,
  path = ".",
  test_dir = "tests/testthat",
  reporter = NULL,
  clean = FALSE,
  local_install = TRUE
) {
  withr::local_dir(path)

  # clean up .gcda files, because pkgbuild wrongly considers them as source
  # files and thinks that the dll is out of data, because they are newer
  setup <- load_package_setup(type = "coverage", path = ".")
  pkg_path <- file.path(setup$dir, setup$pkgname)
  if (!clean) {
    gcov_cleanup(pkg_path)
  }

  dev_data <- load_package(
    type = "coverage",
    path = ".",
    clean = clean,
    local_install = local_install
  )
  on.exit(clean_libpath(dev_data$setup$pkgname), add = TRUE)

  # clean up files from subprocesses
  subprocdir <- file.path(setup$dir, "cov")
  unlink(subprocdir, recursive = TRUE)

  if (!local_install) {
    # TODO: only message if parallel tests are on
    message("`local_install = FALSE` turns off parallel testthat!")
    withr::local_envvar(TESTTHAT_PARALLEL = "false")
  }
  # TODO: ideally we would use load_package = "none" in the main process and
  # load_package = "installed" in the testthat workers
  dev_data$test_results <- testthat::test_dir(
    test_dir,
    package = setup[["pkgname"]],
    load_package = if (local_install) "installed" else "none",
    stop_on_failure = FALSE,
    filter = filter,
    reporter = reporter
  )

  cov_names <- dev_data$coverage$symbol
  counts <- cov_get_counts(cov_names)
  counts <- add_subprocess_coverage(counts, subprocdir)
  for (i in seq_along(counts)) {
    ids <- dev_data$coverage$lines[[i]]$id
    dev_data$coverage$lines[[i]]$coverage[] <- counts[[i]][ids]
    dev_data$coverage$lines_covered[i] <-
      sum(dev_data$coverage$lines[[i]]$coverage > 0, na.rm = TRUE)
    dev_data$coverage$uncovered[[i]] <-
      calculate_uncovered_intervals(dev_data$coverage$lines[[i]])
  }

  if (file.exists("src")) {
    # try to flush the coverage data for the package
    # TODO: is there are a more robust way?
    tryCatch(
      gcov_flush_package(dev_data$setup$pkgname),
      error = function(...) {
        stop(cli::format_error(c(
          "Could not run `gcov_flush()` in the {.pkg {dev_data$setup$pkgname}}
          package. You need to add a `gcov_flush()` function to it, that
          calls `__gcov_dump()`, see an example in the {.pkg ps} package."
        )))
      }
    )
    exc <- if (file.exists(".covrignore")) {
      normalizePath((".covrignore"))
    }
    ccoverage <- load_c_coverage(pkg_path, exc)
    dev_data$coverage <- rbind(dev_data$coverage, ccoverage)
  }

  dev_data$coverage$percent_covered <-
    dev_data$coverage$lines_covered / dev_data$coverage$code_lines * 100
  dev_data$coverage$percent_covered[dev_data$coverage$code_lines == 0] <- 100

  dev_data$coverage <- add_coverage_summary(dev_data$coverage)

  dev_data$coverage_filter <- filter

  class(dev_data$coverage) <- c("coverage_table2", class(dev_data$coverage))
  class(dev_data) <- c("package_coverage", class(dev_data))

  dev_data
}

#' @rdname package_coverage
#' @details
#'
#' `t()` is an alias of `package_coverage()`.
#' @export

t <- package_coverage

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

add_coverage_summary <- function(coverage) {
  # by directory
  dirs <- as.factor(dirname(coverage$path))
  bd_line_count <- tapply(coverage$line_count, dirs, sum)
  bd_code_lines <- tapply(coverage$code_lines, dirs, sum)
  bd_lines_covered <- tapply(coverage$lines_covered, dirs, sum)

  sm <- data.frame(
    name = c("All files", levels(dirs)),
    line_count = c(sum(coverage$line_count), bd_line_count),
    code_lines = c(sum(coverage$code_lines), bd_code_lines),
    lines_covered = c(sum(coverage$lines_covered), bd_lines_covered)
  )

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
  structure(
    lapply(names, function(name) {
      .Call(c_cov_get_counts, get(name, envir = .GlobalEnv))
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

create_copy_plan <- function(
  src = ".",
  pkgname = desc::desc_get("Package", src),
  copy = character()
) {
  withr::local_dir(src)

  topfiles <- dir(all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
  topplan <- data.frame(
    path = topfiles,
    isdir = is_dir(topfiles),
    action = ifelse(topfiles %in% copy, "copy", "link")
  )
  topplan <- exclude_build_ignored(topplan, src = ".", pkgname = pkgname)

  rest <- dir(
    topplan$path[topplan$isdir & topplan$action == "copy"],
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE
  )
  restplan <- data.frame(
    path = rest,
    isdir = is_dir(rest),
    action = rep("copy", length(rest))
  )
  restplan <- exclude_build_ignored(restplan, src = ".", pkgname = pkgname)

  rbind(topplan, restplan)
}

create_update_plan <- function(
  src,
  dst,
  pkgname = desc::desc_get("Package", src),
  copy = character()
) {
  plan <- create_copy_plan(src, pkgname, copy)
  plan$target <- file.path(dst, pkgname, plan$path)
  plan$hash <- NA_character_
  plan$hash[!plan$isdir] <- cli::hash_file_xxhash(plan$path[!plan$isdir])

  update <- list(
    delete = character(),
    add = character(),
    update = character()
  )
  plan_file <- file.path(dst, "plan.rds")
  if (file.exists(plan_file)) {
    oldplan <- readRDS(plan_file)
    # delete paths that are not in the new plan
    update$delete <- setdiff(oldplan$path, plan$path)
    # add paths that are new in the plan, or don't exist
    update$add <- unique(c(
      setdiff(plan$path, oldplan$path),
      plan$path[!file.exists(plan$target)]
    ))
    # check if we need to update files:
    # - dir -> file or file -> dir change
    # - action change
    # - hash of file change
    common_paths <- intersect(plan$path, oldplan$path)
    cold <- oldplan[match(common_paths, oldplan$path), ]
    cnew <- plan[match(common_paths, plan$path), ]
    acthash <- rep(NA_character_, length(common_paths))
    tohash <- !cnew$isdir & !is_link(cnew$target)
    acthash[tohash] <- cli::hash_file_xxhash(cnew$target[tohash])
    update$update <- common_paths[
      cold$isdir != cnew$isdir |
        cold$action != cnew$action |
        (cnew$action == "copy" &
          !cold$isdir &
          !cnew$isdir &
          !is_link(cnew$target) &
          cnew$hash != acthash)
    ]
  } else {
    update[["add"]] <- plan$path
  }

  list(plan = plan, update = update)
}

update_package_tree <- function(
  src,
  dst,
  pkgname = desc::desc_get("Package", src),
  copy = character()
) {
  withr::local_dir(src)

  plan <- create_update_plan(src, dst, pkgname, copy)
  upd <- plan$update
  plan <- plan$plan

  # make sure that target dir exists
  mkdirp(file.path(dst, pkgname))

  todel <- file.path(dst, pkgname, c(upd$delete, upd$update))
  unlink(todel, force = TRUE, recursive = TRUE)
  planadd <- plan[plan$path %in% c(upd$add, upd$update), ]

  # TODO: make relative symlinks, no absolute
  wd <- getwd()
  for (i in seq_len(nrow(planadd))) {
    path <- planadd$path[i]
    action <- planadd$action[i]
    target <- planadd$target[i]
    isdir <- planadd$isdir[i]
    if (action == "link") {
      file.symlink(file.path(wd, path), target)
    } else if (isdir) {
      # files are copied later
      mkdirp(target)
    } else {
      mkdirp(dirname(target))
      file.copy(path, target)
    }
  }

  plan_file <- file.path(dst, "plan.rds")
  saveRDS(plan, plan_file)
  setup_file <- file.path(dst, "setup.rds")
  saveRDS(getOption(opt_setup), setup_file)

  invisible(plan)
}

is_dir <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}

is_link <- function(x) {
  rl <- Sys.readlink(x)
  !is.na(rl) & rl != ""
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

cov_instrument_dir <- function(path = "R", exclusion_file = ".covrignore") {
  rfiles <- apply_covrignore(
    dir(path, pattern = "[.][rR]$", full.names = TRUE),
    exclusion_file
  )
  fls <- data.frame(
    path = rfiles,
    symbol = paste0(".__cov_", tools::file_path_sans_ext(basename(rfiles))),
    line_count = NA_integer_,
    code_lines = NA_integer_,
    lines_covered = 0L,
    percent_covered = 0,
    lines = I(replicate(length(rfiles), NULL, simplify = FALSE)),
    uncovered = I(replicate(length(rfiles), list(), simplify = FALSE))
  )
  for (i in seq_along(rfiles)) {
    fls$lines[[i]] <- cov_instrument_file(fls$path[i], fls$symbol[i])
    fls$line_count[i] <- nrow(fls$lines[[i]])
    fls$code_lines[i] <- sum(fls$lines[[i]]$status == "instrumented")
  }

  fls
}

cov_instrument_file <- function(path, cov_symbol) {
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
  inj_posl <- lapply(brc_poss, get_inject_positions, psd)
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

  # need to insert code concurrently to the same line, for future
  # multi-expression per line support
  lns0 <- lns <- readLines(path)
  inj_lines <- unique(inj$line1)
  for (il in inj_lines) {
    inj1 <- inj[inj$line1 == il, ]
    lns[il] <- str_insert_parallel(lns[il], inj1$col1, inj1$code)
  }
  writeLines(lns, path)

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

  res$status[inj$line1] <- "instrumented"
  res$id[inj$line1] <- inj$line1

  # Handle multi-line expressions. We need to do this backwards, so nested
  # braces work out correctly.
  for (i in rev(seq_len(nrow(inj)))) {
    inji <- inj[i, , drop = FALSE]
    li <- inji$line1:inji$line2
    if (length(li) == 1) {
      next
    }
    cnt <- li[is.na(res$status[li])]
    res$status[cnt] <- "instrumented"
    res$id[cnt] <- inji$line1
  }
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

  res
}

parse_line_exclusions <- function(lns, path) {
  sort(unique(c(
    parse_line_exclusions_single(lns, path),
    parse_line_exclusions_ranges(lns, path)
  )))
}

parse_line_exclusions_single <- function(lns, path) {
  which(
    grepl("__NO_COVERAGE__", lns) |
      grepl("#[ ]*nocov", lns) |
      grepl("//[ ]*nocov", lns) |
      grepl("/[*][ ]*nocov[ ]* [*]/", lns) |
      grepl("LCOV_EXCL_LINE", lns)
  )
}

parse_line_exclusions_ranges <- function(lns, path) {
  start <- unique(sort(c(
    grep("#[ ]*nocov[ ]+(start|begin)", lns),
    grep("LCOV_EXCL_START", lns)
  )))
  end <- unique(sort(c(
    grep("#[ ]*nocov[ ]+end", lns),
    grep("LCOV_EXCL_STOP", lns)
  )))

  length(start) <- length(end) <- max(length(start), length(end))
  if (length(start) == 0) {
    return(integer())
  }

  keep <- rep(TRUE, length(lns))
  ranges <- data.frame(stringsAsFactors = FALSE, start = start, end = end)

  for (i in seq_len(nrow(ranges))) {
    if (is.na(ranges$start[i])) {
      stop(cli::format_error(
        "Found {.code # nocov end} without {.code # nocov start} at
        {.path {path}}:{ranges$end[i]}."
      ))
    }
    if (is.na(ranges$end[i]) || ranges$start[i] > ranges$end[i]) {
      stop(cli::format_error(
        "Found {.code # nocov start} without {.code # nocov end} at
        {.path {path}}:{ranges$start[i]}."
      ))
    }
    keep[ranges$start[i]:ranges$end[i]] <- FALSE
  }

  which(!keep)
}

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

setup_cov_env <- function(cov_data) {
  for (i in seq_len(nrow(cov_data))) {
    assign(
      cov_data$symbol[i],
      .Call(c_cov_make_counter, cov_data$line_count[i]),
      envir = .GlobalEnv
    )
  }
}

re_exclude <- function(pkg) {
  c(
    paste0(
      "(?i)", # these are case insensitive
      c(
        "[.]o$",
        "[.]so$",
        "[.]dll$",

        "(^|/)\\.DS_Store$", # by macOS finder
        "^\\.RData$", # .RData at /
        "~$",
        "\\.bak$",
        "\\.swp$", # backup files
        "(^|/)\\.#[^/]*$",
        "(^|/)#[^/]*#$", # more backup files (Emacs)

        "^config\\.(cache|log|status)$", # leftover by autoconf
        "(^|/)autom4te\\.cache$",

        "^src/.*\\.d$",
        "^src/Makedeps$", # INSTALL leftover on Windows

        "^inst/doc/Rplots\\.(ps|pdf)$" # Sweave leftover
      )
    ),

    "(^|/)\\._[^/]*$", # macOS resource forks

    paste0(
      # hidden files
      "(^|/)\\.",
      c(
        "Renviron",
        "Rprofile",
        "Rproj.user",
        "Rhistory",
        "Rapp.history",
        "tex",
        "log",
        "aux",
        "pdf",
        "png",
        "backups",
        "cvsignore",
        "cproject",
        "directory",
        "dropbox",
        "exrc",
        "gdb.history",
        "gitattributes",
        "github",
        "gitignore",
        "gitmodules",
        "hgignore",
        "hgtags",
        "htaccess",
        "latex2html-init",
        "project",
        "seed",
        "settings",
        "tm_properties"
      ),
      "$"
    ),

    paste0(
      "(^|/)",
      pkg,
      "_[0-9.-]+",
      "\\.(tar\\.gz|tar|tar\\.bz2|tar\\.xz|tgz|zip)",
      "$"
    )
  )
}

re_exclude_dir <- function(pkg) {
  c(
    "^revdep$", # revdepcheck
    paste0(
      # VC
      "(^|/)",
      c(
        "CVS",
        ".svn",
        ".arch-ids",
        ".bzr",
        ".git",
        ".hg",
        "_darcs",
        ".metadata"
      ),
      "$"
    ),

    "(^|/)[^/]*[Oo]ld$",
    "(^|/)[^/]*\\.Rcheck",

    "^src.*/\\.deps$"
  )
}

#' @export

`[.coverage_table2` <- function(x, i, j, drop = FALSE) {
  class(x) <- setdiff(class(x), "coverage_table2")
  NextMethod("[")
}

#' @export

format.coverage_table2 <- function(x, filter = NULL, ...) {
  if (is.null(filter)) {
    format_coverage_table2_full(x, ...)
  } else {
    format_coverage_table2_filter(x, filter, ...)
  }
}

format_coverage_table2_filter <- function(x, filter, ...) {
  mch <- grepl(filter, x$path)
  if (length(mch) == 0) {
    warning(
      "No matching code file for '",
      filter,
      "', showing full coverage table"
    )
    return(format_coverage_table2_full(x, ...))
  }

  x <- x[mch, ]
  bl <- format_pct(x$percent_covered)
  cffn <- ffn <- format(c("code coverage", "", x$path))
  cfbl <- fbl <- format(c("% lines", "", bl))

  mid <- 3:(length(ffn))
  cffn[mid] <- cov_col(ffn[mid], x$percent_covered)
  cfbl[mid] <- cov_col(fbl[mid], x$percent_covered)

  lines <- paste0(cffn, " \u2502 ", cfbl, "\u2502 ")
  maxw <- max(cli::ansi_nchar(lines, type = "width"))
  cw <- cli::console_width()

  uc <- mapply(format_uncovered, x$uncovered, file = x$path, width = 1000L)
  uc <- cli::ansi_strwrap(
    uc,
    width = cw - maxw,
    exdent = maxw,
    simplify = FALSE
  )
  ucls <- lengths(uc)
  uc <- map_chr(uc, paste, collapse = "\n")
  if (any(ucls > 1)) {
    cuc <- c("uncovered line #", "", uc)
    cuc[1] <- cli::ansi_align(cuc[1], width = cw - maxw)
  } else {
    cuc <- cli::ansi_align(
      c("uncovered line #", "", uc),
      width = max(cli::ansi_nchar(uc, "width"))
    )
  }
  cuc[mid] <- cov_col(cuc[mid], x$percent_covered)
  lines <- paste0(lines, cuc)

  # top line
  lines[1] <- cli::style_bold(style_bg_grey(cli::col_white(lines[1])))

  lines
}

format_coverage_table2_full <- function(x, ...) {
  sm <- attr(x, "summary")
  fn0 <- c(sm$name[1], paste0(sm$name[-1], "/"), x$path)
  fn <- c(sm$name[1], paste0(sm$name[-1], "/"), paste0(" ", x$path))
  rl <- c(sm$percent_covered, x$percent_covered)
  bl <- format_pct(rl)

  cffn <- ffn <- format(c("code coverage", "", fn, "", "total"))
  cfbl <- fbl <- format(c("% lines", "", bl, "", bl[1]), justify = "right")

  mid <- 3:(length(ffn) - 2)
  cffn[mid] <- cov_col(ffn[mid], rl)
  cfbl[mid] <- cov_col(fbl[mid], rl)

  lines <- paste0(cffn, " \u2502 ", cfbl, " \u2502 ")
  maxw <- max(cli::ansi_nchar(lines, type = "width"))
  cw <- cli::console_width()

  uc <- mapply(format_uncovered, x$uncovered, file = x$path, width = cw - maxw)
  cuc <- cli::ansi_align(
    c("uncovered line #", "", rep("", nrow(sm)), uc, "", ""),
    width = max(cli::ansi_nchar(uc, "width"))
  )
  tot <- sm$percent_covered
  cuc[mid] <- cov_col(cuc[mid], c(tot, x$percent_covered))
  lines <- paste0(lines, cuc)

  # top line
  lines[1] <- cli::style_bold(style_bg_grey(cli::col_white(lines[1])))

  # bottom line
  if (tot[1] < 75) {
    lines[length(lines)] <- style_bg_orange(lines[length(lines)])
  } else if (tot[1] < 95) {
    lines[length(lines)] <-
      cli::bg_blue(cli::col_white(lines[length(lines)]))
  } else {
    lines[length(lines)] <-
      style_bg_green(cli::col_white(lines[length(lines)]))
  }
  lines[length(lines)] <- cli::style_bold(lines[length(lines)])

  # directory summaries at the right place
  lines[mid] <- lines[mid][c(1, order(fn0[-1]) + 1)]

  lines
}

#' @export

print.coverage_table2 <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

#' @export

format.package_coverage <- function(x, ...) {
  c("", format(x$coverage, filter = x$coverage_filter, ...))
}

#' @export

print.package_coverage <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}

gcov_cleanup <- function(path) {
  path <- file.path(path, "src")
  gcda <- dir(path, pattern = "[.]gcda$", full.names = TRUE, recursive = TRUE)
  gcov <- dir(path, pattern = "[.]gcov$", full.names = TRUE, recursive = TRUE)
  unlink(c(gcda, gcov))
}

load_c_coverage <- function(path, exclusion_file = NULL) {
  withr::local_dir(path)
  gcno <- dir("src", pattern = "[.]gcno$", full.names = TRUE, recursive = TRUE)
  gcno <- apply_covrignore(gcno, exclusion_file)

  # Need to run gcov separately for each subdirectory that has gcno files
  dirs <- unique(dirname(gcno))

  pxs <- lapply(dirs, function(d) {
    fnms <- dir(d, recursive = TRUE, pattern = "[.]gcno$")
    processx::process$new("gcov", c("-p", "-b", fnms), wd = d)
  })

  while (length(pxs) > 0) {
    pr <- processx::poll(pxs, 1000)
    pr <- vapply(pr, "[[", "", "process")
    dn <- pr == "ready"
    st <- vapply(pxs[dn], function(p) p$get_exit_status(), 1L)
    oh <- pxs[dn][st != 0]
    if (length(oh)) {
      warning(
        "gcov failed for directories: ",
        paste(names(pxs)[oh], collapse = ", ")
      )
    }
    pxs <- pxs[!dn]
  }

  ccov <- parse_gcov(".")
  ccov <- ccov[map_int(ccov, nrow) > 0]

  # need to apply exclusions again, because dependent and potentially
  # excluded .h files were still picked up
  paths <- sub("^[.]/", "", map_chr(ccov, function(x) x$file[1]))
  keep <- paths %in% apply_covrignore(paths, exclusion_file)
  ccov <- ccov[keep]

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

  res <- data.frame(
    stringsAsFactors = FALSE,
    path = sub("^[.]/", "", map_chr(ccov, function(x) x$file[1])),
    symbol = NA_character_,
    line_count = map_int(ccov, nrow),
    code_lines = map_int(ccov, function(x) sum(!is.na(x$coverage))),
    lines_covered = map_int(ccov, function(x) {
      sum(x$coverage > 0, na.rm = TRUE)
    }),
    percent_covered = NA_real_,
    lines = I(replicate(length(ccov), NULL, simplify = FALSE)),
    uncovered = I(replicate(length(ccov), NULL, simplify = FALSE))
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
  }

  res
}

parse_gcov <- function(root = ".") {
  gcov <- dir(
    root,
    recursive = TRUE,
    pattern = "[.]gcov$",
    full.names = TRUE
  )

  # drop files that have an absolute path, typically system headers
  gcov <- gcov[!startsWith(basename(gcov), "#")]

  ps <- lapply(gcov, parse_gcov_file)
  ps
}

parse_gcov_file <- function(path) {
  disp <- display_name(path)
  df <- .Call(c_cov_parse_gcov, path, disp)
  class(df) <- c("tbl", "data.frame")
  attr(df, "row.names") <- seq_len(length(df[[1]]))
  df
}

display_name <- function(x) {
  x <- sub("[.]gcov", "", x)
  if (startsWith(basename(x), "^")) {
    x <- gsub("^", "..", fixed = TRUE, x)
    x <- gsub("#", "/", fixed = TRUE, x)
    x
  } else {
    b <- gsub("#", "/", basename(x))
    paste0(dirname(x), "/", b)
  }
}

#' List development builds
#'
#' @inheritParams load_package
#' @return Data frame with columns:
#'   - `type`: Build type, character.
#'   - `r_version`: R version, first two digits only, character.
#'   - `platform`: Build platform triplet, character.
#'   - `last_built`: Time stamp of the last build, [POSIXct].
#'   - `dist_size`: Size of the build directory in bytes, double.
#'   - `id`: Name of the build directory.
#' @export

list_builds <- function(path = ".") {
  withr::local_dir(path)
  dirs <- dir(get_dev_dir(), full.names = TRUE, include.dirs = TRUE)
  setup_paths <- file.path(dirs, "setup.rds")
  setups <- lapply(setup_paths, readRDS)
  builds <- data.frame(
    stringsAsFactors = FALSE,
    type = map_chr(setups, "[[", "type"),
    r_version = map_chr(setups, "[[", "rver"),
    platform = map_chr(setups, "[[", "platform"),
    last_built = file.mtime(setup_paths),
    disk_size = dir_size(dirs),
    id = basename(dirs)
  )
  rownames(builds) <- NULL

  # Better printing if pillar is available
  class(builds) <- c("tbl", class(builds))
  requireNamespace("pillar", quietly = TRUE)

  builds
}

dir_size <- function(dirs) {
  map_dbl(dirs, function(dir) {
    paths <- list.files(dir, recursive = TRUE, full.names = TRUE)
    paths <- paths[!is_link(paths)]
    sum(file.size(paths))
  })
}
