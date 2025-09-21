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
