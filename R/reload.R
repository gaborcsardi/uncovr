#' Reload package
#'
#' @param covr Whether to trace functions for covr.
#' @param internals Whether to attach internal functions to the search path.
#' @param helpers Whether to load testthat helper functions.
#' @export

reload <- function(covr = FALSE, internals = FALSE, helpers = FALSE) {
  desc <- read.dcf("DESCRIPTION")
  pkg <- desc[, "Package"][[1]]

  withr::local_envvar(R_OSTYPE = .Platform$OS.type)

  # -----------------------------------------------------------------------

  recompile_if_needed(covr)

  # -----------------------------------------------------------------------
  # Do this before unloading, because the tools we use might depend on the
  # package we unload, e.g. desc depends on cli

  collate <- desc::desc_get_collate(file = ".")

  # -----------------------------------------------------------------------

  hlpname <- paste0("helpers:", pkg)
  if (hlpname %in% search()) {
    detach(hlpname, character.only = TRUE)
  }
  if (pkg %in% loadedNamespaces()) {
    unload(pkg)
  }

  inst_args <- c(
    "--no-staged-install",
    "--no-byte-compile",
    "--no-help",
    "--no-R",
    "--no-docs",
    "--no-html",
    "--no-demo",
    "--no-multiarch",
    "--no-test-load",
    "--use-vanilla"
  )
  dev_lib <- "dev-lib"
  mkdirp(dev_lib)
  withr::local_libpaths(dev_lib, action = "prefix")
  suppressMessages(asNamespace("tools")$.install_packages(
    args = c(".", inst_args)
  ))

  # -----------------------------------------------------------------------

  collate_r_files(".", file.path(dev_lib, pkg, "R", pkg), collate = collate)
  ns <- loadNamespace(
    pkg,
    keep.source = TRUE,
    keep.parse.data = TRUE,
    partial = TRUE
  )

  # -----------------------------------------------------------------------

  if (covr) {
    asNamespace("covr")$trace_environment(ns)
  }
  make_ns_info(pkg, dev_lib)
  make_lazy_load_db(dev_lib, pkg, ns)
  install_sysdata(
    file.path("R", "sysdata.rda"),
    file.path(dev_lib, pkg, "R", "sysdata")
  )
  trace_dir <- file.path(normalizePath(dev_lib), "_traces")

  # -----------------------------------------------------------------------

  if (internals) {
    loadNamespace(pkg)
    attach(
      asNamespace(pkg),
      name = paste0("package:", pkg),
      warn.conflicts = FALSE
    )
  } else {
    library(pkg, character.only = TRUE)
  }

  if (helpers) {
    hlp_env <- new.env(parent = asNamespace(pkg))
    helpers <- list.files(
      "tests/testthat",
      pattern = "^helper.*[.][rR]",
      full.names = TRUE
    )
    for (hlp in helpers) {
      sys.source(hlp, envir = hlp_env)
    }
    attach(hlp_env, name = paste0("helpers:", pkg))
  }

  if (covr) {
    add_covr_save(trace_dir, file.path(dev_lib, pkg, "R", pkg))
  }

  invisible()
}

recompile_if_needed <- function(covr = TRUE) {
  if (!file.exists("src")) {
    return()
  }

  if (covr) {
    objfs <- list.files(
      "src",
      pattern = "[.]o$",
      recursive = TRUE,
      full.names = TRUE
    )
    for (of in objfs) {
      out <- processx::run("nm", of)
      if (!grepl("_gcov_", out$stdout)) {
        unlink(of)
      }
    }

    withr::local_makevars(covr_flags(), assignment = "+=")
  }

  mkdirp(tmplib <- tempfile())
  on.exit(unlink(tmplib, recursive = TRUE), add = TRUE)
  inst_args <- c("-l", tmplib, "--libs-only", "--no-test-load")
  suppressMessages(asNamespace("tools")$.install_packages(
    args = c(".", inst_args)
  ))
}

covr_flags <- function() {
  c(
    CFLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXXFLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXX1XFLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXX11FLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXX14FLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXX17FLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXX20FLAGS = "-O0 --coverage -DGCOV_COMPILE",

    FFLAGS = "-O0 --coverage",
    FCFLAGS = "-O0 --coverage",
    FLIBS = "-lgcov",

    # LDFLAGS is ignored on windows and visa versa
    LDFLAGS = if (!is_windows()) "--coverage" else NULL,
    SHLIB_LIBADD = if (is_windows()) "--coverage" else NULL
  )
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}
