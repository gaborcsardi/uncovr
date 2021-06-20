
#' @export

reload <- function(covr = FALSE) {
  desc <- read.dcf("DESCRIPTION")
  pkg <- desc[, "Package"][[1]]

  # -----------------------------------------------------------------------

  recompile_if_needed(covr)

  # -----------------------------------------------------------------------

  if (pkg %in% loadedNamespaces()) unload(pkg)

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
  suppressMessages(tools:::.install_packages(args = c(".", inst_args)))

  # -----------------------------------------------------------------------

  collate_r_files(".", file.path(dev_lib, pkg, "R", pkg))
  ns <- loadNamespace(
    pkg,
    keep.source = TRUE,
    keep.parse.data = TRUE,
    partial = TRUE
  )

  # -----------------------------------------------------------------------

  if (covr) asNamespace("covr")$trace_environment(ns)
  make_ns_info(pkg, dev_lib)
  make_lazy_load_db(dev_lib, pkg, ns)
  install_sysdata(
    file.path("R", "sysdata.rda"),
    file.path(dev_lib, pkg, "R", "sysdata")
  )
  trace_dir <- file.path(normalizePath(dev_lib), "_traces")

  # -----------------------------------------------------------------------

  library(pkg, character.only = TRUE)
  if (covr) add_covr_save(trace_dir, file.path(dev_lib, pkg, "R", pkg))

  invisible(ns)
}

recompile_if_needed <- function(covr = TRUE) {
  if (!file.exists("src")) return()

  if (covr) {
    objfs <- list.files("src", pattern = "[.]o$", recursive = TRUE,
                        full.names = TRUE)
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
  suppressMessages(tools:::.install_packages(args = c(".", inst_args)))
}

covr_flags <- function() {
  c(CFLAGS = "-O0 --coverage",
    CXXFLAGS = "-O0 --coverage",
    CXX1XFLAGS = "-O0 --coverage",
    CXX11FLAGS = "-O0 --coverage",
    CXX14FLAGS = "-O0 --coverage",
    CXX17FLAGS = "-O0 --coverage",
    CXX20FLAGS = "-O0 --coverage",

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
