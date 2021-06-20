
#' @export

reload <- function(covr = TRUE) {
  desc <- read.dcf("DESCRIPTION")
  pkg <- desc[, "Package"][[1]]

  if (pkg %in% loadedNamespaces()) unload(pkg)

  # -----------------------------------------------------------------------

  inst_args <- c(
    ".",
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

  asNamespace("covr")$trace_environment(ns)
  make_ns_info(pkg, dev_lib)
  make_lazy_load_db(dev_lib, pkg, ns)
  install_sysdata(
    file.path("R", "sysdata.rda"),
    file.path(dev_lib, pkg, "R", "sysdata")
  )
  trace_dir <- file.path(normalizePath(dev_lib), "_traces")

  # -----------------------------------------------------------------------

  library(pkg, character.only = TRUE)
  add_covr_save(trace_dir, file.path(dev_lib, pkg, "R", pkg))
  
}
