#' Install local package tree
#'
#' @param lib,INSTALL_opts,... Additional arguments are passed to
#'   [utils::install.packages()].
#' @inheritParams reload
#'
#' @export

install <- function(
  type = c("release", "debug", "coverage"),
  path = ".",
  makeflags = NULL,
  clean = FALSE,
  lib = NULL,
  INSTALL_opts = c("--no-staged-install", "--no-test-load"),
  ...
) {
  lib <- lib %||%
    grep("__dev_lib__", .libPaths(), value = TRUE, invert = TRUE)[1]
  withr::local_dir(path)
  type <- match.arg(type)
  setup <- reload_setup(type, ".", makeflags)
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

  pkg_dir <- file.path(setup$dir, setup$pkgname)
  inst <- utils::install.packages(
    pkg_dir,
    lib = lib,
    repos = NULL,
    type = "source",
    INSTALL_opts = INSTALL_opts,
    ...
  )

  invisible(list(
    setup = setup,
    plan = plan,
    inst = inst
  ))
}
