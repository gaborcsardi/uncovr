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
  lib <- normalizePath(lib)
  tgt <- normalizePath(tgt)
  Sys.chmod(lib, mode = "0755", use_umask = FALSE)
  on.exit(Sys.chmod(lib, mode = "0555", use_umask = FALSE))

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
