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
