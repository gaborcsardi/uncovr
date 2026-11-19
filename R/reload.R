reload_setup <- function(path = ".", build_dir = tempdir(), makeflags = NULL) {
  withr::local_dir(path)
  makeflags <- makeflags %||% get_makeflags()

  # setup build options
  setup <- list(
    version = 4L,
    rver = as.character(getRversion()[, 1:2]),
    platform = R.Version()$platform,
    type = "coverage"
  )
  setup[["hash"]] <- cli::hash_obj_sha1(setup)
  setup[["compiler_flags"]] <- makeflags
  setup[["pkgname"]] <- unname(desc::desc_get("Package", "."))
  setup[["dir"]] <- get_dev_dir(build_dir, setup$pkgname, setup)
  setup[["pkgversion"]] <- unname(desc::desc_get("Version", "."))

  setup
}

# Update the build tree, instrument the R code, create the coverage
# counters and load the instrumented package with `pkgload::load_all()`.

load_instrumented <- function(
  path = ".",
  build_dir = tempdir(),
  clean = FALSE
) {
  mkdirp(build_dir)
  build_dir <- normalizePath(build_dir)
  withr::local_dir(path)
  setup <- reload_setup(".", build_dir = build_dir)
  withr::local_options(structure(list(setup), names = opt_setup))

  if (clean) {
    unlink(setup[["dir"]], recursive = TRUE)
  }

  cli::cli_alert_info("Updating build tree")
  plan <- update_package_tree(".", setup$dir, pkgname = setup$pkgname)

  pkg_dir <- file.path(setup$dir, setup$pkgname)

  # It is probably ignored by R CMD build, so need to use the master file
  exc <- if (file.exists(".covrignore")) {
    normalizePath(".covrignore")
  }
  # a counters file from a previous run must not be instrumented
  remove_counters_file(pkg_dir)
  withr::with_dir(pkg_dir, {
    cov_data <- cov_instrument_dir("R", setup$pkgname, exclusion_file = exc)
    cov_data <- cov_data$files
  })
  write_counters_file(pkg_dir, cov_data)
  withr::local_options(
    structure(list(make_counters(cov_data, setup)), names = opt_counters)
  )

  withr::local_options(pkg.build_extra_flags = FALSE)
  withr::local_makevars(setup$compiler_flags, .assignment = "+=")

  loaded <- pkgload::load_all(pkg_dir)

  cov_data <- find_function_names(cov_data, loaded$env, setup)

  list(
    setup = setup,
    plan = plan,
    load = loaded,
    coverage = cov_data
  )
}

find_function_names <- function(cov_data, env, setup) {
  pkgdir <- paste0(file.path(setup$dir, setup$pkgname), "/")
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
      ol1 <- utils::getSrcLocation(fun, "line", first = TRUE)
      ol2 <- utils::getSrcLocation(fun, "line", first = FALSE)
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
