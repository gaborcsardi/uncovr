counters_file_name <- "000-uncovr-counters.R"
opt_counters <- "uncovr.counters"

# The instrumented code looks up the counters with `get0()`, so they need
# to be in the package namespace (or one of its parent environments).
#
# We create the counters before loading the package, and pass them to
# `pkgload::load_all()` in an option, inside an environment. An extra R
# file, which is loaded before the other R files of the package, then
# assigns them in the package namespace. If the counters are not found, e.g. for a function
# whose environment was changed, then `get0()` returns `NULL`, and the
# expression is not counted.

remove_counters_file <- function(pkg_dir) {
  unlink(file.path(pkg_dir, "R", counters_file_name))
}

write_counters_file <- function(pkg_dir, cov_data) {
  lines <- c(
    "# Code coverage counters, created by uncovr",
    "local({",
    sprintf("  cnt <- base::getOption(\"%s\")", opt_counters),
    "  if (!base::is.environment(cnt)) return()",
    "  ns <- base::topenv()",
    "  for (nm in base::ls(cnt, all.names = TRUE)) {",
    "    base::assign(nm, base::get(nm, envir = cnt), envir = ns)",
    "  }",
    "})"
  )
  write_lines_safe(lines, file.path(pkg_dir, "R", counters_file_name))

  # If there is a Collate field, the counters file must come first
  desc_path <- file.path(pkg_dir, "DESCRIPTION")
  dsc <- desc::desc(file = desc_path)
  collate <- dsc$get_collate()
  if (length(collate) > 0 && collate[1] != counters_file_name) {
    dsc$set_collate(c(counters_file_name, setdiff(collate, counters_file_name)))
    # The file might be a hard link to the original DESCRIPTION file, so
    # we must not modify it in place.
    tmp <- tempfile(tmpdir = pkg_dir)
    on.exit(unlink(tmp), add = TRUE)
    dsc$write(file = tmp)
    file.rename(tmp, desc_path)
  }
}

# Returns an environment, because `options()` would duplicate the
# counters, and a duplicate is a regular integer vector, not a counter.

make_counters <- function(cov_data, setup) {
  counter_routines(setup)
  env <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(cov_data))) {
    assign(
      cov_data$symbol[i],
      .Call(routines$make, as.integer(cov_data$num_markers[i])),
      envir = env
    )
  }
  env
}

# The ALTREP class of the counters lives in our DLL, so the DLL must stay
# loaded while the counters are in use. When measuring the coverage of
# uncovr itself, `pkgload::load_all()` unloads our DLL, so we use a copy
# of it, under a different name, and call its native routines.
# `routines` is filled in `.onLoad()` with our own native routines.

routines <- new.env(parent = emptyenv())

counter_routines <- function(setup) {
  if (setup$pkgname != utils::packageName()) {
    routines$make <- c_cov_make_counter
    routines$get <- c_cov_get_counts
    return(routines)
  }
  dll <- getLoadedDLLs()[["cov_xx"]]
  if (is.null(dll)) {
    src <- getLoadedDLLs()[[utils::packageName()]][["path"]]
    tgt <- file.path(
      normalizePath(setup$dir),
      paste0("cov_xx", .Platform$dynlib.ext)
    )
    if (!file.exists(tgt)) {
      file.copy(src, tgt)
    }
    dll <- dyn.load(tgt)
  }
  routines$make <- getNativeSymbolInfo("cov_make_counter", dll)
  routines$get <- getNativeSymbolInfo("cov_get_counts", dll)
  routines
}

cov_get_counts <- function(names, env, setup) {
  counter_routines(setup)
  structure(
    lapply(names, function(name) {
      .Call(routines$get, get(name, envir = env, inherits = FALSE))
    }),
    names = names
  )
}
