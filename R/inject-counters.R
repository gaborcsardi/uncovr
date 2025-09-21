setup_cov_env <- function(cov_data) {
  on.exit(.Call(c_cov_lock_base), add = TRUE)
  env <- if (.Call(c_cov_unlock_base)) baseenv() else globalenv()
  for (i in seq_len(nrow(cov_data))) {
    assign(
      cov_data$symbol[i],
      .Call(c_cov_make_counter, cov_data$num_markers[i]),
      envir = env
    )
  }
}

find_code <- function(path = ".", quiet = FALSE) {
  path_r <- file.path(path, "R")
  r_files <- withr::with_collate(
    "C",
    tools::list_files_with_type(path_r, "code", full.names = TRUE)
  )
  collate <- desc::desc_get_collate(file = path)
  if (length(collate) > 0) {
    collate <- file.path(path_r, collate)
    missing <- setdiff(collate, r_files)
    if (!quiet && length(missing) > 0) {
      cli::cli_alert_info("Skipping missing files: {.file {missing}}")
    }
    collate <- setdiff(collate, missing)
    extra <- setdiff(r_files, collate)
    if (!quiet && length(extra) > 0) {
      cli::cli_alert_info(
        "Adding files missing in collate: {.file {extra}}"
      )
    }
    r_files <- union(collate, r_files)
  }
  r_files
}

setup_cov_inject_script <- function(target, cov_data) {
  script <- file.path(target, "R", "inject.R")
  ttlsoname <- paste0("uncovr", .Platform$dynlib.ext)
  ttlpkg <- find.package("uncovr")
  ttlso <- file.path(ttlpkg, "libs", ttlsoname)
  if (!file.exists(ttlso)) {
    ttlso <- file.path(ttlpkg, "src", ttlsoname)
  }
  stopifnot(file.exists(ttlso))
  mkdirp(file.path(target, "libs"))
  tgtsoname <- paste0("cov_xx", .Platform$dynlib.ext)
  file.copy(ttlso, file.path(target, "libs", tgtsoname))

  lns <- c(
    "# coverage counter injection by uncovr",
    "local({",
    "  info <- loadingNamespaceInfo()",
    "  pkg <- info$pkgname",
    "  tgtsoname <- paste0('cov_xx', .Platform$dynlib.ext)",
    "  dl <- dyn.load(file.path(info$libname, pkg, 'libs', tgtsoname))",
    "  lb <- getNativeSymbolInfo('cov_lock_base', dl)",
    "  ulb <- getNativeSymbolInfo('cov_unlock_base', dl)",
    "  mc <- getNativeSymbolInfo('cov_make_counter', dl)",
    "  mycall <- base::.Call",
    "  env <- if (mycall(ulb)) baseenv() else globalenv()",
    sprintf(
      "  assign('%s', mycall(mc, %dL), envir = env)",
      cov_data$symbol,
      cov_data$num_markers
    ),
    "  mycall(lb)",
    "  output <- file.path(",
    "    dirname(info$libname), ",
    "    '_cov',",
    "    'sub',",
    "    paste0(Sys.getpid(), '.rda')",
    "  )",
    "  dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)",
    "  reg.finalizer(",
    "    env,",
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

# Copy uncovr.so into the build directory, so we can read it
# from there
inject_covxxso <- function(build_dir) {
  soname <- paste0("uncovr", .Platform$dynlib.ext)
  covxxso <- system.file(package = "uncovr", "libs", soname)
  if (covxxso == "") {
    covxxso <- system.file(package = "uncovr", "src", soname)
  }
  if (covxxso == "") {
    stop("Could not find ", soname, " for test coverage counter injection")
  }
  soname2 <- paste0("cov_xx", .Platform$dynlib.ext)
  target <- file.path(build_dir, soname2)
  file.copy(covxxso, target)
  target
}

# In the main process we create the counters manually before loading
# the package. But we need to create the counters when loading the package
# with `load_all()` in a subprocess, we do that here.
#
# These additional commands do not create any objects, so they do not run
# when the (quick) installed package is loaded by `library()`. For that
# case we patch the namespace loader file that loads the package to create
# the counters.

create_counters_lines <- function(setup, cov_data) {
  outdir <- file.path(setup$dir, cov_sub_dir_name)
  mkdirp(outdir)
  subs <- list(
    covxxso_ = normalizePath(setup$covxxso),
    symbols_ = cov_data$symbol,
    nmarkers_ = cov_data$num_markers,
    outdir_ = normalizePath(outdir)
  )
  deparse(
    substitute(
      local({
        dl <- base::dyn.load(covxxso_)
        mc <- base::getNativeSymbolInfo("cov_make_counter", dl)
        lb <- base::getNativeSymbolInfo("cov_lock_base", dl)
        ulb <- base::getNativeSymbolInfo("cov_unlock_base", dl)
        symbols <- symbols_
        nmarkers <- nmarkers_
        mycall <- base::.Call
        env <- if (mycall(ulb)) baseenv() else globalenv()
        for (i in base::seq_along(symbols)) {
          if (base::is.null(env[[symbols[i]]])) {
            base::assign(symbols[i], mycall(mc, nmarkers[i]), envir = env)
          }
        }
        output <- base::file.path(outdir_, paste0(Sys.getpid(), '.rda'))
        base::reg.finalizer(
          .GlobalEnv,
          function(e) {
            base::dir.create(
              dirname(output),
              showWarnings = FALSE,
              recursive = TRUE
            )
            base::save(
              list = base::ls(
                all.names = TRUE,
                pattern = "^[.]__cov_",
                envir = e
              ),
              file = output,
              compression_level = 0,
              envir = e
            )
          },
          onexit = TRUE
        )
      }),
      subs
    )
  )
}
