
make_ns_info <- function(pkg, lib) {
  nsinfo <- parseNamespaceFile(basename(getwd()), dirname(getwd()))
  nsinfopath <- file.path(lib, pkg, "Meta", "nsInfo.rds")
  saveRDS(nsinfo, nsinfopath)
}

make_lazy_load_db <- function(lib, pkg, ns) {
  loader <- file.path(R.home("share"), "R", "nspackloader.R")
  code <- file.path(lib, pkg, "R", pkg)
  mkdirp(dirname(code))
  tools:::makeLazyLoadDB(
    ns,
    code,
    compress = FALSE,
    set.install.dir = NULL
  )
  file.copy(loader, code, overwrite = TRUE)
}

add_covr_save <- function(trace_dir, loader) {
  unlink(trace_dir, recursive = TRUE)
  mkdirp(trace_dir)
  trace_str <- encodeString(trace_dir, quote = "\"")
  lines <- readLines(loader)
  lines <- append(
    lines,
    paste0("reg.finalizer(ns, function(...) { covr:::save_trace(", trace_str, ") }, onexit = TRUE)"),
    length(lines) - 1L
  )
  writeLines(lines, loader)
}

collate_r_files <- function(pkg_dir, output, collate = NULL) {
  r_dir <- file.path(pkg_dir, "R")
  r_files <- withr::with_collate(
    "C",
    tools::list_files_with_type(r_dir,  "code", full.names = TRUE)
  )
  collate <- collate %||% desc::desc_get_collate(file = pkg_dir)
  if (length(collate) > 0) {
    collate <- file.path(r_dir, collate)
    missing <- setdiff(collate, r_files)
    if (length(missing) > 0) {
      message("Skipping missing files: {.file {missing}}")
    }
    collate <- setdiff(collate, missing)
    extra <- setdiff(r_files, collate)
    if (length(extra) > 0) {
      message("Adding files missing in collate: {.file {extra}}")
    }
    r_files <- union(collate, r_files)
  }

  mkdirp(dirname(output))
  con <- file(output, open = "wb")
  on.exit(close(con), add = TRUE)
  for (rf in r_files) {
    cnts <- readBin(rf, "raw", file.size(rf))
    writeLines(paste0("#line 1 \"", rf, "\""), con)
    writeBin(cnts, con)
    writeLines("", con)
  }
}

install_sysdata <- function(input, output) {
  if (!file.exists(input)) return()
  env <- new.env(hash = TRUE)
  load(input, env)
  tools:::makeLazyLoadDB(env, output, compress = FALSE)
}
