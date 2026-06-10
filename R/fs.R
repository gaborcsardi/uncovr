is_dir <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}

is_link <- function(x) {
  rl <- Sys.readlink(x)
  !is.na(rl) & rl != ""
}

fs_state <- new.env(parent = emptyenv())

# Whether this R session can create symlinks. Symlinking commonly fails on
# Windows, where it needs a privilege that is not held by default ("A
# required privilege is not held by the client"). This is a session-wide
# capability, so we probe it once (creating a symlink in a temporary dir)
# and cache the result.
can_symlink <- function() {
  if (!is.null(fs_state$can_symlink)) {
    return(fs_state$can_symlink)
  }
  src <- tempfile("uncovr-symlink-probe-")
  link <- tempfile("uncovr-symlink-probe-")
  mkdirp(src)
  on.exit(unlink(c(src, link), recursive = TRUE, force = TRUE), add = TRUE)
  ok <- tryCatch(
    isTRUE(suppressWarnings(file.symlink(src, link))) && is_link(link),
    error = function(e) FALSE
  )
  fs_state$can_symlink <- ok
  ok
}

# Try to create a symlink at `target` pointing to `from`, falling back to
# copying if this session cannot create symlinks (see `can_symlink()`).
link_or_copy <- function(from, target, isdir) {
  if (can_symlink()) {
    return(invisible(file.symlink(from, target)))
  }

  if (isdir) {
    mkdirp(target)
    contents <- list.files(from, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    file.copy(contents, target, recursive = TRUE)
  } else {
    mkdirp(dirname(target))
    file.copy(from, target)
  }
  invisible(FALSE)
}

dir_size <- function(dirs) {
  map_dbl(dirs, function(dir) {
    paths <- list.files(dir, recursive = TRUE, full.names = TRUE)
    paths <- paths[!is_link(paths)]
    sum(file.size(paths))
  })
}
