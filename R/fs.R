is_dir <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}

is_link <- function(x) {
  rl <- Sys.readlink(x)
  !is.na(rl) & rl != ""
}

clone_file <- function(from, target) {
  .Call(c_cov_clone_file, from, target)
}

# Create `target` from the file `from`. We try, in this order:
# 1. a copy-on-write clone, if the file system supports it (APFS on macOS,
#    Btrfs or XFS on Linux, ReFS on Windows),
# 2. a hard link (e.g. ext4 on Linux, NTFS on Windows),
# 3. a regular copy.
# Because of the hard links, files in the build tree must not be modified
# in place, as that would modify the source file as well. Use
# `write_lines_safe()` instead, which replaces the file.
# Returns `"clone"`, `"link"` or `"copy"`.
clone_link_or_copy <- function(from, target) {
  mkdirp(dirname(target))
  if (clone_file(from, target)) {
    return(invisible("clone"))
  }
  if (isTRUE(suppressWarnings(file.link(from, target)))) {
    return(invisible("link"))
  }
  file.copy(from, target)
  invisible("copy")
}

dir_size <- function(dirs) {
  map_dbl(dirs, function(dir) {
    paths <- list.files(dir, recursive = TRUE, full.names = TRUE)
    paths <- paths[!is_link(paths)]
    sum(file.size(paths))
  })
}
