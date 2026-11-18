is_dir <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}

is_link <- function(x) {
  rl <- Sys.readlink(x)
  !is.na(rl) & rl != ""
}

# Create `target` from the file `from`. We try, in this order:
# 1. a hard link (e.g. ext4 on Linux, NTFS on Windows, APFS on macOS),
# 2. a regular copy.
# Because of the hard links, files in the build tree must not be modified
# in place, as that would modify the source file as well. Use
# `write_lines_safe()` instead, which replaces the file.
# Returns `"link"` or `"copy"`.
link_or_copy <- function(from, target) {
  mkdirp(dirname(target))
  if (isTRUE(suppressWarnings(file.link(from, target)))) {
    return(invisible("link"))
  }
  if (!file.copy(from, target)) {
    stop("Failed to copy '", from, "' to '", target, "'.")
  }
  invisible("copy")
}
