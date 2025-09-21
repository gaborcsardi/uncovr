is_dir <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}

is_link <- function(x) {
  rl <- Sys.readlink(x)
  !is.na(rl) & rl != ""
}

dir_size <- function(dirs) {
  map_dbl(dirs, function(dir) {
    paths <- list.files(dir, recursive = TRUE, full.names = TRUE)
    paths <- paths[!is_link(paths)]
    sum(file.size(paths))
  })
}
