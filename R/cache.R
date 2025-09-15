inst_cache_file <- function(path, hash) {
  path <- gsub("/", "-", path)
  file.path("..", cov_dir_name, path, paste0(hash, ".rds"))
}

# the cached files might be large, so only cache them for an hour, but
# always keep the current one
get_cached_file <- function(path, hash) {
  fn <- inst_cache_file(path, hash)
  oth <- setdiff(dir(dirname(fn), full.names = TRUE), fn)
  todel <- oth[Sys.time() - file.mtime(oth) > as.difftime(1, units = "hours")]
  if (length(todel)) {
    unlink(todel)
  }
  if (file.exists(fn)) {
    val <- NULL
    tryCatch(val <- readRDS(fn), error = function(e) unlink(fn))
    val
  }
}

set_cached_file <- function(path, hash, value) {
  fn <- inst_cache_file(path, hash)
  mkdirp(dirname(fn))
  quick_save_rds(value, fn)
}
