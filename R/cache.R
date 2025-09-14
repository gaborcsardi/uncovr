inst_cache_file <- function(hash) {
  file.path("..", cov_dir_name, paste0("inst-", hash, ".rds"))
}

get_cached_file <- function(hash) {
  fn <- inst_cache_file(hash)
  if (file.exists(fn)) {
    val <- NULL
    tryCatch(val <- readRDS(fn), error = function(e) unlink(fn))
    val
  }
}

set_cached_file <- function(hash, value) {
  fn <- inst_cache_file(hash)
  mkdirp(dirname(fn))
  quick_save_rds(value, fn)
}
