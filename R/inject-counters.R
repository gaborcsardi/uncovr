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

cov_get_counts <- function(names) {
  env <- if (length(names) > 1 && exists(names[1], envir = baseenv())) {
    baseenv()
  } else {
    globalenv()
  }
  structure(
    lapply(names, function(name) {
      .Call(c_cov_get_counts, get(name, envir = env))
    }),
    names = names
  )
}
