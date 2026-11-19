#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib uncovr, .registration = TRUE, .fixes = "c_"
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  routines$make <- c_cov_make_counter
  routines$get <- c_cov_get_counts
}
