#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib uncovr, .registration = TRUE, .fixes = "c_"
## usethis namespace: end
NULL

# to work around not-imported-from R CMD check warning
dummy <- function() {
  R6::R6Class
  prettyunits::pretty_dt
  tools::file_path_sans_ext
}
