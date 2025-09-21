#' Run roxygen2 to generate the package manual and namespace files
#'
#' Loads the debug build, and calls [roxygen2::roxygenize()].
#'
#' @inheritParams reload
#' @return The same as [reload()], with an additional field `roxy`,
#'   that contains the return value of [roxygen2::roxygenize()].
#'
#' @export

document <- function(path = ".", clean = FALSE, local_install = TRUE) {
  withr::local_dir(path)
  dev_data <- reload(
    type = "debug",
    path = ".",
    clean = clean,
    local_install = local_install
  )

  dev_data$roxy <- roxygen2::roxygenize(load_code = function(path) {
    dev_data$load$env
  })

  invisible(dev_data)
}
