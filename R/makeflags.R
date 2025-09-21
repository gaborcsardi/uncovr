get_makeflags <- function(type = c("debug", "release", "coverage")) {
  type <- match.arg(type)
  if (type == "debug") {
    pkgbuild::compiler_flags(debug = TRUE)
  } else if (type == "release") {
    pkgbuild::compiler_flags(debug = FALSE)
  } else if (type == "coverage") {
    paste_named(pkgbuild::compiler_flags(debug = TRUE), covr_flags())
  }
}

covr_flags <- function() {
  c(
    CFLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXXFLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXX1XFLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXX11FLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXX14FLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXX17FLAGS = "-O0 --coverage -DGCOV_COMPILE",
    CXX20FLAGS = "-O0 --coverage -DGCOV_COMPILE",

    FFLAGS = "-O0 --coverage",
    FCFLAGS = "-O0 --coverage",
    FLIBS = "-lgcov",

    # LDFLAGS is ignored on windows and visa versa
    LDFLAGS = if (!is_windows()) {
      if (is_linux()) {
        "--coverage -Wl,--whole-archive -lgcov -Wl,--no-whole-archive"
      } else {
        "--coverage"
      }
    } else {
      NULL
    },
    SHLIB_LIBADD = if (is_windows()) {
      "--coverage"
    } else {
      NULL
    }
  )
}
