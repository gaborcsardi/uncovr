# Only R code coverage is supported currently, so compiled code is built
# with debug flags, without any coverage instrumentation.
get_makeflags <- function() {
  pkgbuild::compiler_flags(debug = TRUE)
}
