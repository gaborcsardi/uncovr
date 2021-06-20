
map_chr <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

mkdirp <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

context_name <- function(filename) {
  # Remove test- prefix
  filename <- sub("^test[-_]", "", filename)
  # Remove terminal extension
  filename <- sub("[.][Rr]$", "", filename)
  filename
}

`%||%` <- function(l, r) if (is.null(l)) r else l

is_interactive <- function() {
  opt <- getOption("rlib_interactive")
  opt2 <- getOption("rlang_interactive")
  if (isTRUE(opt)) {
    TRUE
  } else if (identical(opt, FALSE)) {
    FALSE
  } else if (isTRUE(opt2)) {
    TRUE
  } else if (identical(opt2, FALSE)) {
    FALSE
  } else if (tolower(getOption("knitr.in.progress", "false")) == "true") {
    FALSE
  } else if (identical(Sys.getenv("TESTTHAT"), "true")) {
    FALSE
  } else {
    base::interactive()
  }
}

get_test_file_call <- function(test_file) {
  calls <- sys.calls()
  fns <- map_chr(calls, function(x) c(getSrcFilename(x), NA_character_)[1])
  wch <- rev(which(!is.na(fns) & basename(fns) == basename(test_file)))[1]
  if (is.na(wch)) NULL else calls[[wch]]
}

get_test_file_position <- function(test_file) {
  cll <- get_test_file_call(test_file)
  getSrcLocation(cll) %||% NA_integer_
}

strpad <- function(x, width = cli::console_width(), chr = " ",
                   align = c("left", "right")) {
  align <- match.arg(align)
  n <- pmax(0, width - crayon::col_nchar(x))
  spc <- strrep(chr, n)
  paste0(
    if (align == "right") spc,
    x,
    if (align == "left") spc
  )
}

base_packages <- function() {
  c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tcltk",
    "tools", "utils"
  )
}

lapply_with_names <- function(X, FUN, ...) {
  structure(lapply(X, FUN, ...), names = X)
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = logical(1), ...)
}

viapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = integer(1), ...)
}

last_char <- function(x) {
  l <- nchar(x)
  substr(x, l, l)
}
