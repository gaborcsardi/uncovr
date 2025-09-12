map_chr <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

map_int <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}

map_dbl <- function(X, FUN, ...) {
  vapply(X, FUN, double(1), ...)
}

map_lgl <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
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

`%||%` <- function(l, r) {
  if (is.null(l)) {
    r
  } else {
    l
  }
}

`%&&%` <- function(l, r) {
  if (is.null(l)) {
    NULL
  } else {
    r
  }
}

get_test_file_call <- function(test_file) {
  calls <- sys.calls()
  fns <- map_chr(calls, function(x) {
    c(utils::getSrcFilename(x), NA_character_)[1]
  })
  wch <- rev(which(!is.na(fns) & basename(fns) == basename(test_file)))[1]
  if (is.na(wch)) NULL else calls[[wch]]
}

get_test_file_position <- function(test_file) {
  cll <- get_test_file_call(test_file)
  utils::getSrcLocation(cll) %||% NA_integer_
}

strpad <- function(
  x,
  width = cli::console_width(),
  chr = " ",
  align = c("left", "right")
) {
  align <- match.arg(align)
  n <- pmax(0, width - cli::ansi_nchar(x, type = "width"))
  spc <- strrep(chr, n)
  paste0(
    if (align == "right") spc,
    x,
    if (align == "left") spc
  )
}

# key is ordered

find_zero_ranges <- function(key, value) {
  out <- list()
  start <- end <- NA_integer_
  for (i in seq_along(key)) {
    if (value[i] == 0 && is.na(start)) {
      start <- end <- key[i]
    } else if (value[i] == 0 && !is.na(start)) {
      end <- key[i]
    } else if (value[i] != 0 && !is.na(start)) {
      out[[length(out) + 1]] <- start:end
      start <- end <- NA_integer_
    } else if (value[i] != 0 && is.na(start)) {
      # Nothing to do
    }
  }

  if (!is.na(start)) {
    out[[length(out) + 1]] <- start:end
  }

  out
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}

zero <- function(x) {
  ifelse(x == 0, "", x)
}

order_df <- function(df, col) {
  df[order(df[[col]]), ]
}

summary_line <- function(n_ok, n_fail, n_warn, n_skip) {
  pass <- if (n_ok) format_type("pass") else "PASS"
  fail <- if (n_fail) format_type("error") else "FAIL"
  warn <- if (n_warn) format_type("warning") else "WARN"
  skip <- if (n_skip) format_type("skip") else "SKIP"

  passn <- paste0(pass, " x", n_ok)
  failn <- paste0(fail, " x", n_fail)
  warnn <- paste0(warn, " x", n_warn)
  skipn <- paste0(skip, " x", n_skip)

  paste(
    sep = "  ",
    if (n_ok) passn else cli::col_grey(passn),
    if (n_fail) failn else cli::col_grey(failn),
    if (n_warn) warnn else cli::col_grey(warnn),
    if (n_skip) skipn else cli::col_grey(skipn)
  )
}

styles <- new.env(parent = emptyenv())

style_orange <- function(...) {
  if (is.null(styles$orange)) {
    styles$orange <- cli::make_ansi_style("orange")
  }
  styles$orange(...)
}

style_bg_blue <- function(...) {
  if (is.null(styles$bg_blue)) {
    styles$bg_blue <- cli::make_ansi_style("#0000ff", bg = TRUE)
  }
  styles$bg_blue(...)
}

style_bg_orange <- function(...) {
  if (is.null(styles$bg_orange)) {
    styles$bg_orange <- cli::make_ansi_style("orange", bg = TRUE)
  }
  styles$bg_orange(...)
}

style_bg_green <- function(...) {
  if (is.null(styles$bg_green)) {
    styles$bg_green <- cli::make_ansi_style("darkgreen", bg = TRUE)
  }
  styles$bg_green(...)
}

style_bg_grey <- function(...) {
  if (is.null(styles$bg_grey)) {
    styles$bg_grey <- cli::make_ansi_style("#404040", bg = TRUE)
  }
  styles$bg_grey(...)
}

format_type <- function(type) {
  switch(
    type,
    pass = style_bg_green(cli::col_white("PASS")),
    success = style_bg_green(cli::col_white("PASS")),
    skip = style_bg_blue(cli::col_white("SKIP")),
    error = cli::bg_red(cli::col_white("FAIL")),
    failure = cli::bg_red(cli::col_white("FAIL")),
    warning = style_bg_orange(cli::col_white("WARN"))
  )
}
