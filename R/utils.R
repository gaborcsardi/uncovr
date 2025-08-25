map_chr <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

map_int <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}

map_dbl <- function(X, FUN, ...) {
  vapply(X, FUN, double(1), ...)
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
{
}
