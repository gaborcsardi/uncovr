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

is_linux <- function() {
  tolower(Sys.info()[["sysname"]]) == "linux"
}

zero <- function(x) {
  as.character(ifelse(x == 0, "", x))
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

all_leading_dirs <- function(paths) {
  dirs <- character()
  newdirs <- dirname(paths)
  todo <- newdirs[newdirs != "."]
  dirs <- c(dirs, todo)
  while (length(todo)) {
    newdirs <- dirname(todo)
    todo <- newdirs[newdirs != "."]
    dirs <- c(dirs, todo)
  }
  c("", paste0(sort(unique(dirs)), "/"))
}

str_count <- function(x, chr) {
  stopifnot(nchar(chr) == 1)
  chr <- charToRaw(chr)
  map_int(x, function(xx) sum(charToRaw(xx) == chr))
}

dir_ind <- function(x) {
  x0 <- ifelse(endsWith(x, "/"), substr(x, 1, nchar(x) - 1), x)
  level <- str_count(x0, "/")
  paste0(strrep(" ", level), x)
}

path_order <- function(x) {
  x <- gsub("\\", "/", fixed = TRUE, x)
  dirs <- endsWith(x, "/")
  x[dirs] <- paste0(x[dirs], ".")
  cmps <- strsplit(x, "/", fixed = TRUE)
  maxlevel <- max(lengths(cmps))
  dirs <- vector("list", maxlevel)
  for (i in 1:maxlevel) {
    dirs[[i]] <- map_chr(cmps, "[", i)
  }
  for (i in seq_len(maxlevel - 1)) {
    dirs[[i]] <- paste0(ifelse(is.na(dirs[[i + 1]]), "", "/"), dirs[[i]])
  }
  do.call(order, c(dirs, list(na.last = FALSE)))
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

is_positron <- function() {
  Sys.getenv("POSITRON") != ""
}

is_vscode <- function() {
  Sys.getenv("TERM_PROGRAM") == "vscode"
}

not_null <- function(x) {
  x[!map_lgl(x, is.null)]
}

na_omit <- function(x) {
  x[!is.na(x)]
}

is_ci <- function() {
  Sys.getenv("CI") == "true"
}

paste_named <- function(orig, new) {
  for (nm in names(new)) {
    orig[[nm]] <- if (nm %in% names(orig)) {
      paste(orig[[nm]], new[[nm]])
    } else {
      new[[nm]]
    }
  }
  orig
}

# relative path from project directory
rel_path <- function(x, root) {
  wd <- paste0(getwd(), "/")
  x <- ifelse(startsWith(x, wd), substr(x, nchar(wd) + 1, nchar(x)), x)
  x <- ifelse(startsWith(x, root), substr(x, nchar(root) + 1, nchar(x)), x)
  x
}

quick_save_rds <- function(obj, path) {
  ser <- serialize(obj, NULL, xdr = FALSE)
  writeBin(ser, path)
}

empty_data_frame <- function(nrow) {
  res <- data.frame(row.names = seq_len(nrow))
  rownames(res) <- NULL
  class(res) <- c("tbl", class(res))
  res
}
