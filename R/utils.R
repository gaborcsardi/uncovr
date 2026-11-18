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

styles <- new.env(parent = emptyenv())

style_orange <- function(...) {
  if (is.null(styles$orange)) {
    styles$orange <- cli::make_ansi_style("orange")
  }
  styles$orange(...)
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

not_null <- function(x) {
  x[!map_lgl(x, is.null)]
}

na_omit <- function(x) {
  x[!is.na(x)]
}

is_ci <- function() {
  Sys.getenv("CI") == "true"
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

write_lines_safe <- function(lines, path) {
  bak <- paste0(path, "~")
  on.exit(unlink(bak), add = TRUE)
  writeLines(lines, bak)
  file.rename(bak, path)
}
