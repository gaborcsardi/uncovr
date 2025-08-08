opt_setup <- "testthatlabs.setup"

load_dev <- function(path = ".", coverage = FALSE) {
  withr::local_dir(path)
  # setup build options
  setup <- list(
    version = 1L,
    rver = as.character(getRversion()[, 1:2]),
    platform = R.Version()$platform,
    coverage = coverage
  )
  setup[["hash"]] = cli::hash_obj_sha1(setup)
  withr::local_options(structure(list(setup), names = opt_setup))

  dir <- get_dev_dir()
  wd <- basename(getwd())

  copy <- c("src", if (setup$coverage) "R")
  update_package_tree(".", dir, copy = copy)

  if (coverage) {
    cov_data <- cov_instrument_dir(file.path(dir, wd, "R"))
    setup_cov_env(cov_data)
  }

  pkgload::load_all(file.path(dir, wd))
}

get_dev_dir <- function() {
  setup <- getOption(opt_setup)
  if (is.null(setup)) {
    stop("No dev build setup. :(")
  }
  file.path(".dev", setup$hash)
}

update_package_tree <- function(src, dst, copy = character()) {
  if (!file.exists(dst)) {
    pkgbuild:::copy_package_tree(src, dst)
  }
  wd <- basename(getwd())
  for (copy1 in copy) {
    src1 <- file.path(src, copy1)
    if (!file.exists(src1)) {
      next
    }
    dst1 <- file.path(dst, wd, copy1)
    if (is_link(dst1)) {
      unlink(dst1, recursive = TRUE)
    }
    message(src1, " -> ", dst1)
    if (is_dir(src1)) {
      fs::dir_copy(src1, dst1, overwrite = TRUE)
    } else {
      fs::file_copy(src1, dst1, overwrite = TRUE)
    }
  }
}

is_dir <- function(x) {
  file.info(x)$isdir
}

is_link <- function(x) {
  !is.na(Sys.readlink(x))
}

cov_instrument_dir <- function(path = "R") {
  withr::local_dir(path)
  rfiles <- dir(pattern = "[.][rR]$")
  res <- data.frame(
    path = rfiles,
    symbol = paste0(".__cov_", tools::file_path_sans_ext(basename(rfiles))),
    line_count = NA_integer_
  )
  for (i in seq_along(rfiles)) {
    res$line_count[i] <- cov_instrument_file(res$path[i], res$symbol[i])
  }
  res
}

cov_instrument_file <- function(path, cov_symbol) {
  ps <- parse(path, keep.source = TRUE)
  psd <- getParseData(ps)
  brc_poss <- which(psd$token == "'{'")
  inj_posl <- lapply(brc_poss, get_inject_positions, psd)
  inj <- data.frame(
    line1 = unlist(lapply(inj_posl, "[[", "line1")),
    col1 = unlist(lapply(inj_posl, "[[", "col1"))
  )
  # work around files without '{'
  if (nrow(inj)) {
    inj$code <- paste0("`", cov_symbol, "`[", inj$line1, "]; ")
  } else {
    inj$code <- character()
  }
  # need to insert code concurrently to the same line
  lns <- readLines(path)
  inj_lines <- unique(inj$line1)
  for (il in inj_lines) {
    inj1 <- inj[inj$line1 == il, ]
    lns[il] <- str_insert_parallel(lns[il], inj1$col1, inj1$code)
  }
  writeLines(lns, path)
  length(lns)
}

get_inject_positions <- function(brc_pos, psd) {
  parent_id <- psd$parent[brc_pos]
  expr_pos <- which(psd$parent == parent_id)
  # drop comments, no need to instrument them and cause issues
  expr_pos <- expr_pos[psd$token[expr_pos] != "COMMENT"]
  # first one is '{', last one is '}', drop them
  expr_pos <- expr_pos[c(-1, -length(expr_pos))]
  psd[expr_pos, c("line1", "col1")]
}

str_insert_parallel <- function(str, pos, insert) {
  text_starts <- c(1, pos)
  text_ends <- c(pos - 1, nchar(str))

  text_pieces <- substring(str, text_starts, text_ends)

  res <- character(length(text_pieces) + length(insert))
  res[myseq(1, length(res), by = 2)] <- text_pieces
  res[myseq(2, length(res) - 1, by = 2)] <- insert

  paste(res, collapse = "")
}

myseq <- function(from, to, by = 1) {
  stopifnot(by != 0)
  if (by > 0) {
    if (to < from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  } else {
    if (to > from) {
      integer()
    } else {
      seq(from, to, by = by)
    }
  }
}

setup_cov_env <- function(cov_data) {
  # while ("tools:cov" %in% search()) {
  #   detach("tools:cov")
  # }
  # cov_env <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(cov_data))) {
    assign(
      cov_data$symbol[i],
      rep(NA_integer_, cov_data$line_count[i]),
      envir = .GlobalEnv
    )
  }
  # attach(cov_env, name = "tools:cov")
}
