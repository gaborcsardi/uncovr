cov_instrument_dir <- function(
  path = "R",
  pkgname = desc::desc_get("Package", dirname(path)),
  exclusion_file = ".covrignore"
) {
  cli::cli_alert_info("Instrumenting {pkgname}")
  rfiles <- apply_covrignore(
    dir(path, pattern = "[.][rR]$", full.names = TRUE),
    exclusion_file
  )
  symbol <- paste0(
    ".__cov_",
    pkgname,
    "_",
    tools::file_path_sans_ext(basename(rfiles))
  )
  fls <- data.frame(
    path = rfiles,
    symbol = symbol,
    num_markers = 0L,
    line_count = NA_integer_,
    code_lines = NA_integer_,
    lines_covered = 0L,
    total_hits = 0,
    percent_covered = 0,
    function_count = 0L,
    functions_hit = 0L,
    lines = I(replicate(length(rfiles), NULL, simplify = FALSE)),
    funs = I(replicate(length(rfiles), NULL, simplify = FALSE)),
    uncovered = I(replicate(length(rfiles), list(), simplify = FALSE))
  )
  class(fls) <- c("tbl", class(fls))

  parsed <- structure(vector("list", nrow(fls)), names = fls$path)
  for (i in seq_along(rfiles)) {
    cifile <- cov_instrument_file(fls$path[i], fls$symbol[i])
    parsed[[i]] <- cifile$parsed
    fls$lines[[i]] <- cifile$lines
    fls$line_count[i] <- nrow(fls$lines[[i]])
    fls$code_lines[i] <- sum(fls$lines[[i]]$status == "instrumented")
    fls$funs[[i]] <- cifile$funs
    fls$function_count[i] <- nrow(cifile$funs)
    fls$num_markers[i] <- nrow(cifile$lines) + nrow(cifile$funs)
  }

  list(files = fls, parsed = parsed)
}

cov_instrument_file <- function(path, cov_symbol) {
  hash <- cli::hash_file_xxhash(path)
  cached <- get_cached_file(path, hash)
  if (!is.null(cached)) {
    base::writeLines(cached$code, path)
    return(cached)
  }
  ps <- parse(path, keep.source = TRUE)
  psd <- utils::getParseData(ps)
  brc_poss <- which(psd$token == "'{'")

  # drop rlang's {{ embrace operator up front
  drop <- which(diff(psd$line1[brc_poss]) == 0 & diff(psd$col1[brc_poss]) == 1)
  drop <- c(drop, drop + 1L)
  if (length(drop)) {
    brc_poss <- brc_poss[-drop]
  }

  # calculcate insertion position for each subexpression of every '{'
  # plus prepend the top level exprssions
  toplevel <- psd[
    which(psd$parent == 0 & psd$token != "COMMENT"),
    c("line1", "col1", "line2", "col2")
  ]
  inj_posl <- c(list(toplevel), lapply(brc_poss, get_inject_positions, psd))
  injx <- data.frame(
    line1 = unlist(lapply(inj_posl, "[[", "line1")),
    col1 = unlist(lapply(inj_posl, "[[", "col1")),
    line2 = unlist(lapply(inj_posl, "[[", "line2")),
    col2 = unlist(lapply(inj_posl, "[[", "col2"))
  )
  # work around files without '{'
  if (nrow(injx)) {
    injx$code <- paste0("`", cov_symbol, "`[", injx$line1, "]; ")
  } else {
    injx$code <- character()
  }

  # Drop duplicate lines, only the first expression is counted.
  # TODO: improve this and count every expression individually
  inj <- injx[!duplicated(injx$line1), , drop = FALSE]
  # we note this before adding the function coverage markers, because
  # those typically go into non-instrumented lines
  instrumented_lines <- inj$line1

  lns0 <- lns <- untabify(readLines(path))

  # now do the functions as well
  # TODO: \()? Or maybe not?
  fun_poss <- which(psd$token == "FUNCTION")
  par_poss <- match(psd$parent[fun_poss], psd$id)
  # need an opening brace, whose grandparent's position is par_poss
  par_brc_poss <- match(psd$parent[brc_poss], psd$id)
  ppar_brc_poss <- match(psd$parent[par_brc_poss], psd$id)
  obr_poss <- brc_poss[match(par_poss, ppar_brc_poss)]
  bad <- is.na(obr_poss)
  fun_poss <- fun_poss[!bad]
  par_poss <- par_poss[!bad]
  obr_poss <- obr_poss[!bad]
  funres <- data.frame(
    name = rep(NA_character_, length(fun_poss)),
    aliases = I(replicate(length(fun_poss), NULL, simplify = FALSE)),
    line1 = psd$line1[fun_poss],
    col1 = psd$col1[fun_poss],
    line2 = psd$line2[par_poss],
    col2 = psd$col2[par_poss],
    brace_line = psd$line1[obr_poss],
    brace_col = psd$col1[obr_poss],
    status = rep("instrumented", length(fun_poss)),
    id = seq_along(fun_poss) + length(lns0),
    coverage = rep(NA_integer_, length(fun_poss))
  )

  if (nrow(funres)) {
    injf <- data.frame(
      line1 = funres$brace_line,
      col1 = funres$brace_col + 1L,
      line2 = funres$brace_line,
      col2 = funres$brace_col + 1L,
      code = paste0("`", cov_symbol, "`[", funres$id, "]; ")
    )
    inj <- rbind(inj, injf)
  }

  # need to insert code concurrently to the same line, for future
  # multi-expression per line support
  injs <- split(inj, inj$line1, drop = TRUE)
  ils <- as.integer(names(injs))
  for (i in seq_along(injs)) {
    il <- ils[i]
    lns[il] <- str_insert_parallel(lns[il], injs[[i]]$col1, injs[[i]]$code)
  }
  base::writeLines(lns, path)

  res <- data.frame(
    lines = lns0,
    status = "noncode",
    id = NA_integer_,
    coverage = NA_integer_
  )

  # lines where there is no terminal node are definitely not code
  # apart from long strings, which are code
  notterm <- which(psd$text != "")
  res$status[unique(c(psd$line1[notterm], psd$line2[notterm]))] <- NA_character_
  strconst <- which(psd$token == "STR_CONST")
  for (str in strconst) {
    res$status[psd$line1[str]:psd$line2[str]] <- NA_character_
  }

  res$status[instrumented_lines] <- "instrumented"
  res$id[instrumented_lines] <- instrumented_lines

  # Handle multi-line expressions. We need to do this backwards, so nested
  # braces work out correctly.
  for (i in rev(seq_len(nrow(inj)))) {
    li <- inj$line1[i]:inj$line2[i]
    if (length(li) == 1) {
      next
    }
    cnt <- li[is.na(res$status[li])]
    res$status[cnt] <- "instrumented"
    res$id[cnt] <- inj$line1[i]
  }
  comment_lines <- setdiff(
    psd$line1[psd$token == "COMMENT"],
    psd$line1[psd$token != "COMMENT"]
  )
  res$status[comment_lines] <- "noncode"
  res$id[comment_lines] <- NA_integer_
  res$coverage[res$status == "instrumented"] <- 0L
  res$status[is.na(res$status)] <- "noncode"

  # we add line exclusions here, so it is theoretically possible
  # to only exclude parts of a multi-line expression
  drop <- parse_line_exclusions(lns0, path)
  if (length(drop)) {
    res$status[drop] <- "excluded"
    res$id[drop] <- NA_integer_
    res$coverage[drop] <- NA_integer_
  }

  # drop functions that are completely excluded
  firstlineex <- which(res$status[funres$line1] == "excluded")
  for (fex in firstlineex) {
    l1 <- funres$line1[fex]
    l2 <- funres$line2[fex]
    if (!any(res$status[l1:l2] == "instrumented")) {
      funres$status[fex] <- "excluded"
    }
  }
  if (any(funres$status == "excluded")) {
    funres <- funres[funres$status != "excluded", ]
    funres$id <- seq_len(nrow(funres)) + length(lns0)
  }

  res <- list(lines = res, funs = funres, parsed = ps, code = lns)
  set_cached_file(path, hash, res)
  res
}

# `getParseData()` gives te wrong coordinates for lines with TAB characters,
# it counts each TAB as 8 spaces, so we need to circumvent that.
# But prepare for this to be fixed at some point.

untabify <- local({
  opt <- options(
    keep.parse.data = TRUE,
    keep.parse.data.pkgs = TRUE,
    keep.source = TRUE,
    keep.source.pkgs = TRUE
  )
  on.exit(options(opt), add = TRUE)
  badtab <- getParseData(parse(text = "\tx", keep.source = TRUE))$col1[1] == 9
  function(x) {
    if (badtab) {
      x <- gsub("\t", strrep(" ", 8), fixed = TRUE, x)
    }
    x
  }
})

get_inject_positions <- function(brc_pos, psd) {
  parent_id <- psd$parent[brc_pos]
  expr_pos <- which(psd$parent == parent_id)
  # drop comments, no need to instrument them and cause issues
  expr_pos <- expr_pos[psd$token[expr_pos] != "COMMENT"]
  # first one is '{', last one is '}', drop them
  expr_pos <- expr_pos[c(-1, -length(expr_pos))]
  psd[expr_pos, c("line1", "col1", "line2", "col2")]
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
