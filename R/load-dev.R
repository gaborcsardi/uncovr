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
  pkgname <- desc::desc_get("Package", ".")

  copy <- c("src", if (setup$coverage) "R")
  update_package_tree(".", dir, pkgname = pkgname, copy = copy)

  if (coverage) {
    cov_data <- cov_instrument_dir(file.path(dir, pkgname, "R"))
    setup_cov_env(cov_data)
  }

  pkgload::load_all(file.path(dir, pkgname))
}

get_dev_dir <- function() {
  setup <- getOption(opt_setup)
  if (is.null(setup)) {
    stop("No dev build setup. :(")
  }
  file.path(".dev", setup$hash)
}

# specific for us, it should be in .Rbuildignore but have it here as well
ignored_extra <- c("^[.]dev$")

exclude_build_ignored <- function(
  plan,
  src = ".",
  pkgname = desc::desc_get("Package", src)
) {
  if (nrow(plan) == 0) {
    return(plan)
  }
  ign_file <- file.path(src, ".Rbuildignore")
  ign <- if (file.exists(ign_file)) {
    readLines(ign_file, warn = FALSE)
  }
  ign <- c(ignored_extra, ign)
  ign <- ign[nzchar(ign)]
  # case insensitive!
  ign <- paste0("(?i)", ign)

  ptrn <- c(ign, re_exclude(pkgname))
  ptrn_dir <- re_exclude_dir(pkgname)

  plan$exclude <- FALSE
  plan <- exclude_paths(plan, ptrn)
  plan <- exclude_dirs(plan, ptrn_dir)
  plan <- exclude_downstream(plan)

  plan[!plan$exclude, ]
}

exclude_paths <- function(plan, ptrn) {
  for (p in ptrn) {
    plan$exclude <- plan$exclude | grepl(p, plan$path, perl = TRUE)
  }
  plan
}

exclude_dirs <- function(plan, ptrn) {
  for (p in ptrn) {
    plan$exclude <- plan$exclude |
      (plan$isdir & grepl(p, plan$path, perl = TRUE))
  }
  plan
}

exclude_downstream <- function(plan) {
  # We don't actually need this now, but we could use it to optimize,
  # because we could trim only the subsequent elements after a directory.
  plan <- plan[order(plan$path), ]

  # We need to take each excluded directory, and remove all paths in them
  exdirs <- paste0(plan$path[plan$isdir & plan$exclude], "/")
  del <- logical(nrow(plan))
  for (ed in exdirs) {
    del <- del | startsWith(plan$path, ed)
  }
  plan <- plan[!del, ]
  plan
}

create_copy_plan <- function(
  src = ".",
  pkgname = desc::desc_get("Package", src),
  copy = character()
) {
  withr::local_dir(src)

  topfiles <- dir(all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
  topplan <- data.frame(
    path = topfiles,
    isdir = is_dir(topfiles),
    action = ifelse(topfiles %in% copy, "copy", "link")
  )
  topplan <- exclude_build_ignored(topplan, src = ".", pkgname = pkgname)

  rest <- dir(
    topplan$path[topplan$isdir & topplan$action == "copy"],
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE
  )
  restplan <- data.frame(
    path = rest,
    isdir = is_dir(rest),
    action = rep("copy", length(rest))
  )
  restplan <- exclude_build_ignored(restplan, src = ".", pkgname = pkgname)

  rbind(topplan, restplan)
}

create_update_plan <- function(
  src,
  dst,
  pkgname = desc::desc_get("Package", src),
  copy = character()
) {
  plan <- create_copy_plan(src, pkgname, copy)
  plan$target <- file.path(dst, pkgname, plan$path)
  plan$hash <- NA_character_
  plan$hash[!plan$isdir] <- cli::hash_file_xxhash(plan$path[!plan$isdir])

  update <- list(
    delete = character(),
    add = character(),
    update = character()
  )
  plan_file <- file.path(dst, "plan.rds")
  if (file.exists(plan_file)) {
    oldplan <- readRDS(plan_file)
    # delete paths that are not in the new plan
    update$delete <- setdiff(oldplan$path, plan$path)
    # add paths that are new in the plan, or don't exist
    update$add <- unique(c(
      setdiff(plan$path, oldplan$path),
      plan$path[!file.exists(plan$target)]
    ))
    # check if we need to update files:
    # - dir -> file or file -> dir change
    # - action change
    # - hash of file change
    common_paths <- intersect(plan$path, oldplan$path)
    cold <- oldplan[match(common_paths, oldplan$path), ]
    cnew <- plan[match(common_paths, plan$path), ]
    acthash <- rep(NA_character_, length(common_paths))
    tohash <- !cnew$isdir & !is_link(cnew$target)
    acthash[tohash] <- cli::hash_file_xxhash(cnew$target[tohash])
    update$update <- common_paths[
      cold$isdir != cnew$isdir |
        cold$action != cnew$action |
        (cnew$action == "copy" &
          !cold$isdir &
          !cnew$isdir &
          !is_link(cnew$target) &
          cnew$hash != acthash)
    ]
  } else {
    update[["add"]] <- plan$path
  }

  list(plan = plan, update = update)
}

update_package_tree <- function(
  src,
  dst,
  pkgname = desc::desc_get("Package", src),
  copy = character()
) {
  withr::local_dir(src)

  plan <- create_update_plan(src, dst, pkgname, copy)
  upd <- plan$update
  plan <- plan$plan

  # make sure that target dir exists
  mkdirp(file.path(dst, pkgname))

  todel <- file.path(dst, pkgname, c(upd$delete, upd$update))
  unlink(todel, force = TRUE, recursive = TRUE)
  planadd <- plan[plan$path %in% c(upd$add, upd$update), ]

  # TODO: make relative symlinks, no absolute
  wd <- getwd()
  for (i in seq_len(nrow(planadd))) {
    path <- planadd$path[i]
    action <- planadd$action[i]
    target <- planadd$target[i]
    isdir <- planadd$isdir[i]
    if (action == "link") {
      file.symlink(file.path(wd, path), target)
    } else if (isdir) {
      # files are copied later
      mkdirp(target)
    } else {
      mkdirp(dirname(target))
      file.copy(path, target)
    }
  }

  plan_file <- file.path(dst, "plan.rds")
  saveRDS(plan, plan_file)

  invisible(plan)
}

is_dir <- function(x) {
  file.info(x, extra_cols = FALSE)$isdir
}

is_link <- function(x) {
  rl <- Sys.readlink(x)
  !is.na(rl) & rl != ""
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


re_exclude <- function(pkg) {
  c(
    paste0(
      "(?i)", # these are case insensitive
      c(
        "(^|/)\\.DS_Store$", # by macOS finder
        "^\\.RData$", # .RData at /
        "~$",
        "\\.bak$",
        "\\.swp$", # backup files
        "(^|/)\\.#[^/]*$",
        "(^|/)#[^/]*#$", # more backup files (Emacs)

        "^config\\.(cache|log|status)$", # leftover by autoconf
        "(^|/)autom4te\\.cache$",

        "^src/.*\\.d$",
        "^src/Makedeps$", # INSTALL leftover on Windows

        "^inst/doc/Rplots\\.(ps|pdf)$" # Sweave leftover
      )
    ),

    "(^|/)\\._[^/]*$", # macOS resource forks

    paste0(
      # hidden files
      "(^|/)\\.",
      c(
        "Renviron",
        "Rprofile",
        "Rproj.user",
        "Rhistory",
        "Rapp.history",
        "tex",
        "log",
        "aux",
        "pdf",
        "png",
        "backups",
        "cvsignore",
        "cproject",
        "directory",
        "dropbox",
        "exrc",
        "gdb.history",
        "gitattributes",
        "github",
        "gitignore",
        "gitmodules",
        "hgignore",
        "hgtags",
        "htaccess",
        "latex2html-init",
        "project",
        "seed",
        "settings",
        "tm_properties"
      ),
      "$"
    ),

    paste0(
      "(^|/)",
      pkg,
      "_[0-9.-]+",
      "\\.(tar\\.gz|tar|tar\\.bz2|tar\\.xz|tgz|zip)",
      "$"
    )
  )
}

re_exclude_dir <- function(pkg) {
  c(
    "^revdep$", # revdepcheck
    paste0(
      # VC
      "(^|/)",
      c(
        "CVS",
        ".svn",
        ".arch-ids",
        ".bzr",
        ".git",
        ".hg",
        "_darcs",
        ".metadata"
      ),
      "$"
    ),

    "(^|/)[^/]*[Oo]ld$",
    "(^|/)[^/]*\\.Rcheck",

    "^src.*/\\.deps$"
  )
}
