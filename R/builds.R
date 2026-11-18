create_copy_plan <- function(
  src = ".",
  pkgname = desc::desc_get("Package", src)
) {
  withr::local_dir(src)

  topfiles <- dir(all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
  topplan <- data.frame(
    path = topfiles,
    isdir = is_dir(topfiles)
  )
  topplan <- exclude_build_ignored(topplan, src = ".", pkgname = pkgname)

  rest <- dir(
    topplan$path[topplan$isdir],
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE,
    include.dirs = TRUE
  )
  restplan <- data.frame(
    path = rest,
    isdir = is_dir(rest)
  )
  restplan <- exclude_build_ignored(restplan, src = ".", pkgname = pkgname)

  res <- rbind(topplan, restplan)
  class(res) <- c("tbl", class(res))
  res
}

create_update_plan <- function(
  src,
  dst,
  pkgname = desc::desc_get("Package", src)
) {
  plan <- create_copy_plan(src, pkgname)
  plan$target <- file.path(dst, pkgname, plan$path)
  plan$hash <- NA_character_
  plan$hash[!plan$isdir] <- cli::hash_file_xxhash(plan$path[!plan$isdir])

  update <- list(
    delete = character(),
    add = character(),
    update = character()
  )
  plan_file <- file.path(dst, plan_file_name)
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
    # - target is a symlink, older versions symlinked files and dirs
    # - hash of file change
    common_paths <- intersect(plan$path, oldplan$path)
    cold <- oldplan[match(common_paths, oldplan$path), ]
    cnew <- plan[match(common_paths, plan$path), ]
    islink <- is_link(cnew$target)
    acthash <- rep(NA_character_, length(common_paths))
    tohash <- !cnew$isdir & !islink & file.exists(cnew$target)
    acthash[tohash] <- cli::hash_file_xxhash(cnew$target[tohash])
    update$update <- common_paths[
      cold$isdir != cnew$isdir |
        islink |
        (tohash & cnew$hash != acthash)
    ]
  } else {
    update[["add"]] <- plan$path
  }

  list(plan = plan, update = update)
}

update_package_tree <- function(
  src,
  dst,
  pkgname = desc::desc_get("Package", src)
) {
  withr::local_dir(src)

  plan <- create_update_plan(src, dst, pkgname)
  upd <- plan$update
  plan <- plan$plan

  # make sure that target dir exists
  mkdirp(file.path(dst, pkgname))

  # New paths might already exist in the build tree, e.g. if there is no
  # old plan file, or if the build created them. Remove these, except for
  # existing directories, these might contain build artifacts.
  addplan <- plan[plan$path %in% upd$add, ]
  stale <- addplan$path[
    is_link(addplan$target) |
      (file.exists(addplan$target) &
        (!addplan$isdir | !is_dir(addplan$target)))
  ]

  todel <- file.path(dst, pkgname, c(upd$delete, upd$update, stale))
  # Symlinks from older versions must be removed without `force = TRUE`,
  # because that calls `chmod()` on the symlink, which changes the mode of
  # the source file.
  links <- todel[is_link(todel)]
  unlink(links)
  unlink(setdiff(todel, links), force = TRUE, recursive = TRUE)
  # also add paths that were removed above, e.g. files in a deleted
  # symlinked directory
  planadd <- plan[
    plan$path %in% c(upd$add, upd$update) | !file.exists(plan$target),
  ]

  for (i in seq_len(nrow(planadd))) {
    if (planadd$isdir[i]) {
      mkdirp(planadd$target[i])
    } else {
      link_or_copy(planadd$path[i], planadd$target[i])
    }
  }

  plan_file <- file.path(dst, plan_file_name)
  quick_save_rds(plan, plan_file)
  setup_file <- file.path(dst, setup_file_name)
  quick_save_rds(getOption(opt_setup), setup_file)

  invisible(plan)
}

get_dev_dir <- function(setup = list(hash = "")) {
  file.path(".cache", "uncovr", setup$hash)
}

# specific for us, it should be in .Rbuildignore but have it here as well
ignored_extra <- c("^[.]cache$")

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
