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

  res <- rbind(topplan, restplan)
  class(res) <- c("tbl", class(res))
  res
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

  plan_file <- file.path(dst, plan_file_name)
  saveRDS(plan, plan_file)
  setup_file <- file.path(dst, setup_file_name)
  saveRDS(getOption(opt_setup), setup_file)

  invisible(plan)
}

#' List development builds
#'
#' @inheritParams reload
#' @return Data frame with columns:
#'   - `type`: Build type, character.
#'   - `r_version`: R version, first two digits only, character.
#'   - `platform`: Build platform triplet, character.
#'   - `last_built`: Time stamp of the last build, [POSIXct].
#'   - `dist_size`: Size of the build directory in bytes, double.
#'   - `id`: Name of the build directory.
#' @export

builds <- function(path = ".") {
  withr::local_dir(path)
  dirs <- dir(get_dev_dir(), full.names = TRUE, include.dirs = TRUE)
  setup_paths <- file.path(dirs, setup_file_name)
  setups <- lapply(setup_paths, readRDS)
  builds <- data.frame(
    stringsAsFactors = FALSE,
    type = map_chr(setups, "[[", "type"),
    r_version = map_chr(setups, "[[", "rver"),
    platform = map_chr(setups, "[[", "platform"),
    last_built = file.mtime(setup_paths),
    disk_size = dir_size(dirs),
    id = basename(dirs)
  )
  rownames(builds) <- NULL

  # Better printing if pillar is available
  class(builds) <- c("tbl", class(builds))
  requireNamespace("pillar", quietly = TRUE)

  builds
}
