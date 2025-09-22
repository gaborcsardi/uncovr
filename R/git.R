has_git <- function() {
  Sys.which("git") != ""
}

git_has_changed <- function(paths = NULL) {
  ret <- processx::run(
    "git",
    c("diff-index", "--quiet", "HEAD", "--", paths),
    error_on_status = FALSE
  )
  ret$status == 1
}

git_is_untracked <- function(paths) {
  ret <- processx::run("git", c("ls-files", "--others", "--exclude-standard"))
  unt <- strsplit(ret$stdout, "\n")[[1]]
  paths %in% unt
}

git_current_branch <- function() {
  ret <- processx::run("git", c("symbolic-ref", "--short", "HEAD"))
  trimws(ret$stdout)
}

git_branch_exists <- function(branch) {
  ret <- processx::run(
    "git",
    c("show-ref", "--quiet", paste0("refs/heads/", branch)),
    error_on_status = FALSE
  )
  ret$status == 0
}

git_default_branch <- function() {
  if (git_branch_exists("main")) {
    "main"
  } else if (git_branch_exists("master")) {
    "master"
  } else {
    stop(
      "Cannot calculate coverage relative to default branch, ",
      "cannot determine the default branch."
    )
  }
}
