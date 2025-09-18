#' Upload test coverage results to codecov
#'
#' @details
#' Currently supported CI services:
#'
#'  * `github-actions`: GitHub Actions.
#'
#' @param path Path to the package tree.
#' @param coverage Test coverage results. If `NULL` then the last
#'   results are used via [last()].
#' @param url Codecov URL. If `NULL` the default URL is used.
#' @param service The CI service name. If `NULL` then it is detected
#'   automatically from environment variables.
#' @param token Codecode token to identify the project. If the
#'  `CODECOV_TOKEN` environment variable is set, it is used as the token.
#' @param name A custom name for this specific upload.
#' @param commit The destination commit sha for the report. If `NULL` it is
#'   detected automatically.
#' @param branch The target branch for the report. If `NULL` it is
#'   detected automatically.
#' @param build The build number provided by your CI service. If `NULL` it is
#'   detected automatically.
#' @param job The job number provided by your CI service. If `NULL` it is
#'   detected automatically.
#' @param build_url The http url to link back to your CI provider. If `NULL`
#'   it is detected automatically.
#' @param slug The `owner/repo`` slug name of the project. If `NULL` it is
#'   detected automatically.
#' @param yaml The relative path to the `codecov.yml`` in this project.
#'   If `NULL` it is detected automatically.
#' @param flags Used for [flags](https://docs.codecov.com/v4.6/docs/flags).
#'   Can be one or more flags. E.g., `flags = "unit"` or `flags = "unit,java"`.
#' @param pr The pull request number this commit is currently found in.
#'   If `NULL` it is detected automatically.

#' @export

codecov <- function(
  path = ".",
  coverage = NULL,
  url = NULL,
  service = NULL,
  token = NULL,
  name = NULL,
  commit = NULL,
  branch = NULL,
  build = NULL,
  job = NULL,
  build_url = NULL,
  slug = NULL,
  yaml = NULL,
  flags = NULL,
  pr = NULL
) {
  withr::local_dir(path)
  rm(path)
  coverage <- coverage %||% last(path = ".")

  mkdirp(tmpdir <- tempfile())
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  report <- file.path(tmpdir, "lcov.info")
  lcov(coverage = coverage, output = report)
  cat("<<<<<< EOF\n", file = report, append = TRUE)

  inputs <- list(
    commit = commit %||% get_env("R_COV_COMMIT"),
    branch = branch %||% get_env("R_COV_BRANCH"),
    build = build %||% get_env("R_COV_BUILD"),
    job = job %||% get_env("R_COV_JOB"),
    build_url = build_url %||% get_env("R_COV_BUILD_URL"),
    name = name %||% get_env("R_COV_NAME"),
    slug = slug %||% get_env("R_COV_SLUG"),
    yaml = yaml %||% get_env("R_COV_YAML"),
    service = service %||% get_env("R_COV_SERVICE"),
    flags = flags %||% get_env("R_COV_FLAGS"),
    pr = pr %||% get_env("R_COV_PR")
  )

  cc_prov <- detect_codecov_provider(inputs)
  params <- cc_prov$get_service_params(inputs)

  token <- token %||% get_codecov_token()
  if (is.null(token)) {
    if (is.null(params[["slug"]])) {
      stop("Slug must the set if a Codecov token is not passed")
    } else {
      if (!grepl("/", params[["slug"]])) {
        stop("Slug must follow the format of <owner>/<repo> for Codecov upload")
      }
    }
  }

  url <- url %||% "https://codecov.io"
  furl <- paste0(url, "/upload/v4")
  furl <- curl::curl_modify_url(
    furl,
    params = c(params, token = token, name = name, yaml = yaml)
  )

  hh <- curl::new_handle()
  curl::handle_setopt(hh, customrequest = "POST")
  curl::handle_setheaders(
    hh,
    accept = "text/plain",
    "x-reduced-redundancy" = "false"
  )

  cli::cli_bullets(c(
    "i" = "Codecov upload ({params['service']})",
    "*" = "Repository: {.code {params['slug']}}",
    "*" = "Branch: {.code {params['branch']}}",
    "*" = "SHA: {.code {params['commit']}}",
    "!" = if (is.null(token)) {
      "Could not find Codecov token! Trying token-less upload."
    },
    "*" = if (!is.null(token)) {
      "Found Codecov token ({nchar(token)} characters)"
    }
  ))

  # TODO: retry
  resp <- curl::curl_fetch_memory(furl, handle = hh)
  if (resp$status_code != 200) {
    stop("Failed to upload coverage to Codecov.\n", rawToChar(resp$content))
  }
  cnt <- strsplit(rawToChar(resp$content), "\n", fixed = TRUE)[[1]]
  cc_url <- cnt[1]
  s3_url <- cnt[2]

  cli::cli_alert_success("Got Codecov upload URL")

  resp2 <- curl::curl_upload(
    report,
    s3_url,
    verbose = FALSE,
    reuse = FALSE,
    httpheader = c("content-type" = "text/plain")
  )
  if (resp2$status_code != 200) {
    stop("Failed to upload coverage to Codecov.\n", rawToChar(resp2$content))
  }

  cli::cli_alert_success("Browse code coverage at {.url {cc_url}}")

  invisible(list(url = cc_url))
}

get_codecov_token <- function() {
  # TODO validate
  get_env("CODECOV_TOKEN")
}

detect_codecov_provider <- function(inputs) {
  if (!is.null(inputs[["service"]])) {
    if (inputs[["service"]] %in% names(cc_providers)) {
      return(cc_providers[[inputs[["service"]]]])
    } else {
      stop("Unknown Codecov service provider: ", inputs[["service"]], ".")
    }
  }
  for (prov in cc_providers) {
    if (prov$detect()) {
      return(prov)
    }
  }
  stop("Could not detect Codecov service provider.")
}

ccprov_actions <- list(
  detect = function() {
    Sys.getenv("GITHUB_ACTIONS") != ""
  },

  get_build = function(inputs) {
    inputs[["build"]] %||% get_env("GITHUB_RUN_ID")
  },

  get_build_url = function(inputs) {
    if (!is.null(inputs[["build_url"]])) {
      return(inputs[["build_url"]])
    }
    srv <- get_env("GITHUB_SERVER_URL")
    slug <- ccprov_actions$get_slug(inputs)
    build <- ccprov_actions$get_build(inputs)
    if (!is.null(srv) && !is.null(slug) && !is.null(build)) {
      paste0(srv, "/", slug, "/actions/runs/", build)
    }
  },

  get_branch = function(inputs) {
    ref <- get_env("GITHUB_REF")
    message(ref)
    re_branch <- "^refs/heads/(.*)$"
    branch <- ref %&&% if (grepl(re_branch, ref)) sub(re_branch, "\\1", ref)
    message(branch)
    message(get_env("GITHUB_HEAD_REF"))
    inputs[["branch"]] %||% get_env("GITHUB_HEAD_REF") %||% branch
  },

  get_job = function(inputs) {
    inputs[["job"]] %||% get_env("GITHUB_WORKFLOW")
  },

  get_pr = function(inputs) {
    ref <- get_env("GITHUB_REF")
    re_pr <- "^refs/pull/([0-9]+)/merge$"
    pr <- get_env("GITHUB_HEAD_REF") %&&%
      if (grepl(re_pr, ref)) sub(re_pr, "\\1", ref)
    inputs[["pr"]] %||% pr
  },

  get_service = function(inputs) {
    "github-actions"
  },

  get_service_name = function(inputs) {
    "GitHub Actions"
  },

  get_commit = function(inputs) {
    if (!is.null(inputs[["commit"]])) {
      return(inputs[["commit"]])
    }
    pr <- ccprov_actions$get_pr(inputs)
    commit <- get_env("GITHUB_SHA")
    if (!is.null(pr)) {
      re_merge_commit <- "^[a-z0-9]{40} [a-z0-9]{40}$"
      merge_commit <- trimws(
        processx::run(
          "git",
          c("show", "--no-patch", "--format=%P")
        )$stdout
      )
      if (grepl(re_merge_commit, merge_commit)) {
        commit <- strsplit(merge_commit, " ", fixed = TRUE)[[1]][1]
      }
    }

    commit
  },

  get_slug = function(inputs) {
    inputs[["slug"]] %||% get_env("GITHUB_REPOSITORY")
  },

  get_service_params = function(inputs) {
    c(
      branch = ccprov_actions$get_branch(inputs),
      build = ccprov_actions$get_build(inputs),
      build_url = ccprov_actions$get_build_url(inputs),
      commit = ccprov_actions$get_commit(inputs),
      job = ccprov_actions$get_job(inputs),
      pr = ccprov_actions$get_pr(inputs),
      service = ccprov_actions$get_service(inputs),
      slug = ccprov_actions$get_slug(inputs)
    )
  },

  env_var_names = function() {
    c(
      "GITHUB_ACTION",
      "GITHUB_HEAD_REF",
      "GITHUB_REF",
      "GITHUB_REPOSITORY",
      "GITHUB_RUN_ID",
      "GITHUB_SERVER_URL",
      "GITHUB_SHA",
      "GITHUB_WORKFLOW"
    )
  }
)

cc_providers <- list("github-actions" = ccprov_actions)
