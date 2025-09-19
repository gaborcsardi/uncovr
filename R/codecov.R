#' Upload test coverage results to codecov
#'
#' @details
#' Currently supported CI services:
#'
#'  * GitHub Actions.
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
    stop(
      "No Codecov token. You need to set the `CODECOV_TOKEN` environment ",
      "variable to your Codecov token, either a global token for your ",
      "organization, or a repository token."
    )
  }

  # TODO: this is once we have token-less uploads
  # TODO: how would this even work with a global token?
  if (is.null(token)) {
    if (is.null(params[["slug"]])) {
      stop("Slug must the set if a Codecov token is not passed")
    } else {
      if (!grepl("/", params[["slug"]])) {
        stop("Slug must follow the format of <owner>/<repo> for Codecov upload")
      }
    }
  }

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

  codecov_create_commit(url, token, params)
  codecov_create_report(url, token, params)
  resp <- codecov_start_upload(url, token, params)
  payload <- codecov_create_payload(coverage)
  codecov_store_upload(resp$s3_url, payload)

  cli::cli_alert_success("Browse code coverage at {.url {resp$cc_url}}")

  invisible(list(url = resp$cc_url))
}

codecov_store_upload <- function(url, payload) {
  payload_file <- tempfile()
  on.exit(unlink(payload_file), add = TRUE)
  writeBin(charToRaw(payload), payload_file)

  withr::local_options(HTTPUserAgent = "codecov-cli/11.2.0")

  resp <- curl::curl_upload(
    payload_file,
    url,
    verbose = FALSE,
    reuse = FALSE
  )
  if (resp$status_code != 200) {
    stop("Failed to upload coverage to Codecov.\n", rawToChar(resp$content))
  }
}

codecov_create_payload <- function(coverage) {
  mkdirp(tmpdir <- tempfile())
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  report <- file.path(tmpdir, "lcov.info")
  lcov(coverage = coverage, output = report)

  payload <- list(
    "report_fixes" = list(
      "format" = "legacy",
      "value" = structure(list(), names = character())
    ),
    "network_files" = c(coverage$path, ""),
    "coverage_files" = list(format_codecov_report(report)),
    "metadata" = structure(list(), names = character())
  )

  jsonlite::toJSON(payload, auto_unbox = TRUE)
}

format_codecov_report <- function(report) {
  list(
    filename = file.path(getwd(), "lcov.info"),
    format = "base64+compressed",
    data = encode_codecov_file(report),
    labels = ""
  )
}

encode_codecov_file <- function(report) {
  buf <- readBin(report, "raw", n = file.size(report))
  bufz <- memCompress(buf, type = "gzip")
  b64 <- jsonlite::base64_enc(bufz)
  b64
}

codecov_start_upload <- function(url, token, params) {
  ingest_url <- url %||% "https://ingest.codecov.io"

  furl <- paste0(
    ingest_url,
    "/upload/",
    params[["service"]],
    "/",
    encode_slug(params[["slug"]]),
    "/commits/",
    params[["commit"]],
    "/reports/default/uploads"
  )

  data <- list(
    ci_service = "github-actions",
    ci_url = NULL,
    # the server checks if `cli_args` is included, but not its value
    cli_args = list(
      version = "cli-11.2.0"
    ),
    env = structure(list(), names = character()),
    flags = list(),
    job_code = NULL,
    name = NULL,
    version = "11.2.0",
    file_not_found = FALSE
  )
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, null = "null")

  hand <- curl::new_handle()
  headers <- not_null(list(
    # I am not sure if this is needed
    "user-agent" = "codecov-cli/11.2.0",
    "content-type" = "application/json",
    "content-length" = as.character(nchar(json)),
    "authorization" = if (!is.null(token)) paste0("token ", token)
  ))
  curl::handle_setheaders(hand, .list = headers)
  curl::handle_setopt(hand, customrequest = "POST", postfields = json)

  resp <- curl::curl_fetch_memory(furl, handle = hand)
  if (resp$status_code >= 300) {
    stop(
      "Failed to fetch upload storage URL from Codecov.\n",
      rawToChar(resp$content)
    )
  }

  respurls <- jsonlite::parse_json(rawToChar(resp$content))
  cc_url <- respurls[["url"]]
  s3_url <- respurls[["raw_upload_location"]]

  cli::cli_alert_success("Got Codecov upload URL")

  list(cc_url = cc_url, s3_url = s3_url)
}

encode_slug <- function(slug) {
  if (!is.null(slug)) {
    splitslug <- strsplit(slug, "/")[[1]]
    paste0(gsub("/", ":::", splitslug[1]), "::::", splitslug[2])
  }
}

codecov_create_commit <- function(url, token, params) {
  ingest_url <- url %||% "https://ingest.codecov.io"

  furl <- paste0(
    ingest_url,
    "/upload/",
    params[["service"]],
    "/",
    encode_slug(params[["slug"]]),
    "/commits"
  )

  data <- na_omit(c(
    branch = params[["branch"]],
    # the server checks if `cli_args` is included, but not its value
    cli_args = list(
      version = "cli-11.2.0"
    ),
    commitid = params[["commit"]],
    parent_commit_id = NULL,
    pullid = params[["pr"]]
  ))
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, null = "null")

  hand <- curl::new_handle()
  headers <- not_null(list(
    # I am not sure if this is needed
    "user-agent" = "codecov-cli/11.2.0",
    "content-type" = "application/json",
    "content-length" = as.character(nchar(json)),
    "authorization" = if (!is.null(token)) paste0("token ", token)
  ))
  curl::handle_setheaders(hand, .list = headers)
  curl::handle_setopt(hand, customrequest = "POST", postfields = json)

  resp <- curl::curl_fetch_memory(furl, handle = hand)
  if (resp$status_code >= 300) {
    stop("Failed to create commit at Codecov.\n", rawToChar(resp$content))
  }
}

codecov_create_report <- function(url, token, params) {
  ingest_url <- url %||% "https://ingest.codecov.io"
  furl <- paste0(
    ingest_url,
    "/upload/",
    params[["service"]],
    "/",
    encode_slug(params[["slug"]]),
    "/commits/",
    params[["commit"]],
    "/reports"
  )

  data <- list(
    # the server checks if `cli_args` is included, but not its value
    cli_args = list(
      version = "cli-11.2.0"
    ),
    code = "default"
  )
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, null = "null")

  hand <- curl::new_handle()
  headers <- not_null(list(
    # I am not sure if this is needed
    "user-agent" = "codecov-cli/11.2.0",
    "content-type" = "application/json",
    "content-length" = as.character(nchar(json)),
    "authorization" = if (!is.null(token)) paste0("token ", token)
  ))
  curl::handle_setheaders(hand, .list = headers)
  curl::handle_setopt(hand, customrequest = "POST", postfields = json)

  resp <- curl::curl_fetch_memory(furl, handle = hand)
  if (resp$status_code >= 300) {
    stop("Failed to create report at Codecov.\n", rawToChar(resp$content))
  }
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
    re_branch <- "^refs/heads/(.*)$"
    branch <- ref %&&% if (grepl(re_branch, ref)) sub(re_branch, "\\1", ref)
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
    "github"
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
    list(
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

cc_providers <- list("github" = ccprov_actions)
