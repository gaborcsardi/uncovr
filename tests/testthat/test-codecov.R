test_that("get_codecov_token prefers CODECOV_TOKEN", {
  withr::local_envvar(
    CODECOV_TOKEN = "secret",
    GITHUB_ACTIONS = "true",
    ACTIONS_ID_TOKEN_REQUEST_URL = "https://example.com/token?api-version=2",
    ACTIONS_ID_TOKEN_REQUEST_TOKEN = "reqtoken"
  )
  local_mocked_bindings(
    github_oidc_fetch = function(...) stop("should not be called")
  )
  expect_equal(c(get_codecov_token()), "secret")
})

test_that("get_github_oidc_token returns NULL outside of GitHub Actions", {
  withr::local_envvar(
    CODECOV_TOKEN = NA,
    GITHUB_ACTIONS = NA,
    ACTIONS_ID_TOKEN_REQUEST_URL = "https://example.com/token?api-version=2",
    ACTIONS_ID_TOKEN_REQUEST_TOKEN = "reqtoken"
  )
  expect_null(get_github_oidc_token())
  expect_null(get_codecov_token())
})

test_that("get_github_oidc_token returns NULL without id-token permission", {
  withr::local_envvar(
    GITHUB_ACTIONS = "true",
    ACTIONS_ID_TOKEN_REQUEST_URL = NA,
    ACTIONS_ID_TOKEN_REQUEST_TOKEN = NA
  )
  expect_null(get_github_oidc_token())
})

test_that("get_github_oidc_token", {
  withr::local_envvar(
    CODECOV_TOKEN = NA,
    GITHUB_ACTIONS = "true",
    ACTIONS_ID_TOKEN_REQUEST_URL = "https://example.com/token?api-version=2",
    ACTIONS_ID_TOKEN_REQUEST_TOKEN = "reqtoken"
  )
  args <- NULL
  local_mocked_bindings(
    github_oidc_fetch = function(url, req_token) {
      args <<- list(url = url, req_token = req_token)
      list(status_code = 200L, content = charToRaw('{"value":"jwt"}'))
    }
  )
  expect_output(token <- get_codecov_token(), "::add-mask::jwt", fixed = TRUE)
  expect_equal(c(token), "jwt")
  expect_equal(attr(token, "source")[1], "oidc")
  expect_equal(
    args$url,
    "https://example.com/token?api-version=2&audience=https%3A%2F%2Fcodecov.io"
  )
  expect_equal(c(args$req_token), "reqtoken")
})

test_that("get_github_oidc_token errors", {
  withr::local_envvar(
    GITHUB_ACTIONS = "true",
    ACTIONS_ID_TOKEN_REQUEST_URL = "https://example.com/token?api-version=2",
    ACTIONS_ID_TOKEN_REQUEST_TOKEN = "reqtoken"
  )
  local_mocked_bindings(
    github_oidc_fetch = function(url, req_token) {
      list(status_code = 403L, content = charToRaw("forbidden"))
    }
  )
  expect_error(get_github_oidc_token(), "forbidden")

  local_mocked_bindings(
    github_oidc_fetch = function(url, req_token) {
      list(status_code = 200L, content = charToRaw("{}"))
    }
  )
  expect_error(get_github_oidc_token(), "invalid response")
})

test_that("codecov() no_token", {
  withr::local_envvar(
    CI = NA,
    CODECOV_TOKEN = NA,
    R_COV_NO_TOKEN = NA,
    GITHUB_ACTIONS = "true",
    GITHUB_REPOSITORY = "owner/repo",
    GITHUB_REF = "refs/heads/main",
    GITHUB_HEAD_REF = NA,
    GITHUB_EVENT_PATH = NA,
    ACTIONS_ID_TOKEN_REQUEST_URL = NA,
    ACTIONS_ID_TOKEN_REQUEST_TOKEN = NA
  )
  cov <- list()

  # not a PR: error by default
  expect_error(codecov(coverage = cov, commit = "abc"), "No Codecov token")
  expect_message(
    res <- codecov(coverage = cov, commit = "abc", no_token = "skip"),
    "skipping Codecov upload"
  )
  expect_null(res$url)

  # PR, not from a fork: also error by default
  withr::local_envvar(
    GITHUB_REF = "refs/pull/12/merge",
    GITHUB_HEAD_REF = "feature"
  )
  expect_error(codecov(coverage = cov, commit = "abc"), "No Codecov token")
  expect_message(
    codecov(coverage = cov, commit = "abc", no_token = "skip"),
    "skipping Codecov upload for pull request"
  )

  # env var
  withr::local_envvar(R_COV_NO_TOKEN = "skip")
  expect_message(
    codecov(coverage = cov, commit = "abc"),
    "skipping Codecov upload"
  )

  expect_error(codecov(coverage = cov, commit = "abc", no_token = "foo"))
})

local_fork_pr <- function(head_repo, .local_envir = parent.frame()) {
  event <- withr::local_tempfile(.local_envir = .local_envir)
  owner <- sub("/.*$", "", head_repo)
  jsonlite::write_json(
    list(
      pull_request = list(
        head = list(
          label = paste0(owner, ":feature"),
          repo = list(full_name = head_repo)
        )
      )
    ),
    event,
    auto_unbox = TRUE
  )
  withr::local_envvar(
    CI = NA,
    CODECOV_TOKEN = NA,
    R_COV_NO_TOKEN = NA,
    GITHUB_ACTIONS = "true",
    GITHUB_REPOSITORY = "owner/repo",
    GITHUB_REF = "refs/pull/12/merge",
    GITHUB_HEAD_REF = "feature",
    GITHUB_EVENT_PATH = event,
    ACTIONS_ID_TOKEN_REQUEST_URL = NA,
    ACTIONS_ID_TOKEN_REQUEST_TOKEN = NA,
    .local_envir = .local_envir
  )
}

test_that("is_fork, get_head_label", {
  local_fork_pr("someone/repo")
  expect_true(ccprov_actions$is_fork(list()))
  expect_equal(ccprov_actions$get_head_label(list()), "someone:feature")

  local_fork_pr("owner/repo")
  expect_false(ccprov_actions$is_fork(list()))

  withr::local_envvar(GITHUB_EVENT_PATH = NA)
  expect_false(ccprov_actions$is_fork(list()))
  expect_null(ccprov_actions$get_head_label(list()))
})

test_that("codecov() token-less upload for fork PR", {
  local_fork_pr("someone/repo")
  args <- NULL
  local_mocked_bindings(
    codecov_upload = function(url, token, params, coverage) {
      args <<- list(token = token, params = params)
      list(cc_url = "https://codecov.io/x")
    }
  )
  expect_message(
    res <- codecov(coverage = list(), commit = "abc"),
    "token-less"
  )
  expect_null(args$token)
  expect_equal(args$params$branch, "someone:feature")
  expect_equal(res$url, "https://codecov.io/x")

  # explicit branch is kept
  suppressMessages(
    codecov(coverage = list(), commit = "abc", branch = "mybranch")
  )
  expect_equal(c(args$params$branch), "mybranch")
})

test_that("codecov() failed token-less upload", {
  local_fork_pr("someone/repo")
  local_mocked_bindings(
    codecov_upload = function(...) stop("Token required")
  )
  # auto: skip
  expect_message(
    res <- codecov(coverage = list(), commit = "abc"),
    "upload failed, skipping"
  )
  expect_null(res$url)

  # explicit tokenless: error
  expect_error(
    codecov(coverage = list(), commit = "abc", no_token = "tokenless"),
    "Token required"
  )
})

test_that("codecov() does not try token-less upload for non-fork PR", {
  local_fork_pr("owner/repo")
  local_mocked_bindings(
    codecov_upload = function(...) stop("should not be called")
  )
  expect_error(codecov(coverage = list(), commit = "abc"), "No Codecov token")
})

test_that("codecov() errors if upload with token fails in non-fork PR", {
  local_fork_pr("owner/repo")
  withr::local_envvar(CODECOV_TOKEN = "secret")
  local_mocked_bindings(
    codecov_upload = function(...) stop("Upload failed")
  )
  expect_error(
    suppressMessages(codecov(coverage = list(), commit = "abc")),
    "Upload failed"
  )
})
