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
