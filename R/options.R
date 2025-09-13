opt_default <- list(
  lcov_info = FALSE
)

get_option <- function(opt, type = "flag", default = opt_default[[opt]]) {
  type <- match.arg(type)
  attr(default, "source") <- "default"

  envname <- opt_envvar_name(opt)
  optname <- opt_option_name(opt)
  val <- get_opt(optname) %||% get_env(envname) %||% default

  switch(
    type,
    flag = {
      get_option_flag(opt, val)
    }
  )
}

get_option_flag <- function(opt, val) {
  if (isTRUE(val) || isFALSE(val)) {
    return(val)
  }
  if (is_string(val) && tolower(val) %in% true_values) {
    return(structure(TRUE, source = attr(val, "source")))
  }
  if (is_string(val) && tolower(val) %in% false_values) {
    return(structure(FALSE, source = attr(val, "source")))
  }
  source <- attr(val, "source")
  switch(
    source[1],
    envvar = {
      cli::cli_abort(c(
        "Invalid value for {.val {opt}}, from {.envvar {source[2]}}
         environment variable).",
        i = "It must be {.val TRUE} or {.val FALSE}.",
        i = "It is {.val {val}}."
      ))
    },
    option = {
      cli::cli_abort(c(
        "Invalid value for {.val {opt}}, from {.var {source[2]}} option).",
        i = "It must be a {.val TRUE} or {.val FALSE} flag.",
        i = "It is {.type {val}}."
      ))
    },
    default = {
      cli::cli_abort(c(
        "Invalid default value for {.val {opt}}.",
        i = "It must be a {.val TRUE} or {.val FALSE} flag.",
        i = "It is {.type {val}}."
      ))
    }
  )
}

opt_envvar_name <- function(opt, pkgname = utils::packageName()) {
  opt <- gsub("-", "_", opt)
  paste0("R_", toupper(pkgname), "_", toupper(opt))
}

opt_option_name <- function(opt, pkgname = utils::packageName()) {
  opt <- gsub("-", "_", opt)
  paste0(pkgname, "_", tolower(opt))
}

get_env <- function(env) {
  val <- Sys.getenv(env, NA_character_)
  if (is.na(val)) {
    NULL
  } else {
    structure(val, source = c("envvar", env))
  }
}

get_opt <- function(opt) {
  val <- getOption(opt)
  val %&&% structure(val, source = c("option", opt))
}

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

true_values <- c("true", "t", "yes", "yeah", "yep", "y", "1", "on")
false_values <- c("false", "f", "no", "nay", "nope", "n", "0", "off")
