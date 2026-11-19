local_demo_pkg <- function(.local_envir = parent.frame()) {
  tmp <- withr::local_tempdir(.local_envir = .local_envir)
  src <- system.file("example", "demopkg", package = "uncovr")
  file.copy(src, tmp, recursive = TRUE)
  withr::defer(
    if ("demopkg" %in% loadedNamespaces()) pkgload::unload("demopkg"),
    envir = .local_envir
  )
  file.path(tmp, "demopkg")
}

run_coverage <- function(pkg, ...) {
  suppressMessages(coverage(
    path = pkg,
    ...,
    reporter = "silent",
    show_coverage = FALSE,
    stop_on_failure = FALSE
  ))
}

file_cov <- function(res, file) {
  res$coverage[res$coverage$path == file, ]
}

test_that("coverage() end to end", {
  skip_on_cran()
  pkg <- local_demo_pkg()
  res <- run_coverage(pkg)

  expect_s3_class(res, "package_coverage")
  expect_s3_class(res$coverage, "coverage_table2")

  # tests ran and passed
  tr <- as.data.frame(res$test_results)
  expect_equal(sum(tr$failed), 0)
  expect_false(any(tr$error))
  expect_equal(sum(tr$nb), 4)

  expect_setequal(res$coverage$path, c("R/fizzbuzz.R", "R/greet.R"))

  fb <- file_cov(res, "R/fizzbuzz.R")
  expect_equal(fb$code_lines, 11L)
  expect_equal(fb$lines_covered, 10L)
  expect_equal(fb$uncovered[[1]], list(7L))
  expect_equal(fb$function_count, 1L)
  expect_equal(fb$functions_hit, 1L)
  expect_equal(fb$funs[[1]]$name, "fizzbuzz")
  # called three times in the tests
  expect_equal(fb$funs[[1]]$coverage, 3L)
  lns <- fb$lines[[1]]
  expect_equal(lns$coverage[lns$lines == "    \"FizzBuzz\""], 1L)
  expect_equal(lns$coverage[lns$lines == "    \"Buzz\""], 0L)

  gr <- file_cov(res, "R/greet.R")
  expect_equal(gr$lines_covered, 6L)
  expect_equal(gr$uncovered[[1]], list(3L))
  expect_equal(gr$funs[[1]]$name, "greet")

  sm <- attr(res$coverage, "summary")
  expect_equal(sm$code_lines[1], 18L)
  expect_equal(sm$lines_covered[1], 16L)

  # the original package tree is not modified
  expect_false(file.exists(file.path(pkg, ".cache")))
  src <- system.file("example", "demopkg", package = "uncovr")
  expect_setequal(
    dir(pkg, recursive = TRUE, all.files = TRUE),
    dir(src, recursive = TRUE, all.files = TRUE)
  )
  for (f in c("R/fizzbuzz.R", "R/greet.R", "DESCRIPTION")) {
    expect_equal(
      readLines(file.path(pkg, f)),
      readLines(file.path(src, f)),
      info = f
    )
  }
})

test_that("coverage() with a Collate field", {
  skip_on_cran()
  pkg <- local_demo_pkg()
  dsc <- file.path(pkg, "DESCRIPTION")
  cat("Collate:\n    'greet.R'\n    'fizzbuzz.R'\n", file = dsc, append = TRUE)
  orig <- readLines(dsc)

  res <- run_coverage(pkg)

  # the top-level expressions are counted in every file, so the counters
  # were created before any of the package files was loaded
  for (f in res$coverage$path) {
    expect_equal(file_cov(res, f)$lines[[1]]$coverage[1], 1L, info = f)
  }
  expect_equal(attr(res$coverage, "summary")$lines_covered[1], 16L)

  # Collate field is updated in the build, but not in the original
  expect_equal(readLines(dsc), orig)
  bdsc <- file.path(res$setup$dir, "demopkg", "DESCRIPTION")
  expect_equal(
    desc::desc_get_collate(file = bdsc),
    c("000-uncovr-counters.R", "greet.R", "fizzbuzz.R")
  )
})

test_that("coverage() twice reuses the build", {
  skip_on_cran()
  pkg <- local_demo_pkg()
  res1 <- run_coverage(pkg)
  res2 <- run_coverage(pkg)
  # counters are fresh for every run
  expect_equal(res2$coverage$lines_covered, res1$coverage$lines_covered)
  expect_equal(
    file_cov(res2, "R/fizzbuzz.R")$funs[[1]]$coverage,
    3L
  )
})

test_that("coverage() build_dir", {
  skip_on_cran()
  pkg <- local_demo_pkg()
  bd <- withr::local_tempdir()
  # relative to the working directory, not to the package tree
  withr::local_dir(bd)
  res <- run_coverage(pkg, build_dir = "builds")

  expect_true(startsWith(res$setup$dir, normalizePath(file.path(bd, "builds"))))
  expect_true(file.exists(file.path(res$setup$dir, "demopkg", "DESCRIPTION")))
  expect_equal(attr(res$coverage, "summary")$lines_covered[1], 16L)
  expect_equal(file_cov(res, "R/fizzbuzz.R")$funs[[1]]$name, "fizzbuzz")
})

test_that("coverage() uses separate builds for separate package trees", {
  skip_on_cran()
  pkg1 <- local_demo_pkg()
  pkg2 <- local_demo_pkg()
  bd <- withr::local_tempdir()
  res1 <- run_coverage(pkg1, build_dir = bd)
  res2 <- run_coverage(pkg2, build_dir = bd)
  expect_false(res1$setup$dir == res2$setup$dir)
  expect_equal(dirname(res1$setup$dir), dirname(res2$setup$dir))
})
