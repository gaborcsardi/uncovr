test_that("clone_link_or_copy", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mkdirp(tmp)
  src <- file.path(tmp, "src")
  writeLines(c("foo", "bar"), src)

  tgt <- file.path(tmp, "sub", "dir", "tgt")
  how <- clone_link_or_copy(src, tgt)
  expect_true(how %in% c("clone", "link", "copy"))
  expect_equal(readLines(tgt), c("foo", "bar"))
  expect_false(is_link(tgt))

  # cloning does not overwrite
  expect_false(clone_file(src, tgt))
  expect_equal(readLines(tgt), c("foo", "bar"))

  # missing source
  expect_false(clone_file(file.path(tmp, "nope"), tgt))
})

test_that("clone_link_or_copy falls back to hard links", {
  local_mocked_bindings(clone_file = function(...) FALSE)
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mkdirp(tmp)
  src <- file.path(tmp, "src")
  writeLines("foo", src)
  tgt <- file.path(tmp, "tgt")
  how <- clone_link_or_copy(src, tgt)
  expect_true(how %in% c("link", "copy"))
  expect_equal(readLines(tgt), "foo")
})

test_that("clone_link_or_copy errors if it cannot copy", {
  local_mocked_bindings(clone_file = function(...) FALSE)
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mkdirp(tmp)
  src <- file.path(tmp, "src")
  writeLines("foo", src)
  tgt <- file.path(tmp, "tgt")
  writeLines("bar", tgt)
  expect_error(clone_link_or_copy(src, tgt), "Failed to copy")
})

test_that("write_lines_safe does not write through hard links", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mkdirp(tmp)
  src <- file.path(tmp, "src")
  tgt <- file.path(tmp, "tgt")
  writeLines("foo", src)
  skip_if_not(isTRUE(suppressWarnings(file.link(src, tgt))))

  write_lines_safe("changed", tgt)
  expect_equal(readLines(tgt), "changed")
  expect_equal(readLines(src), "foo")
})

test_that("update_package_tree", {
  src <- tempfile()
  dst <- tempfile()
  on.exit(unlink(c(src, dst), recursive = TRUE), add = TRUE)
  mkdirp(file.path(src, "R"))
  mkdirp(file.path(src, "inst", "empty"))
  writeLines("Package: foo", file.path(src, "DESCRIPTION"))
  writeLines("f <- function() 1", file.path(src, "R", "f.R"))
  withr::local_options(structure(list(list()), names = opt_setup))

  plan <- update_package_tree(src, dst, pkgname = "foo")
  tgt <- file.path(dst, "foo")
  expect_equal(readLines(file.path(tgt, "R", "f.R")), "f <- function() 1")
  expect_true(is_dir(file.path(tgt, "inst", "empty")))
  expect_false(any(is_link(plan$target)))

  # updates and deletions
  writeLines("f <- function() 2", file.path(src, "R", "f.R"))
  writeLines("g <- function() 1", file.path(src, "R", "g.R"))
  unlink(file.path(src, "inst"), recursive = TRUE)
  update_package_tree(src, dst, pkgname = "foo")
  expect_equal(readLines(file.path(tgt, "R", "f.R")), "f <- function() 2")
  expect_equal(readLines(file.path(tgt, "R", "g.R")), "g <- function() 1")
  expect_false(file.exists(file.path(tgt, "inst")))

  # changes in the build tree are reverted
  write_lines_safe("modified", file.path(tgt, "R", "f.R"))
  expect_equal(readLines(file.path(src, "R", "f.R")), "f <- function() 2")
  update_package_tree(src, dst, pkgname = "foo")
  expect_equal(readLines(file.path(tgt, "R", "f.R")), "f <- function() 2")
})

test_that("update_package_tree replaces existing files not in the plan", {
  src <- tempfile()
  dst <- tempfile()
  on.exit(unlink(c(src, dst), recursive = TRUE), add = TRUE)
  mkdirp(file.path(src, "R"))
  mkdirp(file.path(src, "src"))
  writeLines("Package: foo", file.path(src, "DESCRIPTION"))
  writeLines("f <- function() 1", file.path(src, "R", "f.R"))
  writeLines("int x;", file.path(src, "src", "foo.c"))
  withr::local_options(structure(list(list()), names = opt_setup))

  update_package_tree(src, dst, pkgname = "foo")
  tgt <- file.path(dst, "foo")

  # build artifact, not in the source tree
  writeLines("generated", file.path(tgt, "src", "Makevars"))
  writeLines("object", file.path(tgt, "src", "foo.o"))
  # now the file is added to the source tree
  writeLines("PKG_CFLAGS = -O0", file.path(src, "src", "Makevars"))
  update_package_tree(src, dst, pkgname = "foo")
  expect_equal(
    readLines(file.path(tgt, "src", "Makevars")),
    "PKG_CFLAGS = -O0"
  )

  # no plan file, e.g. after an interrupted run
  write_lines_safe("modified", file.path(tgt, "R", "f.R"))
  unlink(file.path(dst, plan_file_name))
  update_package_tree(src, dst, pkgname = "foo")
  expect_equal(readLines(file.path(tgt, "R", "f.R")), "f <- function() 1")
  # existing directories are kept, with the build artifacts
  expect_equal(readLines(file.path(tgt, "src", "foo.o")), "object")
})

test_that("update_package_tree replaces old symlinks", {
  skip_on_os("windows")
  src <- tempfile()
  dst <- tempfile()
  on.exit(unlink(c(src, dst), recursive = TRUE), add = TRUE)
  mkdirp(file.path(src, "R"))
  writeLines("Package: foo", file.path(src, "DESCRIPTION"))
  writeLines("f <- function() 1", file.path(src, "R", "f.R"))
  Sys.chmod(file.path(src, "DESCRIPTION"), "0644")
  withr::local_options(structure(list(list()), names = opt_setup))

  update_package_tree(src, dst, pkgname = "foo")
  tgt <- file.path(dst, "foo")
  # simulate a build tree from an older version
  unlink(file.path(tgt, c("R", "DESCRIPTION")), recursive = TRUE)
  file.symlink(normalizePath(file.path(src, "R")), file.path(tgt, "R"))
  file.symlink(
    normalizePath(file.path(src, "DESCRIPTION")),
    file.path(tgt, "DESCRIPTION")
  )

  update_package_tree(src, dst, pkgname = "foo")
  expect_false(is_link(file.path(tgt, "DESCRIPTION")))
  # the mode of the source file is not changed
  expect_equal(
    format(file.mode(file.path(src, "DESCRIPTION"))),
    "644"
  )
  expect_false(is_link(file.path(tgt, "R")))
  expect_true(is_dir(file.path(tgt, "R")))
  expect_equal(readLines(file.path(tgt, "R", "f.R")), "f <- function() 1")
  expect_true(file.exists(file.path(src, "R", "f.R")))
})
