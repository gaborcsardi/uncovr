test_that("map_chr", {
  expect_equal(map_chr(list(), identity), character())
  expect_equal(
    map_chr(character(), identity),
    structure(character(), names = character())
  )
  expect_equal(map_chr(1:5, function(i) letters[i]), letters[1:5])
  expect_snapshot(error = TRUE, {
    map_chr(1:2, function(x) 1)
  })
})

test_that("map_int", {
  expect_equal(map_int(list(), function() 1L), integer())
  expect_equal(
    map_int(character(), function() 1L),
    structure(integer(), names = character())
  )
  expect_equal(map_int(1:5, identity), 1:5)
  expect_snapshot(error = TRUE, {
    map_int(1:2, function(x) 1)
  })
})

test_that("map_dbl", {
  expect_equal(map_dbl(list(), function() 1), double())
  expect_equal(
    map_dbl(character(), function() 1),
    structure(double(), names = character())
  )
  expect_equal(map_dbl(1:5, function(i) as.double(i)), as.double(1:5))
  expect_snapshot(error = TRUE, {
    map_dbl(1:2, function(x) "foo")
  })
})

test_that("map_lgl", {
  expect_equal(map_lgl(list(), function() TRUE), logical())
  expect_equal(
    map_lgl(character(), function() TRUE),
    structure(logical(), names = character())
  )
  expect_equal(
    map_lgl(1:5, function(i) i %% 2 == 0),
    c(FALSE, TRUE, FALSE, TRUE, FALSE)
  )
  expect_snapshot(error = TRUE, {
    map_lgl(1:2, function(x) 1)
  })
})

test_that("mkdirp", {
  tmp <- file.path(tempfile(), "foo", "bar")
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mkdirp(tmp)
  expect_true(file.exists(tmp))
  expect_silent(mkdirp(tmp))
})

test_that("%||%", {
  expect_equal("foo" %||% stop("no"), "foo")
  expect_equal(NULL %||% "bar", "bar")
  expect_equal(NULL %||% NULL, NULL)
})

test_that("%&&%", {
  expect_equal("foo" %&&% "bar", "bar")
  expect_equal(NULL %&&% stop("no"), NULL)
  expect_equal(NULL %&&% NULL, NULL)
})

test_that("find_zero_ranges", {
  expect_equal(
    find_zero_ranges(integer(), integer()),
    list()
  )
  expect_equal(
    find_zero_ranges(1:10, 1:10),
    list()
  )
  expect_equal(
    find_zero_ranges(1:10, c(1, 2, 3, 0, 0, 0, 7, 8, 9, 10)),
    list(4:6)
  )
  expect_equal(
    find_zero_ranges(1:10, c(1, 2, 3, 0, 0, 0, 7, 0, 9, 10)),
    list(4:6, 8)
  )
  expect_equal(
    find_zero_ranges(1:10, c(0, 0, 3, 0, 0, 0, 7, 0, 9, 10)),
    list(1:2, 4:6, 8)
  )
  expect_equal(
    find_zero_ranges(1:10, c(1, 2, 3, 0, 0, 0, 7, 0, 0, 0)),
    list(4:6, 8:10)
  )
})

test_that("is_windows", {
  expect_equal(is_windows(), .Platform$OS.type == "windows")
})

test_that("zero", {
  expect_equal(zero(integer()), character())
  expect_equal(zero(c(1, 2, 3, 0, 0, 6)), c("1", "2", "3", "", "", "6"))
})

test_that("order_df", {
  df <- data.frame(
    x = 1:5,
    y = 5:1
  )
  expect_equal(order_df(df, "y"), df[order(df$y), ])
})
