test_that("map_chr", {
  expect_equal(map_chr(list(), identity), character())
  expect_equal(
    map_chr(character(), identity),
    structure(character(), names = character())
  )
  expect_equal(map_chr(1:5, function(i) letters[i]), letters[1:5])
})

test_that("map_int", {
  expect_equal(map_int(list(), function() 1L), integer())
  expect_equal(
    map_int(character(), function() 1L),
    structure(integer(), names = character())
  )
  expect_equal(map_int(1:5, identity), 1:5)
})

test_that("map_dbl", {
  expect_equal(map_dbl(list(), function() 1), double())
  expect_equal(
    map_dbl(character(), function() 1),
    structure(double(), names = character())
  )
  expect_equal(map_dbl(1:5, function(i) as.double(i)), as.double(1:5))
})
