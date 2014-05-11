context("dictionary")

test_that("dictionary primitives work", {

  d <- dict()
  d["a"] <- 1
  expect_identical(d[["a"]], 1)
  expect_identical(d["a"], list(1))
  d["b"] <- 2
  expect_equal(d[["b"]], 2)
  expect_equal(length(d), 2)
  d[["c"]] <- 3
  expect_equal(d[["c"]], 3)
  d[["c"]] <- 4
  expect_equal(d[["c"]], 4)
  expect_equal(length(d), 3)
  en <- enumerate(d, function(key, value) list(key, value))
  expect_identical(en, list(list("a", 1), list("b", 2), list("c", 4)))

})
