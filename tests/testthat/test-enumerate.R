context("enumerate")

## these fail because testthat breaks the evaluation of enumerate
v <- replicate(2, rnorm(2), simplify=FALSE)

expect_that (enumerate(v, function(x, j) x + 1),
  is_identical_to( lapply(v, function(x) x + 1)))

l <- as.list(1:10)
expect_that( enumerate(l, function(x, k) x + 1),
  is_identical_to( lapply(l, function(x) x + 1)))

## make sure even the ridiculous calls might work
enumerate <- 10:1
expect_that( enumerate(l, function(x, k) enumerate[k]),
  is_identical_to( as.list(10:1)))
