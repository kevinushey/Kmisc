library(testthat)
library(microbenchmark)
library(compiler)

v <- replicate(10, rnorm(1E3), simplify=FALSE)

test_that( enumerate(v, function(x) x + 1),
  is_identical_to( lapply(v, function(x) x + 1)))

test_that (enumerate(v, function(x, j) x + 1),
  is_identical_to( lapply(v, function(x) x + 1)))

l <- as.list(1:10)
test_that( enumerate(l, function(x, k) x + 1),
  is_identical_to( lapply(v, function(x) x + 1)))

microbenchmark( times=5,
  enumerate(v, function(x) x + 1),
  enumerate(v, function(x, i) x + 1),
  lapply(v, function(x) x + 1)
)
