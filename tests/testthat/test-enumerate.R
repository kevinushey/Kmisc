library(testthat)
library(compiler)

v <- rnorm(1E2)

test_that( enumerate(v, function(x) x + 1),
  is_identical_to( lapply(v, function(x) x + 1)))

test_that (enumerate(v, function(x, j) x + 1),
  is_identical_to( lapply(v, function(x) x + 1)))

microbenchmark( times=5,
  enumerate(v, function(x) x + 1),
  enumerate(v, function(x, i) x + 1),
  lapply(v, function(x) x + 1)
)
