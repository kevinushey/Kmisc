library(testthat)
library(microbenchmark)
library(compiler)

v <- replicate(1E3, rnorm(1E3), simplify=FALSE)

test_that (enumerate(v, function(x, j) x + 1),
  is_identical_to( lapply(v, function(x) x + 1)))

l <- as.list(1:10)
test_that( enumerate(l, function(x, k) x + 1),
  is_identical_to( lapply(v, function(x) x + 1)))

microbenchmark( times=100,
  enumerate(v, function(x, i) x + 1),
  lapply(v, function(x) x + 1)
)

v <- replicate(3, rnorm(3), simplify=FALSE)
x <- 1:3
gctorture(TRUE)
enumerate(v, function(x, i) x + i)
gctorture(FALSE)
