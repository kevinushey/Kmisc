library(testthat)

## these fail because testthat breaks the evaluation of enumerate
v <- replicate(1E3, rnorm(1E3), simplify=FALSE)
lapply(v, function(x) x + 1)
enumerate(v, function(x, j) x + 1)

test_that (enumerate(v, function(x, j) x + 1),
  is_identical_to( lapply(v, function(x) x + 1)))

l <- as.list(1:10)
test_that( enumerate(l, function(x, k) x + 1),
  is_identical_to( lapply(v, function(x) x + 1)))
