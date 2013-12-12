library(microbenchmark)
library(Kmisc)
library(testthat)

Rswap <- function(vec, from, to) {
  tmp <- to[match(vec, from)]
  tmp[is.na(tmp)] <- vec[is.na(tmp)]
  return(tmp)
}

m <- 1:10
x <- 1:5
y <- (1:5)*10

expect_identical(
  swap(m, x, y),
  Rswap(m, x, y)
)

y <- as.character(y)

expect_identical(
  swap(m, x, y),
  Rswap(m, x, y)
)

y <- (1:5)*10
m <- as.character(m)

expect_identical(
  swap(m, x, y),
  Rswap(m, x, y)
)

m[c(2, 5)] <- NA

expect_identical(
  swap(m, x, y),
  Rswap(m, x, y)
)

m <- as.integer(m)

expect_identical(
  swap(m, x, y),
  Rswap(m, x, y)
)

m <- as.numeric(m)
x <- as.integer(x)

expect_identical(
  swap(m, x, y),
  Rswap(m, x, y)
)

x[c(2)] <- NA

identical(swap(x, c(0, 1), c(10, 20)), Rswap(x, c(0, 1), c(10, 20)))
