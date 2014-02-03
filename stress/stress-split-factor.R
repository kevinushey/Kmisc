factor__ <- function(f) {
  levels <- sort(unique.default(x))
  f <- match(x, levels)
  levels(f) <- as.character(levels)
  class(f) <- "factor"
  f
}

library(microbenchmark)

x <- sample(letters, 1E5, TRUE)
x[100] <- NA
f <- sample(letters, 1E5, TRUE)
f[100] <- NA

microbenchmark(
  split(x, f),
  split_(x, f),
  split(x, factor_(f))
)

microbenchmark(
  factor(x),
  factor_(x),
  factor__(x)
)

x <- sample(1:10, 1E5, TRUE)
f <- sample(1:10, 1E5, TRUE)

microbenchmark(
  split(x, f),
  split_(x, f),
  split(x, factor_(f))
)

microbenchmark(
  as.factor(x),
  factor(x),
  factor_(x),
  factor__(x)
)

x <- rnorm(1E5)
x[ sample(1:1E5, 1E3) ] <- NA
x[ sample(1:1E5, 1E3) ] <- NaN

microbenchmark(
  split(x, f),
  split_(x, f),
  split(x, as.factor(f)),
  split(x, factor_(f))
)

microbenchmark( times=5,
  factor(x),
  factor_(x),
  factor__(x)
)

x <- rnorm(10)
x[1:3] <- NA
x[6:8] <- NaN
identical( factor_(x), factor__(x) )