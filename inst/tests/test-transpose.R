library(microbenchmark)
library(Kmisc)
library(testthat)

l <- replicate(10, rnorm(1E1), simplify=FALSE)
gctorture(TRUE)
tmp1 <- transpose(l)
gctorture(FALSE)
expect_identical(
  tmp1,
  tmp2 <- unname(as.list(as.data.frame(t(as.matrix(as.data.frame(l))))))
)


l <- list(1:3, 4:7)
expect_error(transpose(l))
l <- list(1:3, letters[4:6])
expect_warning(transpose(l))
