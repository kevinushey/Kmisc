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

m <- matrix(1:9, nrow=3)
rownames(m) <- letters[1:3]
colnames(m) <- LETTERS[1:3]

stopifnot( identical(
  transpose(m),
  t(m)
) )

l <- list(1:3, 4:7)
expect_error(transpose(l))
l <- list(1:3, letters[4:6])
expect_warning(transpose(l))
