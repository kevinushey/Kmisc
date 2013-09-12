library(testthat)
library(Kmisc)

m <- matrix(1:16, nrow=4, ncol=4)
rownames(m) <- letters[1:4]
colnames(m) <- LETTERS[1:4]

expect_identical( 
  colApply(m, sum), 
  setNames( as.integer(c(10, 26, 42, 58)), LETTERS[1:4]) 
)

expect_identical( 
  rowApply(m, sum), 
  setNames( as.integer( c(28, 32, 36, 40) ), letters[1:4])
)

expect_identical( 
  colApply(m, sum, drop=FALSE), 
  `colnames<-`(matrix( as.integer(c(10, 26, 42, 58)), ncol=4 ), LETTERS[1:4])
)

expect_identical( 
  rowApply(m, sum, drop=FALSE), 
  `rownames<-`(matrix( as.integer(c(28, 32, 36, 40)), nrow=4 ), letters[1:4])
)

expect_identical(
  colApply(m, function(x) x / sum(x)),
  apply(m, 2, function(x) x / sum(x))
)

expect_identical(
  rowApply(m, function(x) x / sum(x)),
  t(apply(m, 1, function(x) x/sum(x)))
)