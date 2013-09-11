library(testthat)
library(Kmisc)

m <- matrix(1, nrow=4, ncol=4)
rownames(m) <- letters[1:4]
colnames(m) <- LETTERS[1:4]
expect_identical( colApply(m, sum), setNames(rep(4, 4), LETTERS[1:4]) )
expect_identical( rowApply(m, sum), setNames(rep(4, 4), letters[1:4]) )

expect_identical( 
  colApply(m, sum, drop=FALSE), 
  `colnames<-`(matrix( rep(4,4), ncol=4 ), LETTERS[1:4])
)

expect_identical( 
  rowApply(m, sum, drop=FALSE), 
  `rownames<-`(matrix( rep(4,4), nrow=4 ), letters[1:4])
)
