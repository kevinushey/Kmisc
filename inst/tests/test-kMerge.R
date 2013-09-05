library( testthat )
set.seed(123)

n <- 10

x <- data.frame( id=n:1, y=rnorm(n), z=sample( letters, n, replace=TRUE ) )
y <- data.frame( id=sample(1:n), a=rnorm(n), b=sample( LETTERS, n, replace=TRUE ) )

dat <- kMerge( x, y )
dat2 <- kMerge( x, y, by.x="id" )
dat3 <- kMerge( x, y, by.y="id" )
dat4 <- kMerge( x, y, by="id" )

expect_identical( dat, dat2 )
expect_identical( dat, dat3 )
expect_identical( dat, dat4 )

expect_identical( dat[1:3], x )

xx <- data.frame( id=1:n, y=rnorm(n) )
expect_identical( kMerge( xx, y), merge(xx, y) )
