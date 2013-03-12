library(testthat)
library(reshape2)
library(microbenchmark)

n <- 1E4
dat <- data.frame( stringsAsFactors=FALSE,
                   x=sample(letters, n, TRUE), 
                   y=sample(LETTERS, n, TRUE),
                   za=rnorm(n), 
                   zb=rnorm(n), 
                   zc=rnorm(n)
)

tmp1 <- melt( dat, c("x", "y") )
tmp2 <- melt_(dat, c("x", "y") )

for( i in 1:ncol(tmp1) ) {
  stopifnot( all( tmp1[,i] == tmp2[,i] ) )
}

microbenchmark(
  melt( dat, c("x", "y") ),
  melt_( dat, c("x", "y") ),
  times=1
)

dat$x <- as.factor( dat$x )
expect_warning( tmp <- melt_(dat, c("x", "y")) )

dat$zc <- as.integer( dat$zc )
expect_warning( tmp <- melt_(dat, c("x", "y")) )
dat$zb <- as.character( dat$zb )
expect_warning( tmp <- melt_(dat, c("x", "y")) )
dat$zb <- sample( c(TRUE, FALSE), nrow(dat), replace=TRUE )
expect_warning( tmp <- melt_(dat, c("x", "y")) )
