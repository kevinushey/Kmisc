library(testthat)
library(reshape2)
library(microbenchmark)
library(Kmisc)

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
expect_identical( melt_(dat, id.vars=c("x", "y")), melt_(dat, measure.vars=c("za", "zb", "zc")) )
expect_identical( 
  names( melt_(dat, id.vars=c("x", "y"), variable.name="vars", value.name="vals") ),
  c("x", "y", "vars", "vals")
)

for( i in 1:ncol(tmp1) ) {
  stopifnot( all( tmp1[,i] == tmp2[,i] ) )
}

microbenchmark(
  melt( dat, c("x", "y") ),
  melt_( dat, c("x", "y") ),
  times=10
)

dat$x <- as.factor( dat$x )
expect_warning( tmp <- melt_(dat, c("x", "y")) )

dat$zc <- as.integer( dat$zc )
expect_warning( tmp <- melt_(dat, c("x", "y")) )

dat$zb <- as.character( dat$zb )
expect_warning( tmp <- melt_(dat, c("x", "y")) )

dat$zb <- sample( c(TRUE, FALSE), nrow(dat), replace=TRUE )
expect_warning( tmp <- melt_(dat, c("x", "y")) )

## testing new args
suppressWarnings(
  expect_identical( melt_(dat, id.vars=c("x", "y")), melt_(dat, measure.vars=c("za", "zb", "zc")) )
)

## using row.names
df <- melt_(dat, "row.names")
df2 <- melt(dat, measure.vars=1:ncol(dat))
expect_identical( as.character( df2[,1] ), df[,1] )
expect_identical( df[,2], df2[,2] )

rm( list=ls() )

## matrix tests
x <- matrix(1:24, nrow=4)
rownames(x) <- 1:4
colnames(x) <- letters[1:6]
melt_(x)
melt(x)
expect_true( all( melt(x) == melt_(x) ) )

rownames(x) <- NULL
expect_true( all( melt_(x) == melt(x) ) )
colnames(x) <- NULL
expect_true( all( melt_(x) == melt(x) ) )

x <- matrix( letters[1:16], nrow=2)
expect_true( all( melt(x) == melt_(x) ) )

microbenchmark( melt(x), melt_(x) )

x <- matrix( rnorm(1E5), ncol=1E2 )
expect_true( all( melt(x) == melt_(x) ) )
microbenchmark( melt(x), melt_(x), times=5 )

