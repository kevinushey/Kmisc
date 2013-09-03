library(microbenchmark)
library(testthat)
library(reshape2)
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

expect_identical( 
  melt_(dat, id.vars=c("x", "y")), 
  melt_(dat, measure.vars=c("za", "zb", "zc")) 
)

expect_identical( 
  names( melt_(dat, id.vars=c("x", "y"), variable.name="vars", value.name="vals") ),
  c("x", "y", "vars", "vals")
)

for( i in 1:ncol(tmp1) ) {
  stopifnot( all( tmp1[,i] == tmp2[,i] ) )
}
## check that melt_ handles NAs
dat$za[ sample(1:n, n/2) ] <- NA
dat$zb[ sample(1:n, n/2) ] <- NA
dat$zc[ sample(1:n, n/2) ] <- NA

expect_identical( melt_(dat, id.vars=c("x", "y")), melt_(dat, measure.vars=c("za", "zb", "zc")) )

dat$x <- as.factor( dat$x )
suppressWarnings( stopifnot( identical(
  factor_to_char(melt_(dat, c('x', 'y'))),
  factor_to_char(melt(dat, c('x', 'y')))
) ) )

all.equal(
  tmp1 <- melt_(dat, c('x', 'y')),
  tmp2 <- factor_to_char(melt(dat, c('x', 'y')))
)

dat$x <- as.character(dat$x)

dat$zc <- as.integer( dat$zc )
expect_warning( tmp <- melt_(dat, c("x", "y")) )
suppressWarnings( stopifnot( identical(
  melt_(dat, c('x', 'y')),
  factor_to_char(melt(dat, c('x', 'y')))
) ) )

dat$zb <- as.character( dat$zb )
expect_warning( tmp <- melt_(dat, c("x", "y")) )

suppressWarnings( stopifnot( identical(
  melt_(dat, c('x', 'y')),
  factor_to_char(melt(dat, c('x', 'y')))
) ) )

dat$zb <- sample( c(TRUE, FALSE), nrow(dat), replace=TRUE )
expect_warning( tmp <- melt_(dat, c("x", "y")) )
suppressWarnings( stopifnot( identical(
  melt_(dat, c('x', 'y')),
  factor_to_char(melt(dat, c('x', 'y')))
) ) )

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

## more tests
n <- 1E3
df <- data.frame( stringsAsFactors=FALSE,
  x=sample(letters, n, TRUE),
  y=factor(sample(letters, n, TRUE)),
  z=sample(LETTERS, n, TRUE)
)
df <- cbind(
  df,
  as.data.frame( replicate(100, rnorm(n), simplify=FALSE) )
)

names(df) <- c('x', 'y', 'z', paste0("V", 1:100))
tmp1 <- factor_to_char(melt_(df, id.vars=c('x', 'y', 'z')), inplace=TRUE)
tmp2 <- factor_to_char(melt(df, id.vars=c('x', 'y', 'z')), inplace=TRUE)
stopifnot( identical(tmp1, tmp2) )
microbenchmark( times=5,
  melt_=melt_(df, id.vars=c('x', 'y', 'z')),
  melt=melt(df, id.vars=c('x', 'y', 'z'))
)

## tests from Arun
DF <- data.frame(x=1:5, y=6:10, z=11:15)

m1 <- melt_(DF, id=c("y", "z")) # works!
m2 <- melt_(DF, id=c("z", "y")) # works!
m3 <- melt_(DF, id=c(1,2)) # works!
m4 <- melt_(DF, id=c(2,1)) # crashes (same for measure = .)

expect_error(melt_(DF, id=c(2, 1, 4)))
expect_error(melt_(DF, id=c(0)))
expect_identical(
  melt_(DF, id=c("z", "y")),
  factor_to_char(melt(DF, id=c("z", "y")))
)


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

m <- matrix(1:1E2, nrow=10)

gctorture(TRUE)
tmp <- melt_(m)
gctorture(FALSE)
tmp2 <- melt_(m)

expect_identical(tmp, tmp2)
