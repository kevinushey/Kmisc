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

## what if we have no id.vars?
expect_warning(df <- melt_(dat, m=1:ncol(dat)))
df2 <- melt(dat, measure.vars=1:ncol(dat))
expect_identical( as.character( df2[,1] ), df[,1] )
expect_identical( df[,2], df2[,2] )

df <- melt(dat, i=NULL)
expect_warning(df2 <- melt_(dat, i=NULL))
expect_identical( factor_to_char(df), df2 )

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
## see: issue #2
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

expect_identical(
  melt_(DF, m=c("z", "y")),
  factor_to_char(melt(DF, m=c("z", "y")))
)

## see: issue #3
DF <- structure(list(x1 = 1:5, x2 = 6:10, x3 = 11:15, x4 = structure(c(2L, 
  1L, 5L, 3L, 4L), .Label = c("f", "j", "m", "q", "r"), class = "factor"), 
  x5 = structure(c(15950, 15951, 15952, 15953, 15954), class = "Date"), 
  x7 = structure(c(15345, 15344, 15343, 15342, 15341), class = "Date"), 
  x8 = c("g", "b", "h", "y", "m")), .Names = c("x1", "x2", 
    "x3", "x4", "x5", "x7", "x8"), class = "data.frame", row.names = c(NA, 
      -5L))

tmp1 <- melt_(DF, id=c("x1", "x4"), measure="x2")
tmp2 <- melt(DF, id=c("x1", "x4"), measure="x2")
tmp2$variable <- as.character(tmp2$variable)
expect_identical(tmp1, tmp2)

expect_error(tmp3 <- melt_(DF, id=c("x1", "x4"), measure=c("x5", "x6")))
expect_error(tmp4 <- melt(DF, id=c("x1", "x4"), measure=c("x5", "x6")))

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
