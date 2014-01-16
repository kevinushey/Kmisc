
## ----, tidy=FALSE--------------------------------------------------------
set.seed(123)
library(data.table)
library(Kmisc)
library(lattice)
library(grid)
library(Rcpp)
library(knitr)
library(rbenchmark)
dat <- data.frame( x=letters[1:4], y=1:4, z=LETTERS[1:4] )
opts_chunk$set(
  results="markup"
)


## ----, tidy=FALSE--------------------------------------------------------
## let's remove columns 'x' and 'z' from dat.
tryCatch( dat[ -c('x', 'z') ], error=function(e) print(e$message) )
## oh :(
dat[ !(names(dat) %in% c('x', 'z')) ]
## I always find that a bit awkward. Let's use Kmisc's without instead.
without(dat, x, z)


## ----tidy=FALSE----------------------------------------------------------
extract(dat, x, y)


## ----tidy=FALSE----------------------------------------------------------
re_extract(dat, "[xy]")
re_without(dat, "[xy]")


## ----tidy=FALSE----------------------------------------------------------
tDat <- dat ## make a temporary copy of dat

## Replace some elements in tDat$y
tDat$y <- swap( tDat$y, from=c(2, 4), to=c(20, 40) )
cbind( dat$y, tDat$y )


## ----tidy=FALSE----------------------------------------------------------
bDat <- data.frame( x=rnorm(10), y=sample(letters,10), z=sample(letters,10) )
str( bDat )
str( factor_to_char(bDat) )


## ----, tidy=FALSE--------------------------------------------------------
dat <- data.frame( x = rnorm(100), y = rnorm(100), z = rnorm(100) )
dapply( dat, summary )


## ----, tidy=FALSE--------------------------------------------------------
dat1 <- data.frame( id=5:1, x=c("a","a","b","b","b"), y=rnorm(5) )
dat2 <- data.frame( id=c(1, 2, 4), z=rnorm(3) )

## default merge changes id order
merge( dat1, dat2, by="id", all.x=TRUE )
## even the sort parameter can't save you
merge( dat1, dat2, by="id", all.x=TRUE, sort=TRUE )
# kMerge keeps it as is
kMerge( dat1, dat2, by="id" )


## ----tidy=FALSE----------------------------------------------------------
x <- runif(10)*10; lo <- 5; hi <- 10
print( data.frame( x=x, between_5_and_10=in_interval(x, lo, hi) ) )


## ----tidy=FALSE----------------------------------------------------------
dfs <- replicate(1E3,
  data.frame(x=rnorm(10), y=sample(letters,10), z=sample(LETTERS,10)),
  simplify=FALSE
)
str( stack_list(dfs) )
system.time( stack_list(dfs) )
system.time( do.call(rbind, dfs) )
system.time( data.table::rbindlist(dfs) )


## ----tidy=FALSE----------------------------------------------------------
l <- replicate(5, rnorm(5), simplify=FALSE)
invisible(list2df(l, inplace=TRUE))
class(l)

## see also df2list, mat2df, df2mat


## ----tidy=FALSE----------------------------------------------------------
str_rev( c("ABC", "DEF", NA, paste(LETTERS, collapse="") ) )
str_rev2( c("はひふへほ", "abcdef") )


## ----tidy=FALSE----------------------------------------------------------
str_slice( c("ABCDEF", "GHIJKL", "MNOP", "QR"), 2 )
str_slice2( "ハッピー", 2 )


## ----, tidy=FALSE--------------------------------------------------------
str_sort("asnoighewgypfuiweb")


## ----, tidy=FALSE--------------------------------------------------------
str_collapse( c("ABC", "DEF", "GHI") )


## ----, tidy=FALSE--------------------------------------------------------
n <- 1E5
dat <- data.frame( y=rnorm(n), x=sample(letters[1:5], n, TRUE) )
tMean <- Rcpp_tapply_generator("return mean(x);")
with( dat, tMean(y, x) )
with( dat, tapply(y, x, mean) )
benchmark(
  Kmisc=with( dat, tMean(y, x) ),
  R=with( dat, tapply(y, x, mean) ),
  replications=5
)


## ----, tidy=FALSE--------------------------------------------------------
aMean <- Rcpp_apply_generator("return mean(x);")
mat <- matrix( rnorm(1E3), nrow=100 )
aMean(mat, 2)
apply(mat, 2, mean)
benchmark(
  Kmisc=aMean(mat, 2),
  R=apply(mat, 2, mean)
)


## ----, tidy=FALSE--------------------------------------------------------
y <- rnorm(1E5); x <- sample(letters[1:5], 1E5, TRUE)
tapply(y, x, mean)
tapply_(y, x, mean)
benchmark( replications=10,
  tapply(y, x, mean),
  tapply_(y, x, mean),
  tMean(y, x)
)


## ----, tidy=FALSE--------------------------------------------------------
dat <- data.frame(
  id=LETTERS[1:5],
  x1=rnorm(5),
  x2=rnorm(5),
  x3=rnorm(5)
)
print(dat)
melt_(dat, id.vars="id")


## ----, tidy=FALSE--------------------------------------------------------
lets <- sample(letters, 1E6, TRUE)
stopifnot( identical(
  factor_(lets),
  factor(lets)
) )
benchmark( replications=5,
  factor_(lets),
  factor(lets)
)


## ----, tidy=FALSE--------------------------------------------------------
df <- data.table(x=1, y=2)
str(df)
anatomy(df)


## ----results='asis', tidy=FALSE------------------------------------------
html(
  table( class="table table-bordered table-striped table-condensed table-hover", ## bootstrap classes
    tr(
      td("Apples"),
      td("Bananas")
    ),
    tr(
      td("20"),
      td("30")
    )
  )
)


