library(testthat)
library(reshape2)
library(Kmisc)
library(microbenchmark)

n <- 1E2
dat <- data.frame( stringsAsFactors=FALSE,
  x=factor(sample(letters, n, TRUE)), 
  y=sample(LETTERS, n, TRUE),
  za=rnorm(n),
  zb=as.integer(rnorm(n)),
  zc=as.character(rnorm(n)),
  zd=as.factor(rnorm(n))
)

old_dat <- duplicate(dat)

gctorture(TRUE)
tempfile <- tempfile()
sink( tempfile )

for( i in 1:10 ) {
  
  tmp1 <- .Call(Kmisc:::Cmelt_dataframe, dat, (1:2)-1L, (3:6)-1L, "variable", "value", PACKAGE="Kmisc")
  tmp2 <- melt_(dat, c("x", "y"))
  
  stopifnot( identical(tmp1, tmp2) )
  stopifnot( identical(dat, old_dat) )
  
}

sink()
gctorture(FALSE)
print( readlines(tempfile) )
file.remove(tempfile)

Cmelt_dataframe <- Kmisc:::Cmelt_dataframe

microbenchmark( times=100, 
  c1=.Call(Cmelt_dataframe, dat, (1:2)-1L, (3:6)-1L, "variable", "value", PACKAGE="Kmisc"),
  c2=.Call("melt_dataframe", dat, (1:2)-1L, (3:6)-1L, "variable", "value", PACKAGE="Kmisc"),
  c3=melt_(dat, c("x", "y"))
)