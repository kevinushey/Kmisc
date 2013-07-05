library(testthat)
library(reshape2)
library(microbenchmark)

rm( list=ls() )
gctorture(TRUE)
tempfile <- tempfile()
sink( tempfile )

for( i in 1:10 ) {
  
  n <- 1E2
  dat <- data.frame( stringsAsFactors=FALSE,
                     x=sample(letters, n, TRUE), 
                     y=sample(LETTERS, n, TRUE),
                     za=rnorm(n), 
                     zb=rnorm(n), 
                     zc=rnorm(n)
  )
  
  tmp1 <- melt(dat, c("x", "y"))
  tmp2 <- melt_(dat, c("x", "y"))
  
  stopifnot( all( tmp1[1:3] == tmp2[1:3]) )
  
}

sink()
gctorture(FALSE)
file.remove(tempfile)