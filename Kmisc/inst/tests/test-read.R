library(testthat)
library(Kmisc)

dat <- as.data.frame( replicate(10, rnorm(100), simplify=FALSE) )
tempfile <- tempfile()
write.table(dat, file=tempfile)

tmp1 <- readLines(tempfile)
tmp2 <- readlines(tempfile)
stopifnot( identical(tmp1, tmp2) )

tmp3 <- read(tempfile)
stopifnot( identical( tmp2, unlist(strsplit(tmp3, "\n", fixed=TRUE)) ) )
unlink(tempfile)
