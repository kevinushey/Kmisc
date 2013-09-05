library(Kmisc)
library(testthat)

tmp <- tempfile(fileext=".txt")
out <- paste0( strip_extension(tmp), "_sub", ".txt" )
dat <- data.frame( x=rnorm(100), y=rep(letters[1:5], each=20), stringsAsFactors=FALSE )
write.table( dat,
             file=tmp,
             row.names=FALSE,
             col.names=FALSE,
             sep="\t",
             quote=FALSE
             )

extract_rows_from_file( tmp, out, column=2, keep="a" )
extract_rows_from_file( tmp, out, column=2, sep="\t", keep="a")
dat_sub <- read.table( out, sep="\t", header=FALSE, as.is=TRUE )
names(dat_sub) <- c("x", "y")

expect_true( all.equal( dat_sub, dat[ dat$y == "a", ] ) )

extract_rows_from_file( tmp, out, column=2, sep="\t", keep=c("a","e") )
dat_sub <- read.table( out, header=FALSE, as.is=TRUE )
names(dat_sub) <- c("x", "y")

expect_true( all.equal( dat_sub$x, dat$x[ dat$y %in% c("a", "e") ] ) )

dat <- str_split( extract_rows_from_file( tmp, column=2, sep="\t", keep=c("a", "e") ),
                  "\t" )
expect_true( all( dat[,2] == dat_sub[,2] ) )
