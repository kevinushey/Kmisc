library(Kmisc)
library(testthat)

dat <- data.frame(x=c(1, 2, 3), y=c('a', 'b', 'c'), z=c(10L, 11L, 12L))
file <- tempfile()
file2 <- paste(file, "_sub", sep='')
write.table(dat, file=file,
            row.names=FALSE,
            col.names=FALSE,
            sep="\t",
            quote=FALSE
)

dat2 <- read.table( text=awk('print $0', file=file) )
expect_true( all(dat == dat2) )

awk('print $1', file=file, out=file2)
dat3 <- read.table( file2, header=FALSE )
expect_true( all(dat[,1] == dat3[,1]) )
