library(Kmisc)
library(testthat)

n <- 1E4

dat <- data.frame( x=sample(LETTERS, n, TRUE), y=rnorm(n), stringsAsFactors=FALSE )
tempfile <- tempfile()
write.table( dat,
             file=tempfile,
             row.names=FALSE,
             col.names=FALSE,
             sep="\t",
             quote=FALSE
             )

outPath <- file.path( dirname(tempfile), "split" )
list.files(outPath)
for( file in list.files(outPath, full.names=TRUE) ) {
  unlink(file)
}

split_file( tempfile, 
            column=1, 
#            sep="\t", 
            outDir=outPath,
            verbose=FALSE
            )

out <- NULL
for( file in list.files(outPath, full.names=TRUE) ) {
  tmp <- read.table( file, header=FALSE, sep="\t", as.is=TRUE, colClasses=c("character", "numeric") )
  out <- rbind( out, tmp )
}
names(out) <- c("x", "y")
dat_ordered <- dat[ order(dat$x), ]
rownames(dat_ordered) <- 1:nrow(dat_ordered)

expect_equal( dat_ordered, out )

system.time( 
  split_file( tempfile, 
              column=1, 
              sep="\t", 
              outDir=outPath,
              verbose=FALSE
  )
)

for( file in list.files(outPath, full.names=TRUE) ) {
  unlink(file)
}
