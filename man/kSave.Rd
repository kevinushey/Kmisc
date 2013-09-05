\name{kSave}
\alias{kSave}
\title{Write out and Save a Tabular File}
\usage{
  kSave(x, file, lvl = 1, Rext = ".rda", ...)
}
\arguments{
  \item{x}{the R object you want to save / write to file}

  \item{file}{the location to write the file to, with
  extension desired for object written by write.table}

  \item{lvl}{how many extensions do you want to strip from
  your output file?}

  \item{Rext}{the extension to use for the saved object.}

  \item{...}{optional arguments passed to
  \code{write.table}}
}
\description{
  A function that both writes a file to table with
  \code{write.table}, and saves it with the same name but a
  separate file extension.
}
\examples{
dat <- data.frame( x=c(1,2,3), y=c('a','b','c') )
kSave( dat, file="dat.txt" )
## the file 'dat.rda' is written as well - let's see if it exists
dat2 <- kLoad( "dat.rda" )
stopifnot( identical(dat, dat2) ) ## TRUE
}

