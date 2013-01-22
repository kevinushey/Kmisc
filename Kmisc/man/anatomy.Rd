\name{anatomy}
\alias{anatomy}
\title{Display the Anatomy of a Data Frame}
\usage{
  anatomy(df, n = 3)
}
\arguments{
  \item{df}{an object inheriting class \code{data.frame}.}

  \item{n}{number of elements to print from each vector.}
}
\description{
  This function displays the 'anatomy' of a data frame. In
  practice, it's used to implement a faster version of
  \code{str} for data frames built entirely of atomic
  vectors, as \code{str.data.frame} is very slow for large
  data frames. If there are non-atomic vectors in
  \code{df}, we throw an error and instruct the user to
  check \code{str} (and ask why they have a data frame with
  non-atomic elements).
}
\examples{
local({
  bigDF <- as.data.frame( matrix( factor(1:1E3), nrow=1E3, ncol=1E3 ) )
  sink( tmp <- tempfile() )
  str <- system.time( str(bigDF, list.len=1E3) )
  anat <- system.time( anat(bigDF) )
  sink()
  unlink(tmp)
  print( rbind( str, anat ) )
})
}

