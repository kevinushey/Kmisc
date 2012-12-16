\name{us}
\alias{us}
\title{unlist( strsplit( ... ) )}
\usage{
  us(x, split = "", ...)
}
\arguments{
  \item{x}{vector of items, as passed to
  \code{\link{strsplit}}}

  \item{split}{the delimiter to split on}

  \item{...}{optional arguments passed to strsplit}
}
\description{
  This is a thin wrapper to \code{ unlist( strsplit( ... )
  ) }.
}
\examples{
x <- "apple_banana_cherry"
us(x, "_")
}
\seealso{
  \code{\link{unlist}}, \code{\link{strsplit}}
}

