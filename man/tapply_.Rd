\name{tapply_}
\alias{tapply_}
\title{Faster tapply}
\usage{
tapply_(X, INDEX, FUN = NULL, ..., simplify = TRUE)
}
\arguments{
  \item{X}{An atomic vector.}

  \item{INDEX}{A vector coercable to factor; must be one of
  the common atomic types: factor, integer, numeric, or
  character.}

  \item{FUN}{The function to be applied. See more details
  at \code{\link{lapply}}.}

  \item{...}{Optional arguments to pass to \code{FUN}.}

  \item{simplify}{boolean; if \code{TRUE}, we unlist the
  output and hence return a named vector of values.}
}
\description{
This function acts as a faster version of \code{tapply} for
the common case of splitting an atomic vector by another
atomic vector, and then applying a function.
}
\examples{
x <- rnorm(100)
gp <- sample( 1:10, 100, TRUE )
stopifnot( all(
  tapply(x, gp, mean) == tapply_(x, gp, mean)
) )
}

