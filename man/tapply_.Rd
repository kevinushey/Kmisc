\name{tapply_}
\alias{tapply_}
\title{Faster tapply}
\usage{
tapply_(X, INDEX, FUN = NULL, FUN.VALUE = NULL, ..., simplify = TRUE,
  USE.NAMES = TRUE)
}
\arguments{
  \item{X}{An atomic vector.}

  \item{INDEX}{A vector coercable to factor; must be one of
  the common atomic types: factor, integer, numeric, or
  character.}

  \item{FUN}{The function to be applied. See more details
  at \code{\link{lapply}}.}

  \item{FUN.VALUE}{Optional; if specified we try to
  leverage \code{vapply} for computation of results.}

  \item{...}{Optional arguments to pass to \code{FUN}.}

  \item{simplify}{boolean; if \code{TRUE}, we unlist the
  output and hence return a named vector of values.}

  \item{USE.NAMES}{boolean; if \code{TRUE} use \code{X} as
  \code{\link{names}} for the result unless it had names
  already. Sometimes, one can achieve substantial speedups
  by setting this to \code{FALSE}. This option is only used
  when \code{FUN.VALUE} is not \code{NULL}.}
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

