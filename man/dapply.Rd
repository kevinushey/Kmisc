\name{dapply}
\alias{dapply}
\title{Apply a Function over a List}
\usage{
dapply(X, FUN, ...)
}
\arguments{
  \item{X}{a vector, expression object, or a
  \code{data.frame}}

  \item{FUN}{function to be applied to each element of
  \code{X}.}

  \item{...}{optional arguments to \code{FUN}.}
}
\description{
  A convenience function that works like \code{lapply}, but
  coerces the output to a \code{data.frame} if possible. We
  set \code{stringsAsFactors=FALSE}, and
  \code{optional=TRUE}, to minimize the amount of automatic
  coersion \R might try to do.
}
\details{
  This function is preferable to \code{\link{sapply}} or
  \code{\link{lapply}} when you explicitly want a data
  frame returned.
}
\examples{
dat <- data.frame(
  x = rnorm(10),
  y = rnorm(10)
)

## Calculate 0.025, 0.975 quantiles for each column in a data.frame,
## and return result as data.frame .
dapply( dat, function(x) {
  quantile(x, c(0.025, 0.975))
} )

dapply( dat, summary )
str( dapply( dat, summary ) )
}
\seealso{
  \code{\link{lapply}}, \code{\link{lapply}}
}

