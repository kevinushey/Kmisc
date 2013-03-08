\name{melt_}
\alias{melt_}
\title{Melt a Data Frame}
\usage{
  melt_(data, id.vars)
}
\arguments{
  \item{data}{The \code{data.frame} to melt.}

  \item{id.vars}{Vector of id variables. Can be integer
  (variable index) or string (variable name). All variables
  not included here are assumed stackable.}
}
\description{
  Inspired by \code{reshape2::melt.data.frame}, this
  function will melt a \code{data.frame} for the special
  case in which we have \code{n} identification variables
  but only 1 value variable. This function is built for
  speed and can melt large \code{data.frame}s faster than
  \code{reshape2::melt}; however, the benefit is only seen
  for \code{data.frame} that are at least 1 million rows
  and larger.
}
\examples{
n <- 20
tmp <- data.frame( stringsAsFactors=FALSE,
  x=sample(letters, n, TRUE),
  y=sample(LETTERS, n, TRUE),
  za=rnorm(n),
  zb=rnorm(n),
  zc=rnorm(n)
)

out2 <- melt_(tmp, c("x", "y"))
}

