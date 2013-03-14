\name{melt_}
\alias{melt_}
\alias{melt_.data.frame}
\alias{melt_.matrix}
\title{Melt a Data Frame}
\usage{
  melt_(data, id.vars)

  \method{melt_}{data.frame} (data, id.vars)

  \method{melt_}{matrix} (data, id.vars)
}
\arguments{
  \item{data}{The \code{data.frame} to melt.}

  \item{id.vars}{Vector of id variables. Can be integer
  (variable index) or string (variable name). All variables
  not included here are assumed stackable, and will be
  coerced as needed. Only used for \code{data} of class
  \code{data.frame}.}
}
\description{
  Inspired by \code{reshape2:::melt}, we melt
  \code{data.frame}s for the special case in which we have
  multiple id variables and a single value variable, and
  \code{matrix}s. This function is built for speed.
}
\details{
  If items to be stacked are not of the same internal type,
  they will be promoted in the order \code{logical} >
  \code{integer} > \code{numeric} > \code{character}.
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

