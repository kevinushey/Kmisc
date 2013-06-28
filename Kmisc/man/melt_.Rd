\name{melt_}
\alias{melt_}
\alias{melt_.data.frame}
\alias{melt_.matrix}
\title{Make a 'Wide' data set 'Long'}
\usage{
  melt_(data, ...)

  \method{melt_}{data.frame} (data, id.vars, measure.vars,
    variable.name = "name", ..., value.name = "value")

  \method{melt_}{matrix} (data, ...)
}
\arguments{
  \item{data}{The \code{data.frame} to melt.}

  \item{...}{Arguments passed to other methods.}

  \item{id.vars}{Vector of id variables. Can be integer
  (variable position) or string (variable name). If blank,
  we use all variables not in \code{measure.vars}.}

  \item{measure.vars}{Vector of measured variables. Can be
  integer (variable position) or string (variable name). If
  blank, we use all variables not in \code{id.vars}.}

  \item{variable.name}{Name of variable used to store
  measured variable names.}

  \item{value.name}{Name of variable used to store values.}
}
\description{
  Inspired by \code{reshape2:::melt}, we melt
  \code{data.frame}s and \code{matrix}s. This function is
  built for speed.
}
\details{
  If items to be stacked are not of the same internal type,
  they will be promoted in the order \code{logical} >
  \code{integer} > \code{numeric} > \code{character}.

  See \code{\link{melt_.data.frame}} and
  \code{melt_.matrix} for the S3 dispatched functions.
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

