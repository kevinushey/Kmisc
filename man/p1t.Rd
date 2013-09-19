\name{p1t}
\alias{p1t}
\title{Make 1D HTML Table}
\usage{
  p1t(x,
    class = "table table-condensed table-striped table-hover",
    id = NULL, ...)
}
\arguments{
  \item{x}{a \code{data.frame}, typically output of
  \code{kTable}.}

  \item{class}{class to be passed to HTML table; used for
  CSS styling.}

  \item{id}{id to be passed to HTML table; used for CSS
  styling.}

  \item{...}{optional arguments passed to
  \code{\link{makeHTMLTable}}.}
}
\description{
  This tabling function is intended for the output of
  \code{kTable}, as generated when only one 'data' argument
  is passed.
}
\examples{
y <- factor( rbinom( 100, 2, 0.2 ) )
p1t( kTable( y ) )
}
\seealso{
  \code{\link{kTable}}, \code{\link{makeHTMLTable}}
}

