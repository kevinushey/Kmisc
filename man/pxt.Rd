\name{pxt}
\alias{pxt}
\title{Make 2x2 HTML Contingency Table}
\usage{
pxt(x, class = "twoDtable", id = NULL, ...)
}
\arguments{
  \item{x}{a 2x2 table; typically something returned from
  \code{kTable(x,y)}}

  \item{class}{class to be passed to HTML table; used for
  CSS styling.}

  \item{id}{id to be passed to HTML table; used for CSS
  styling.}

  \item{...}{optional arguments passed to
  \code{\link{makeHTMLTable}}.}
}
\description{
  Function for outputting cross-tabulated tables as
  marked-up HTML. CSS styling can be used to make these
  tables look especially nice.
}
\examples{
x <- rbinom( 100, 2, 0.2 )
y <- rbinom( 100, 2, 0.2 )
pxt( kTable(x, y) )
}
\seealso{
  \code{\link{kTable}}
}

