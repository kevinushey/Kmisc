\name{kMakeDummy}
\alias{kMakeDummy}
\title{Make Dummy Variables from Factor}
\usage{
  kMakeDummy(x)
}
\arguments{
  \item{x}{a variable coercable to \code{factor}.}
}
\description{
  This functions converts a single factor into dummy
  variables, with one dummy variable for each level of that
  factor. Names are constructed as
  \code{<varName>_<level>}.
}
\examples{
x <- factor( rep( c("a", "b", "c", "d"), each=25 ) )
kMakeDummy(x)
}

