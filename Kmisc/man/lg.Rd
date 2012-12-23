\name{lg}
\alias{lg}
\title{length( grep( ... ) )}
\usage{
  lg(...)
}
\arguments{
  \item{...}{passed to \code{\link{grep}}.}
}
\description{
  This is a wrapper to a commonly used R idiom. See
  examples for usage.
}
\examples{
x <- c("apple", "banana", "cherry")
if( lg( "^ap", x ) > 0 ) {
  print( "regular expression '^ap' found in 'x'" )
  }
}
\seealso{
  \code{\link{re.exists}}
}

