\name{re.exists}
\alias{re.exists}
\title{Check whether Regular Expression was Found}
\usage{
  re.exists(...)
}
\arguments{
  \item{...}{passed to \code{ \link{grep} }.}
}
\description{
  A tighter wrapper of a commonly used R idiom. See
  examples for usage.
}
\examples{
if( re.exists("^ap", c("apple", "banana") ) ) print("yay!")
}
\seealso{
  \code{\link{lg}}
}

