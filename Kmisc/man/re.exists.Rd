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
  Checks whether a match was found for a given regular
  expression in a vector. See examples for usage.
}
\examples{
if( re.exists("^ap", c("apple", "banana") ) ) print("yay!")
}
\seealso{
  \code{\link{lg}}
}

