\name{re.exists}
\alias{re.exists}
\alias{re_exists}
\title{Check whether Regular Expression was Found}
\usage{
  re.exists(pattern, x, perl = TRUE, ...)

  re_exists(pattern, x, perl = TRUE, ...)
}
\arguments{
  \item{pattern}{regex pattern passed to \code{grep}.}

  \item{x}{vector on which we attempt to match
  \code{pattern} on.}

  \item{perl}{boolean. use perl-compatible regular
  expressions?}

  \item{...}{additional arguments passed to \code{
  \link{grep} }.}
}
\description{
  Checks whether a match was found for a given regular
  expression in a vector. See examples for usage.
}
\examples{
if( re.exists("^ap", c("apple", "banana") ) ) print("yay!")
}

