\name{str_split}
\alias{str_split}
\title{Split a Vector of Strings Following a Regular Structure}
\usage{
  str_split(x, sep, names = NULL, ...)
}
\arguments{
  \item{x}{a vector of strings.}

  \item{sep}{the delimiter / \code{\link{regex}} you wish
  to split your strings on.}

  \item{names}{optional: a vector of names to pass to the
  returned \code{data.frame}.}

  \item{...}{optional arguments passed to \code{strsplit}.}
}
\description{
  This function takes a vector of strings following a
  regular structure, and converts that vector into a
  \code{data.frame}, split on that delimiter. A nice
  wrapper to \code{\link{strsplit}}, essentially - the
  primary bonus is the automatic coersion to a
  \code{data.frame}.
}
\examples{
dat <- str_split(
  c("regular_structure", "in_my", "data_here"),
  sep="_",
  names=c("apple", "banana")
)
x <- c("somewhat_different.structure", "in_this.guy")
str_split( x, "[_\\\\.]" )
}
\seealso{
  \code{\link{strsplit}}
}

