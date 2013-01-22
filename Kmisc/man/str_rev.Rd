\name{str_rev}
\alias{str_rev}
\title{Reverse a Vector of Strings}
\usage{
  str_rev(x)
}
\arguments{
  \item{x}{a character vector.}
}
\description{
  Reverses a vector of 'strings' (a character vector). Not
  safe for unicode (UTF-8) characters.
}
\details{
  This function is written in C for fast execution;
  however, we do not handle non-ASCII characters. For a
  'safe' version of \code{str_rev} that handles unicode
  characters, see \code{\link{str_rev2}}.
}
\examples{
x <- c("ABC", "DEF", "GHIJ")
str_rev(x)
}
\seealso{
  \code{\link{str_rev2}}
}

