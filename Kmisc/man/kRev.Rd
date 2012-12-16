\name{kRev}
\alias{kRev}
\title{Reverse Elements of a Character Vector}
\usage{
  kRev(x)
}
\arguments{
  \item{x}{a character vector.}
}
\value{
  Returns \code{x} with each element reversed.
}
\description{
  Use this function to reverse each element in a vector of
  characters (or a vector of items coercable to character).
}
\examples{
kRev( c("abc", "def") ) ## returns c("cba", "fed")
}

