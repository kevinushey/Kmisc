\name{matches}
\alias{matches}
\title{Count Matches}
\usage{
matches(...)
}
\arguments{
  \item{...}{A set of (possibly named) arguments, all of
  the same type.}
}
\description{
This function returns a matrix of matches between each
argument passed. Each cell \code{x_ij} in the output
denotes how many times the elements in input \code{i} were
found in input \code{j}.
}
\examples{
x <- c("a", "b", "c", "d")
y <- c("a", "b", "c")
z <- c("a", "b", "d")
matches(x, y, z)
}

