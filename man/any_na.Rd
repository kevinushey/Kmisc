\name{any_na}
\alias{any_na}
\title{Check whether there are any Missing Values in a Vector}
\usage{
any_na(x, how = "unlist")
}
\arguments{
  \item{x}{An \R object.}

  \item{how}{The simplification to use if \code{x} is a
  list. See \code{\link{rapply}} for more details.}
}
\description{
  This function checks whether there are any missing values
  in an \R object. For list objects, we recurse over each
  entry. This is typically faster than \code{any(is.na(x))}
  as we exit and return \code{TRUE} as soon as an \code{NA}
  is discovered.
}

