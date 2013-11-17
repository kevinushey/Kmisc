\name{kFivenum}
\alias{kFivenum}
\title{Fivenum with Names}
\usage{
kFivenum(x, na.rm = TRUE)
}
\arguments{
  \item{x}{numeric, maybe including \code{NA}s and
  \code{Inf}s.}

  \item{na.rm}{logical. remove NAs?}
}
\value{
  \code{data.frame} version of five number summary
}
\description{
  A wrapper to \code{stats::fivenum} that also produces
  variable names.
}
\seealso{
  \code{\link{fivenum}}
}

