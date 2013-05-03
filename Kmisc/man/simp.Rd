\name{simp}
\alias{simp}
\title{Area Under the Curve with Simpson's Rule}
\usage{
  simp(x, y)
}
\arguments{
  \item{x}{A vector of values \code{x}.}

  \item{y}{A vector of values \code{f(x)}.}

  \item{n}{The number of points at which to evaluate and
  interpolate over for computing the area under the curve.
  Must be an even number.}
}
\description{
  This function computes the area under the curve using
  Simpson's (composite) rule, for a function \code{f(x)}
  evaluated over equally spaced points \code{x}.
}

