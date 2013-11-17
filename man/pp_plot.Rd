\name{pp_plot}
\alias{pp_plot}
\title{Construct a Probability-Probability Plot from a Set of P-Values}
\usage{
pp_plot(x, ...)
}
\arguments{
  \item{x}{A vector of p-values; numbers within the range 0
  to 1.}

  \item{...}{Optional arguments passed to
  \code{\link{xyplot}}. Note that a custom panel function
  is used for generating the plot and hence you shouldn't
  try to generate your own panel function.}
}
\description{
  This function constructs a probability-probability plot
  as based on a vector of p-values.
}
\examples{
pp_plot( runif(100), main="PP-Plot of 100 random uniforms" )
}

