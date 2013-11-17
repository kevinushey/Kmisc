\name{gradient}
\alias{gradient}
\title{Generate Gradient from Continuous Variable}
\usage{
gradient(x, m = 10, cols = c("darkorange", "grey60", "darkblue"))
}
\arguments{
  \item{x}{a continuous variable to generate colors over.}

  \item{m}{the number of distinct colors you wish to pull
  from the pallete.}

  \item{cols}{the colors to interpolate over. passed to
  \code{\link{colorRampPalette}}.}
}
\description{
  Assign colors based on a continuous variable. Useful for
  plotting functions where you would like to generate a
  gradient based on (a function of) the continuous
  variables you are plotting quickly.
}
\examples{
dat <- data.frame(y=rnorm(100), x=rnorm(100))
with( dat, plot( y ~ x, col=gradient(y) ) )
}
\seealso{
  \code{\link{colorRampPalette}}
}

