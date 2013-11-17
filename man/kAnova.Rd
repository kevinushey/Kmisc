\name{kAnova}
\alias{kAnova}
\title{Nicely Formatted ANOVA Table}
\usage{
kAnova(fit, test = "LRT", swap.periods = TRUE)
}
\arguments{
  \item{fit}{the model fit to generate an ANOVA table for.}

  \item{test}{the type of test to perform. default is
  likelihood-ratio test (LRT).}

  \item{swap.periods}{swap periods with spaces?}
}
\description{
  Returns a nicely formatted ANOVA table. See
  \code{\link{kCoef}} for other details.
}
\examples{
x <- rnorm(100)
y <- ifelse( x + runif(100) > 1, 1, 0 )
myFit <- glm( y ~ x, family="binomial" )
kAnova( myFit )
}

