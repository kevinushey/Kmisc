\name{labeller}
\alias{labeller}
\title{ggplot2 labeller}
\usage{
  labeller(..., .parse = TRUE)
}
\arguments{
  \item{...}{A set of named arguments.}

  \item{.parse}{boolean; if \code{TRUE} we \code{parse}
  the}
}
\description{
  This function works as a labelling mapper for
  \code{ggplot2}, typically used in \code{facet_grid}. All
  arguments must be named. Items are mapped as \code{name
  => value}, where \code{name} represents the original
  levels of the factor used for facetting.
}
\examples{
if( require(ggplot2) ) {

  df <- data.frame(
    x=1:100,
    y=rnorm(100),
    grp=rep( c("tau+", "tau-"), each=50 ) ## levels are "tau+", "tau-"
  )

  f <- labeller(
    `tau-` = 'tau["-"]',
    `tau+` = 'tau["+"]'
  )

  ggplot(df, aes(x=x, y=y)) +
    geom_point() +
    facet_grid(". ~ grp", labeller=f)

  df$grp2 <- factor(rep( c("beta+", "beta-"), each=50 ))

  f <- labeller(
    `tau-` = 'tau["-"]',
    `tau+` = 'tau["+"]',
    `beta+` = 'beta["+"]',
    `beta-` = 'beta["-"]'
  )

  ggplot(df, aes(x=x, y=y)) +
    geom_point() +
    facet_grid("grp ~ grp2", labeller=f)
}
}

