\name{grid.text2}
\alias{grid.text2}
\title{Grid Text with a Background}
\usage{
  grid.text2(label, x = unit(0.5, "npc"),
    y = unit(0.5, "npc"), just = "centre", hjust = NULL,
    vjust = NULL, check.overlap = FALSE,
    default.units = "npc", name = NULL,
    gp = gpar(col = "black", fill = "grey92", lineend = "butt", linejoin = "round"),
    draw = TRUE, vp = NULL, widthAdj = unit(0.05, "npc"),
    heightAdj = unit(0.05, "npc"))
}
\arguments{
  \item{label}{A character or \code{\link{expression}}
  vector. Other objects are coerced by
  \code{as.graphicsAnnot}.}

  \item{x}{A numeric vector or unit object specifying
  x-values.}

  \item{y}{A numeric vector or unit object specifying
  y-values.}

  \item{just}{The justification of the text relative to its
  (x, y) location. If there are two values, the first value
  specifies horizontal justification and the second value
  specifies vertical justification. Possible string values
  are: \code{"left"}, \code{"right"}, \code{"centre"},
  \code{"center"}, \code{"bottom"}, and \code{"top"}. For
  numeric values, 0 means left alignment and 1 means right
  alignment.}

  \item{hjust}{A numeric vector specifying horizontal
  justification. If specified, overrides the \code{just}
  setting.}

  \item{vjust}{A numeric vector specifying vertical
  justification. If specified, overrides the \code{just}
  setting.}

  \item{check.overlap}{A logical value to indicate whether
  to check for and omit overlapping text.}

  \item{default.units}{A string indicating the default
  units to use if \code{x} or \code{y} are only given as
  numeric vectors.}

  \item{name}{A character identifier.}

  \item{gp}{An object of class \code{gpar}, typically the
  output from a call to the function \code{gpar}. This is
  basically a list of graphical parameter settings.}

  \item{draw}{A logical value indiciating whether graphics
  output should be produced.}

  \item{vp}{A Grid viewport object (or \code{NULL}).}

  \item{widthAdj}{A width adjustment parameter, to help
  control how much horizontal padding there should be
  between the text and the background rectangle.}

  \item{heightAdj}{A height adjustment parameter, to help
  control how much vertical padding there should be between
  the text and the background rectangle.}
}
\description{
  This function produces text as does \code{grid.text}, but
  also generates a background rectangle through
  \code{grid.rect}. Helpful for plotting e.g. overlaying
  correlation statistics on a plot, where you'd like the
  element to stand out a little more.
}
\examples{
x <- rnorm(10)
y <- rnorm(10)
if (require(lattice)) xyplot( y ~ x,
        panel = function(x, y, ...) {
          panel.xyplot(x, y, ...)
          grid.text2("some text\\nwith a nice\\nbackground")
          grid.text2( expression( sum(x[i], i==1, n)^2 ), x=0.8, y=0.8 )
          grid.text2( paste("sum of rnorm(10): ", sum(rnorm(10)) ), x=0.2, y=0.2, just="left" )
          grid.text2( "horizontal justifications work too", x=0.95, y=0.35, just="right" )
        })
## will work for multi-panel plots as well
}
\seealso{
  \code{\link{grid.text}} and \code{\link{grid.rect}}
}

