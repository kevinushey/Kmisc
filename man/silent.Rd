\name{silent}
\alias{silent}
\title{Hide All Potential Output from an Expression}
\usage{
silent(expr, envir = parent.frame(1), enclos = baseenv())
}
\arguments{
  \item{expr}{An \R expression.}
}
\description{
This function combines hides any output a function might
try to write to \code{stdout}, \code{stderr}, to the \R
console as a message, warning, and so on.
}

