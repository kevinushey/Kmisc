\name{pad}
\alias{pad}
\title{Pad an Object with NAs}
\usage{
pad(x, n)
}
\arguments{
  \item{x}{An \R object (list, data.frame, matrix, atomic
  vector).}

  \item{n}{The final length of each object.}
}
\description{
  This function pads an \R object (list, data.frame,
  matrix, atomic vector) with \code{NA}s. For matrices,
  lists and data.frames, this occurs by extending each
  (column) vector in the object.
}

