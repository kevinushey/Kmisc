\name{size}
\alias{size}
\title{Print the Object Size, with Auto Units}
\usage{
size(x, quote = FALSE, units = "auto", ...)
}
\arguments{
  \item{x}{An \R object.}

  \item{quote}{logical, indicating whether or not the
  result should be printed with surrounding quotes.}

  \item{units}{The units to be used in printing the size.
  Other allowed values are \code{"Kb"}, \code{"Mb"},
  \code{"Gb"} and \code{"auto"}. See
  \code{\link{object.size}} for more details.}

  \item{...}{Arguments to be passed to or from other
  methods.}
}
\description{
Provides an estimate of the memory that is being used to
store an \R object. Similar to \code{\link{object.size}},
but we set \code{units="auto"} as default.
}

