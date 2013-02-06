\name{factor_to_char}
\alias{factor_to_char}
\title{Converts Characters to Factors in an Object}
\usage{
  factor_to_char(X)
}
\arguments{
  \item{X}{an object.}
}
\description{
  Converts characters to factors in an object. Leaves
  non-character elements untouched. Objects that can be
  subscripted with \code{[[} are supported.
}
\details{
  We iterate through all elements in the object (e.g. if it
  is a list) and convert anything that is a factor into a
  character.
}

