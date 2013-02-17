\name{factor_to_char}
\alias{factor_to_char}
\title{Converts Factors to Characters in an Object}
\usage{
  factor_to_char(X)
}
\arguments{
  \item{X}{an object.}
}
\description{
  Converts factors to characters in an object. Leaves
  non-character elements untouched.
}
\details{
  We iterate through all elements in the object (e.g. if it
  is a list) and convert anything that is a factor into a
  character.
}

