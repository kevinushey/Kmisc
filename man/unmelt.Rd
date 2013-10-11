\name{unmelt}
\alias{unmelt}
\title{Unmelt a Melted Data Frame}
\usage{
  unmelt(data, id = "variable", value = "value")
}
\arguments{
  \item{data}{A \code{data.frame}.}

  \item{id}{The index, or name, of the \code{id} vector;
  analogous to the vector produced with name
  \code{variable.name}.}

  \item{value}{The value of the \code{value} vector;
  analogous to the vector produced with name
  \code{value.name}.}
}
\description{
  This function undoes the \code{melt}ing process done by
  either \code{reshape2::melt} or \code{\link{melt_}}.
}

