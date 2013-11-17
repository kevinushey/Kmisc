\name{transpose}
\alias{transpose}
\alias{transpose.data.frame}
\alias{transpose.default}
\alias{transpose.list}
\title{Tranpose an Object}
\usage{
transpose(x)

\method{transpose}{list}(x)

\method{transpose}{data.frame}(x)

\method{transpose}{default}(x)
}
\arguments{
  \item{x}{A matrix, data.frame, or matrix-like list.}
}
\description{
  This functions similarily to \R's \code{t}, but we add a
  new method, \code{transpose.list}, for transposing lists
  in a specific way.
}

