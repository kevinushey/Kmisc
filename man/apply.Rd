\name{rowApply}
\alias{colApply}
\alias{rowApply}
\title{Apply Wrappers}
\usage{
rowApply(X, FUN, ..., drop = TRUE)

colApply(X, FUN, ..., drop = TRUE)
}
\arguments{
  \item{X}{A matrix, or a 2D array.}

  \item{FUN}{The function to be applied.}

  \item{...}{Optional arguments to \code{FUN}.}

  \item{drop}{Boolean. If \code{TRUE}, we 'drop' dimensions
  so that results of dimension \code{n x 1} or \code{1 x n}
  are coerced to vectors.}
}
\description{
  These are thin but clearer wrappers to \code{apply(x, 1,
  FUN, ...)} (row apply) and \code{apply(x, 2, FUN, ...)}
  (column apply). Intended for use with 2D \R
  \code{matrix}s. We do a bit more work to ensure row
  names, column names are passed along if appropriate.
}
\details{
  See \code{\link{apply}} for more info.
}

