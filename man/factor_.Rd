\name{factor_}
\alias{factor_}
\title{Fast Factor Generation}
\usage{
factor_(x, levels = NULL)
}
\arguments{
  \item{x}{An object of atomic type \code{integer},
  \code{numeric}, \code{character} or \code{logical}.}

  \item{levels}{An optional character vector of levels. Is
  coerced to the same type as \code{x}. By default, we
  compute the levels as \code{sort(unique(x))}.}
}
\description{
  This function generates factors quickly using faster
  sorting and matching algorithms available in Rcpp.
}

