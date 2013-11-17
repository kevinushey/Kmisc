\name{Rcpp_gen_makevars}
\alias{Rcpp_gen_makevars}
\alias{rcpp_gen_makevars}
\title{Reproduce Rcpp Makevars Files}
\usage{
Rcpp_gen_makevars(src = file.path(getwd(), "src"))

rcpp_gen_makevars(src = file.path(getwd(), "src"))
}
\arguments{
  \item{src}{the location to output the Makevars.}
}
\description{
  If you're building a package and want a simple set of
  Makevars files to export, this function will handle it
  for you. Borrowed from the Rcpp
  \code{rcpp.package.skeleton} function.
}

