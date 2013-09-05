\name{prepare_package}
\alias{prepare_package}
\title{Prepare Package}
\usage{
  prepare_package(build = TRUE, check = TRUE,
    install = FALSE)
}
\arguments{
  \item{build}{Build the package with \code{R CMD build}?}

  \item{check}{Check the package with \code{R CMD check}?}

  \item{install}{Install the package with \code{R CMD
  INSTALL}? Only be done if \code{build} is \code{TRUE} as
  well.}
}
\description{
  This function prepares the package such that all the C /
  C++ source files are concatenated into one file (for each
  source). This increases compilation time, and produces a
  tarball that can be used for submission to CRAN.
}

