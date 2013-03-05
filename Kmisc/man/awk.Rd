\name{awk}
\alias{awk}
\title{A Simple Front-end to Awk}
\usage{
  awk(code, BEGIN = NULL, END = NULL, vars = NULL,
    fs = NULL, file, out = TRUE, verbose = FALSE)
}
\arguments{
  \item{code}{The \code{awk} code you want to put in the
  main execution block.}

  \item{BEGIN}{A block of code to run as though it were
  within the \code{BEGIN} block.}

  \item{END}{A block of code to run as though it were
  within the \code{END} block.}

  \item{vars}{A named list, whereby variables are assigned
  so that \code{name=value}.}

  \item{fs}{The field separator (passed to \code{-F}).}

  \item{file}{The file we are running \code{awk} on.}

  \item{out}{The location to output the result of the
  computation. If this is \code{TRUE}, we intern the
  process and bring the results back into the \R session.
  Otherwise, it should be a string specifying the output
  path for a file.}

  \item{verbose}{Output the generated \code{awk} code?}
}
\description{
  This function provides a simple front-end to \code{awk}.
}

