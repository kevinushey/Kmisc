\name{sys}
\alias{sys}
\title{Invoke a System Command}
\usage{
sys(..., intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE,
  wait = TRUE, input = NULL, show.output.on.console = TRUE,
  minimized = FALSE, invisible = TRUE)
}
\arguments{
  \item{...}{System command to be invoked; this gets passed
  into \code{paste(..., sep='', collapse='')}.}

  \item{intern}{A logical (not \code{NA}) which indicates
  whether to capture the output of the command as an \R
  character vector.}

  \item{ignore.stdout}{Ignore \code{stdout}?}

  \item{ignore.stderr}{Ignore \code{stderr}?}

  \item{wait}{Should the \R interpreter wait for the
  program to finish execution?}

  \item{input}{If a character vector is supplied, this is
  copied one string per line to a temporary file, and the
  standard input of \code{...} is redirected to the file.}

  \item{show.output.on.console}{Windows only -- show output
  on console?}

  \item{minimized}{Windows only -- run the shell
  minimized?}

  \item{invisible}{Windows only -- run invisibly?}
}
\description{
This function wraps to \code{system}, but interprets all
un-named arguments as things to be \code{paste}-ed. See
\code{\link{system}} for further details.
}

