\name{read}
\alias{read}
\alias{readlines}
\title{Read a File}
\usage{
  read(file)

  readlines(file)
}
\arguments{
  \item{file}{Path to a file.}
}
\description{
  These functions read a file in. We memory map the file
  for fast I/O. The file is read in as a character vector
  (length one for \code{read}, length \code{n} for
  \code{readlines}).
}

