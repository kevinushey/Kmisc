\name{split_file}
\alias{split_file}
\title{Split a File by Unique Entries in a Column with Awk}
\usage{
  split_file(file, column,
    outDir = file.path(getwd(), "split"), prepend = "",
    dots = 1)
}
\arguments{
  \item{file}{The location of the file we are splitting.}

  \item{column}{The column (by index) to split over.}

  \item{outDir}{The directory to output the files. We
  default to the sub-directory \code{split} of the current
  working directory.}

  \item{prepend}{A string to prepend to the output file
  names; typically an identifier for what the column is
  being split over.}

  \item{dots}{The number of dots used in making up the file
  extension. If there are no dots in the file name, this
  argument is ignored.}
}
\description{
  This script calls \code{awk} in order to split a file
  according to unique entries in a column. The name of the
  entry being split over is appended to the file name
  (before the file extension).
}
\details{
  This function requires the command line tools \code{awk},
  \code{uniq} and \code{sort} to be available. If you're
  running Windows, please either install cygwin, or have
  Rtools installed and available on your PATH.
}

