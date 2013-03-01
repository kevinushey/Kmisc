\name{split_file}
\alias{split_file}
\title{Split a File by Unique Entries in a Column with Awk}
\usage{
  split_file(file, column, prepend = "")
}
\arguments{
  \item{file}{The location of the file we are splitting.}

  \item{column}{The column (by index) to split over.}

  \item{prepend}{A string to prepend to the output file
  name.}
}
\description{
  This script calls \code{awk} in order to split a file
  according to unique entries in a column. The name of the
  entry being split over is appended to the file name
  (before the file extension).
}
\details{
  This function requires the command line tools \code{awk},
  \code{uniq} and \code{sort} to be available.
}

