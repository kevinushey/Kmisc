\name{split_file}
\alias{split_file}
\title{Split a File by Unique Entries in a Column}
\usage{
  split_file(file, column, sep = "\t",
    outDir = file.path(dirname(file), "split"),
    prepend = "", dots = 1)
}
\arguments{
  \item{file}{The location of the file we are splitting.}

  \item{column}{The column (by index) to split over.}

  \item{sep}{The file separator. Must be a single
  character.}

  \item{outDir}{The directory to output the files.}

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

