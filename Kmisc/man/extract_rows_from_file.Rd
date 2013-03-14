\name{extract_rows_from_file}
\alias{extract_rows_from_file}
\title{Extract Rows from File}
\usage{
  extract_rows_from_file(file, out = NULL, column, sep,
    keep)
}
\arguments{
  \item{file}{The input file to extract rows from.}

  \item{out}{The location to output the file. If this is
  \code{NULL}, we redirect output back into the \R
  session.}

  \item{column}{The column to check, indexed from 1.}

  \item{sep}{The delimiter used in \code{file}. Must be a
  single character.}

  \item{keep}{A character vector containing all items that
  we want to check and keep within the \code{column}th
  column of each row.}
}
\description{
  This function reads through a delimited file on disk,
  determines if the entry at the specified column is in a
  character vector of items, and writes that line to file
  (or to \R) if it is.
}
\seealso{
  \code{\link{split_file}}
}

