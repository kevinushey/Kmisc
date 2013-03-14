\name{split_file}
\alias{split_file}
\title{Split a File by Unique Entries in a Column}
\usage{
  split_file(file, column, sep = "\t",
    outDir = file.path(dirname(file), "split"),
    prepend = "", dots = 1, skip = 0, verbose = TRUE)
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

  \item{skip}{Integer; number of rows to skip (e.g. to
  avoid a header).}

  \item{verbose}{Be chatty?}
}
\description{
  This script splits a delimited file by unique entries in
  a selected column. The name of the entry being split over
  is appended to the file name (before the file extension).
}
\details{
  This function should help users out in the unfortunate
  case that the data they have attempted to read is too
  large to fit into RAM. By splitting the file into
  multiple, smaller files, we hope that each file,
  post-splitting, is now small enough to fit into RAM.

  The focus is on efficient splitting of 'well-mannered'
  files, so if you have comments, quoted delimiters, cell
  entries that have paragraphs of unicode text, or other
  wacky things this is probably not the function for you.
}
\seealso{
  \code{\link{extract_rows_from_file}}
}

