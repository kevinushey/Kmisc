\name{write.cb}
\alias{write.cb}
\title{Write Tabular Data to the Clipboard}
\usage{
write.cb(dat, row.names = FALSE, col.names = TRUE, sep = "\\t",
  quote = FALSE)
}
\arguments{
  \item{dat}{the data file you want to write out; passed to
  \code{write.table}.}

  \item{row.names}{logical. include the row names of dat?}

  \item{col.names}{logical. include the column names of
  dat?}

  \item{sep}{the delimiter used to separate elements after
  exporting dat.}

  \item{quote}{logical. include quotes around character
  vectors in dat?}
}
\description{
Directs output of \code{write.table} to the clipboard. This
can be useful if you want to quickly write some \R table
out and paste it into some other file, eg. a Word document,
Excel table, and so on.
}
\seealso{
\code{\link{write.table}}
}

