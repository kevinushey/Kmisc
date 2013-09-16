\name{read.cb}
\alias{read.cb}
\title{Read Tabular Data from the Clipboard}
\usage{
  read.cb(sep = "\t", header = TRUE, ...)
}
\arguments{
  \item{sep}{the delimiter used in the copied text.}

  \item{header}{boolean; does the first row contain column
  names?}

  \item{...}{optional arguments passed to
  \code{read.table}.}
}
\description{
  Convenience function for reading tabular data from the
  clipboard. The function checks the system OS and provides
  the appropriate wrapper call to \code{\link{read.table}}.
}
\examples{
## with some data on the clipboard, simply write
# x <- read.cb()
}
\seealso{
  \code{\link{read.table}}
}

