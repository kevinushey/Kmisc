\name{read.cb}
\alias{read.cb}
\title{Read Tabular Data from the Clipboard}
\usage{
  read.cb(sep = "\t", ...)
}
\arguments{
  \item{sep}{the delimiter used in the copied text.}

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

