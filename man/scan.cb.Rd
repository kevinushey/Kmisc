\name{scan.cb}
\alias{scan.cb}
\title{Read Data from the Clipboard}
\usage{
scan.cb(what = character(), sep = "\\n", quiet = TRUE, ...)
}
\arguments{
  \item{what}{passed to \code{scan}.}

  \item{sep}{passed to \code{scan}.}

  \item{quiet}{passed to \code{scan}.}

  \item{...}{passed to \code{scan}.}
}
\description{
Convenience function for reading data from the clipboard.
Wraps to \code{\link{scan}}. By default, we assume the data
is \code{character}, and delimit by new lines.
}
\seealso{
\code{\link{scan}}
}

