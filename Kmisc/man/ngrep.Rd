\name{ngrep}
\alias{ngrep}
\title{Inverse grep}
\usage{
  ngrep(pattern, x, perl = TRUE, value = FALSE, ...)
}
\arguments{
  \item{pattern}{regular expression pattern}

  \item{x}{vector of items to match against}

  \item{perl}{boolean. use perl-compatible regular
  expressions?}

  \item{value}{logical. if \code{TRUE}, we return the
  actual values matched; otherwise we return the indices of
  the matches.}

  \item{...}{passed to grep}
}
\value{
  the elements in \code{x} that did not match the pattern
  supplied
}
\description{
  From a vector \code{x}, return the indices of elements
  that do not match \code{pattern}.
}
\details{
  Use \code{value=TRUE} to return the actual non-matched
  elements.
}
\examples{
some_files <- c("output_file.tar.gz", "old_log1.txt", "old_log2.txt")
## get the non-log files
files_I_want <- ngrep( "_log[0-9]+\\\\.txt$", some_files )
}
\seealso{
  \code{\link{grep}} \code{\link{regex}}
}

