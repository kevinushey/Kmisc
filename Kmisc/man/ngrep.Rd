\name{ngrep}
\alias{ngrep}
\title{Inverse grep}
\usage{
  ngrep(pattern, x, ...)
}
\arguments{
  \item{pattern}{regular expression pattern}

  \item{x}{vector of items to match against}

  \item{...}{passed to grep}
}
\value{
  the elements in \code{x} that did not match the pattern
  supplied
}
\description{
  From a vector \code{x}, return only the elements that do
  not satisfy a particular regular expression statement;
  ie, remove all elements that are matched by
  \code{pattern}.
}
\details{
  Note that, by default, \code{grep} returns the location
  of matches, rather than the actual matches themselves.
  \code{ngrep} only returns matched elements, not the
  location of matches.
}
\examples{
some_files <- c("output_file.tar.gz", "old_log1.txt", "old_log2.txt")
## get the non-log files
files_I_want <- ngrep( "_log[0-9]+\\\\.txt$", some_files )
}
\seealso{
  \code{\link{grep}} \code{\link{regex}}
}

