\name{ngrep}
\alias{ngrep}
\title{Inverse Grep}
\usage{
ngrep(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
  fixed = FALSE, useBytes = FALSE)
}
\arguments{
  \item{pattern}{character string containing a character
  string to be matched in the given character vector;
  coerced by \code{\link{as.character}} if possible.}

  \item{x}{a character vector where matches are sought, or
  an object coercable by \code{as.character}.}

  \item{ignore.case}{boolean; if \code{TRUE} we perform
  case-insensitive matching.}

  \item{perl}{boolean; if \code{TRUE}, we use
  perl-compatible regular expressions.}

  \item{value}{boolean; if \code{TRUE} we return the actual
  matches; if \code{FALSE} we return the indices
  corresponding to the matches.}

  \item{fixed}{boolean; if \code{TRUE} the pattern is
  matched as-is. Overrides all conflicting arguments.}

  \item{useBytes}{boolean; if \code{TRUE} we perform
  matching byte-by-byte rather than character by
  character.}
}
\description{
This is equivalent to calling \code{grep} with
\code{invert=TRUE}.
}
\examples{
ngrep( "abc", c("abc", "babcd", "abcdef", "apple"), value=TRUE )
}
\seealso{
\code{\link{grep}}
}

