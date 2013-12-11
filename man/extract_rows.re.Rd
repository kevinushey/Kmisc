\name{extract_rows.re}
\alias{extract_rows.re}
\alias{re_extract_rows}
\title{Extract Rows from a Data Frame / Matrix Based on Regex Matching}
\usage{
extract_rows.re(x, regex, match_var = rownames(x), perl = TRUE, ...)

re_extract_rows(x, regex, match_var = rownames(x), perl = TRUE, ...)
}
\arguments{
  \item{x}{a data frame / matrix with row names.}

  \item{regex}{a regular expression to evaluate against the
  row names.}

  \item{match_var}{the variable to match on. defaults to
  \code{rownames(x)}.}

  \item{perl}{boolean. use perl-compatible regular
  expressions?}

  \item{...}{optional arguments passed to \code{grep}.}
}
\description{
This function extracts rows from a data frame / matrix
\code{x} for all rows matching the \code{regex} pattern
supplied.
}
\examples{
dat <- data.frame( x=letters, y=LETTERS )
rownames(dat) <- 1:26
## get all rows in dat with a 1, 2, 3 or 4 in the name
extract_rows.re( dat, "[1-4]" )
}

