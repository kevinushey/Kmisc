\name{without_rows.re}
\alias{without_rows.re}
\title{Exclude Rows from a Data Frame Based on Regex Matching}
\usage{
  without_rows.re(df, regex, match_var = rownames(df), ...)
}
\arguments{
  \item{df}{a data frame with row names}

  \item{regex}{a regular expression to evaluate against the
  row names}

  \item{...}{optional arguments passed to \code{grep}}
}
\description{
  This function extracts rows from a data frame \code{df}
  for all rows -not- matching the \code{regex} pattern
  supplied.
}
\examples{
dat <- data.frame( x=letters, y=LETTERS )
rownames(dat) <- 1:26
## get all rows in dat with a 1, 2, 3 or 4 in the name
without_rows.re( dat, "[0-4]" )
}

