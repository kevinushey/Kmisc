#' Extract Rows from a Data Frame / Matrix Based on Regex Matching
#' 
#' This function extracts rows from a data frame / matrix \code{x} for all rows
#' matching the \code{regex} pattern supplied.
#' 
#' @param x a data frame / matrix with row names.
#' @param regex a regular expression to evaluate against the row names.
#' @param match_var the variable to match on. defaults to \code{rownames(x)}.
#' @param perl boolean. use perl-compatible regular expressions?
#' @param ... optional arguments passed to \code{grep}.
#' @export
#' @examples
#' dat <- data.frame( x=letters, y=LETTERS )
#' rownames(dat) <- 1:26
#' ## get all rows in dat with a 1, 2, 3 or 4 in the name
#' extract_rows.re( dat, "[1-4]" )
extract_rows.re <- function(x, regex, match_var=rownames(x), perl=TRUE, ...) {
  return( x[ grep( regex, match_var, perl=perl, ... ), ] )
}

#' Exclude Rows from a Data Frame / Matrix Based on Regex Matching
#' 
#' This function extracts rows from a data frame / matrix \code{x} for all rows
#' -not- matching the \code{regex} pattern supplied.
#' 
#' @param x a data frame / matrix with row names.
#' @param regex a regular expression to evaluate against the row names.
#' @param match_var the variable to match on. defaults to \code{rownames(x)}.
#' @param perl boolean. use perl-compatible regular expressions?
#' @param ... optional arguments passed to \code{grep}.
#' @export
#' @examples
#' dat <- data.frame( x=letters, y=LETTERS )
#' rownames(dat) <- 1:26
#' ## get all rows in dat with a 1, 2, 3 or 4 in the name
#' without_rows.re( dat, "[0-4]" )
without_rows.re <- function(x, regex, match_var=rownames(x), perl=TRUE, ...) {
  return( x[ 1:nrow(x) %nin% grep( regex, match_var, perl=perl, ... ), ] )
}
