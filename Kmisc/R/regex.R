#' Inverse Grep
#' 
#' This is equivalent to calling \code{grep} with \code{invert=TRUE}.
#'
#' @param pattern character string containing a character string to be matched
#' in the given character vector; coerced by \code{\link{as.character}} if possible.
#' @param x a character vector where matches are sought, or an object coercable
#' by \code{as.character}.
#' @param ignore.case boolean; if \code{TRUE} we perform case-insensitive matching.
#' @param perl boolean; if \code{TRUE}, we use perl-compatible regular expressions.
#' @param value boolean; if \code{TRUE} we return the actual matches; if \code{FALSE}
#' we return the indices corresponding to the matches.
#' @param fixed boolean; if \code{TRUE} the pattern is matched as-is. Overrides
#' all conflicting arguments.
#' @param useBytes boolean; if \code{TRUE} we perform matching byte-by-byte rather
#' than character by character.
#' @export
#' @seealso \code{\link{grep}}
#' @examples
#' ngrep( "abc", c("abc", "babcd", "abcdef", "apple"), value=TRUE )
ngrep <- function( pattern, x, ignore.case=FALSE, perl=FALSE, value=FALSE, fixed=FALSE, useBytes=FALSE ) {
  grep( pattern=pattern, x=x, ignore.case=ignore.case, perl=perl, value=value, fixed=fixed, useBytes=useBytes, invert=TRUE )
}

#' Fixed/Fast Grep
#' 
#' This is equivalent to calling \code{grep} with \code{fixed=TRUE}.
#'
#' @param pattern character string containing a character string to be matched
#' in the given character vector; coerced by \code{\link{as.character}} if possible.
#' @param x a character vector where matches are sought, or an object coercable
#' by \code{as.character}.
#' @param ignore.case boolean; if \code{TRUE} we perform case-insensitive matching.
#' @param value boolean; if \code{TRUE} we return the actual matches; if \code{FALSE}
#' we return the indices corresponding to the matches.
#' @param useBytes boolean; if \code{TRUE} we perform matching byte-by-byte rather
#' than character by character.
#' @param invert boolean; if \code{TRUE} we return indices or values for elements that
#' do not match.
#' @export
#' @seealso \code{\link{grep}}
#' @examples
#' fgrep( "abc", c("abc", "babcd", "abcdef", "apple"), value=TRUE )
fgrep <- function( pattern, x, ignore.case=FALSE, value=FALSE, useBytes=FALSE, invert=FALSE ) {
  grep( pattern=pattern, x=x, ignore.case=ignore.case, value=value, useBytes=useBytes, invert=invert, fixed=TRUE )
}
