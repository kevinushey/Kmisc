##' Pattern matching and Replacement
##' 
##' These functions provide simple extensions to base regular expressions in \R,
##' primarily intended to assist with extraction of elements based on the
##' result of a regular expression evaluation.
##' 
##' The order is \emph{reversed} for the \code{re_} set of functions;
##' i.e., an \R object is expected first, rather than a regular expression pattern.
##'
##' @param x An \R object (in the case of \code{re_} methods), or a character
##'   vector (in the case of \code{ngrep}, \code{fgrep})
##' @param pattern character string containing a character string to be matched
##' in the given character vector; coerced by \code{\link{as.character}} if possible.
##' @param ignore.case boolean; if \code{TRUE} we perform case-insensitive matching.
##' @param perl boolean; if \code{TRUE}, we use perl-compatible regular expressions.
##' @param value boolean; if \code{TRUE} we return the actual matches; if \code{FALSE}
##' we return the indices corresponding to the matches.
##' @param fixed boolean; if \code{TRUE} the pattern is matched as-is. Overrides
##' all conflicting arguments.
##' @param useBytes boolean; if \code{TRUE} we perform matching byte-by-byte rather
##' than character by character.
##' @param match_var A variable to match on, as used in the
##'   \code{re_extract_rows} function.
##' @param invert Invert the results of the regular expression match?
##' @param ... Optional arguments passed to \code{grep} or \code{grepl}.
##' @rdname regex
##' @export
##' @seealso \code{\link{grep}}
##' @examples
##' ngrep( "abc", c("abc", "babcd", "abcdef", "apple"), value=TRUE )
ngrep <- function( pattern, x, ignore.case=FALSE, perl=FALSE, value=FALSE, fixed=FALSE, useBytes=FALSE ) {
  grep( pattern=pattern, x=x, ignore.case=ignore.case, perl=perl, value=value, fixed=fixed, useBytes=useBytes, invert=TRUE )
}

##' @rdname regex
##' @export
fgrep <- function( pattern, x, ignore.case=FALSE, value=FALSE, useBytes=FALSE, invert=FALSE ) {
  grep( pattern=pattern, x=x, ignore.case=ignore.case, value=value, useBytes=useBytes, invert=invert, fixed=TRUE )
}

##' @rdname regex
##' @export
##' @examples
##' if( re_exists(c("apple", "banana"), "^ap") ) print("yay!")
re_exists <- function(x, pattern, perl=TRUE, fixed=FALSE, ... ) {
  if (fixed) perl <- FALSE
  any( grepl( pattern, x, perl=perl, fixed=fixed, ... ) )
}

##' @rdname regex
##' @export
re.exists <- function(pattern, x, perl=TRUE, fixed=FALSE, ...) {
  .Deprecated("re_exists")
  if (fixed) perl <- FALSE
  re_exists(pattern=pattern, x=x, perl=perl, fixed=fixed, ...)
}

##' @rdname regex
##' @export
re_extract <- function( x, pattern, perl=TRUE, fixed=FALSE, ... ) {
  if (fixed) perl <- FALSE
  return( x[ grep( pattern, names(x), perl=perl, fixed=fixed, ... ) ] )
}

##' @rdname regex
##' @export
extract.re <- function(x, pattern, perl=TRUE, fixed=FALSE, ...) {
  .Deprecated("re_extract")
  return(re_extract(x=x, pattern=pattern, perl=perl, fixed=fixed, ...))
}

##' @rdname regex
##' @export
##' @seealso \code{\link{grep}}, \code{\link{regex}}
re_without <- function( x, pattern, perl=TRUE, fixed=FALSE, ... ) {
  if (fixed) perl <- FALSE
  return( x[ 1:length(x) %nin% grep( pattern, names(x), perl=perl, fixed=fixed, ... ) ] )
}

##' @rdname regex
##' @export
without.re <- function(x, pattern, perl=TRUE, fixed=FALSE, ...) {
  .Deprecated("re_without")
  return( re_without(x=x, pattern=pattern, perl=perl, fixed=fixed, ...) )
}

##' @rdname regex
##' @export
##' @examples
##' dat <- data.frame( x=letters, y=LETTERS )
##' rownames(dat) <- 1:26
##' ## get all rows in dat with a 1, 2, 3 or 4 in the name
##' re_extract_rows( dat, "[1-4]" )
re_extract_rows <- function(x, pattern, match_var=rownames(x), perl=TRUE, fixed=FALSE, ...) {
  return( x[ grep( pattern, match_var, perl=perl, fixed=fixed, ... ), ] )
}

##' @rdname regex
##' @export
extract_rows.re <- function(x, pattern, match_var=rownames(x), perl=TRUE, fixed=FALSE, ...) {
  .Deprecated("re_extract_rows")
  re_extract_rows(x=x, pattern=pattern, match_var=match_var, perl=perl, fixed=fixed, ...)
}

##' @rdname regex
##' @export
##' @examples
##' dat <- data.frame( x=letters, y=LETTERS )
##' rownames(dat) <- 1:26
##' ## get all rows in dat with a 1, 2, 3 or 4 in the name
##' re_without_rows( dat, "[0-4]" )
re_without_rows <- function(x, pattern, match_var=rownames(x), perl=TRUE, fixed=FALSE, ...) {
  return( x[ 1:nrow(x) %nin% grep( pattern, match_var, perl=perl, fixed=fixed, ... ), ] )
}

##' @rdname regex
##' @export
without_rows.re <- function(x, pattern, match_var=rownames(x), perl=TRUE, fixed=FALSE, ...) {
  .Deprecated("re_without_rows")
  return( x[ 1:nrow(x) %nin% grep( pattern, match_var, perl=perl, fixed=fixed, ... ), ] )
}
