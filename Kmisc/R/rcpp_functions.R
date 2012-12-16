#' Slice a String at Consecutive Indices
#' 
#' Slice a string at consecutive indices, such that substrings of equal length
#' are generated from a string.
#' @param x a character vector of length one; ie, a string.
#' @param n the index at which to cut
#' @return A character vector, 'sliced' at consecutive indices \code{n}.
#' @export
#' @useDynLib Kmisc
#' @examples
#' x <- "ABCDEF"
#' stopifnot( strslice( x, 2 ) == c("AB", "CD", "EF") )
#' 
#' ## applying strslice to a vector of 'strings'
#' y <- c("ABCD", "EFGH", "IJK") ## note unequal length
#' sapply( y, function(x) { strslice(x, 2) } ) ## each element sliced; warning generated for "IJK"
strslice <- function(x,n) {
  
  if( length(x) > 1 ) {
    stop("x must be of length 1")
  }
  
  if( require("Rcpp") ) {
    
    x <- as.character(x); n <- as.integer(n)
    if( nchar(x)/n != floor( nchar(x)/n ) ) {
      warning( paste("number of characters in 'x' not a multiple of 'n'.",
                     "output will be truncated") )
    }
    
    if( n == 1 ) {
      return( unlist( strsplit( as.character(x), "" ) ) )
    } else {
      return( .Call("strslice", as.character(x), as.integer(n),
                    PACKAGE="Kmisc") )
    }
  } else {
    starts <- seq( 1L, nchar(x), by = n )
    substring( x, starts, starts + n-1L )
  }
}

#' Reverse Elements of a Character Vector
#' 
#' Use this function to reverse each element in a vector of characters (or a
#' vector of items coercable to character).
#' @param x a character vector.
#' @export
#' @useDynLib Kmisc
#' @return Returns \code{x} with each element reversed.
#' @examples
#' kRev( c("abc", "def") ) ## returns c("cba", "fed")
kRev <- function(x) {
  if( require("Rcpp") ) {
    .Call("kRev", as.character(x), PACKAGE="Kmisc")
  } else {
    return( sapply( x, function(xx) {
      tmp <- paste( sep="", rev( us(x) ), collapse="" )
      names(tmp) <- NULL
      return( tmp )
    } ) )
  }
}