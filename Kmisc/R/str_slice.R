#' Slice a Vector at Indices
#' 
#' This function slices the strings of a character vector \code{x} at consecutive indices
#' \code{n}, thereby generating consecutive substrings of length \code{n}
#' and returning the result as a list
#' 
#' @param x a character vector
#' @param n integer (or numeric coercible to integer); index at which to slice
#' @export
#' @return A list of length equal to the length of \code{x}, with each element
#' made up of the substrings generated from \code{x[i]}.
str_slice <- function(x, n=1) {
  
  stopifnot( n > 0 )
  
  if( n != as.integer(n) ) {
    warning("non-integer cutpoint passed; cutpoint rounded down" )
  }
  
  if( any( nchar(x) %% n != 0 ) ) {
    warning( 
      "n is not a multiple of all x; output will be truncated for those x. check with `nchar(x) %% n != 0`"
    )
  }
  
  .Call( "str_slice", as.character(x), as.integer(n), PACKAGE="Kmisc" )
  
}