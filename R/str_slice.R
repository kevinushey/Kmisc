##' Slice a Vector at Consecutive Indices
##' 
##' This function 'slices' the strings of a character vector \code{x} at consecutive indices
##' \code{n}, thereby generating consecutive substrings of length \code{n}
##' and returning the result as a list. Not safe for use with unicode characters.
##' 
##' @param x a character vector.
##' @param n integer (or numeric coercible to integer); index at which to slice.
##' @export
##' @return A list of length equal to the length of \code{x}, with each element
##' made up of the substrings generated from \code{x[i]}.
##' @note Underlying code is written in C for fast execution.
##' @seealso \code{\link{str_slice2}}, for slicing a UTF-8 encoded vector.
##' @examples
##' x <- c("ABCD", "EFGH", "IJKLMN")
##' str_slice(x, 2)
##' 
str_slice <- function(x, n=1) {
  
  stopifnot( n > 0 )
  
  if( n != as.integer(n) ) {
    warning("non-integer cutpoint passed; cutpoint rounded down" )
  }
  
  if( any( nchar(x) %% n != 0 ) ) {
    warning( 
      "n is not a divisor of all x; output will be truncated for those x. check with `nchar(x) %% n != 0`"
    )
  }
  
  tmp <- .Call( Cstr_slice, as.character(x), as.integer(n) )
  tmp[ which(is.na(x)) ] <- NA
  return(tmp)
  
}

##' Slice a Vector at Consecutive Indices
##' 
##' This function 'slices' the strings of a character vector \code{x} at consecutive indices
##' \code{n}, thereby generating consecutive substrings of length \code{n}
##' and returning the result as a list. This function will safely 'slice' a
##' UTF-8 encoded vector.
##' 
##' @param x a character vector.
##' @param n integer (or numeric coercible to integer); index at which to slice.
##' @param USE.NAMES logical. if names attribute already exists on \code{x},
##' pass this through to the result?
##' @export
##' @return A list of length equal to the length of \code{x}, with each element
##' made up of the substrings generated from \code{x[i]}.
##' @note Safe for use with UTF-8 characters, but slower than \code{str_slice}.
##' @seealso \code{\link{str_slice}}, for slicing an ASCII vector.
str_slice2 <- function(x, n=1, USE.NAMES=TRUE) {
  
  stopifnot( n > 0 )
  
  if( n != as.integer(n) ) {
    warning("non-integer cutpoint passed; cutpoint rounded down" )
  }
  
  if( any( nchar(x) %% n != 0 ) ) {
    warning( 
      "n is not a divisor of all x"
    )
  }
  
  tmp <- lapply( x, function(xx) {
    substring( xx,
               seq(1, by=n, length.out=nchar(xx)/n),
               seq(1+n-1, by=n, length.out=nchar(xx)/n)
    )
  })
  
  return(tmp)
  
}
