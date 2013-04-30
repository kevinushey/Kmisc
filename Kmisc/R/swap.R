#' Swap Elements in a Vector
#' 
#' This function swaps elements in a vector. See examples for usage.
#' 
#' @param vec the vector of items whose elements you will be replacing.
#' @param from the items you will be mapping 'from'.
#' @param to the items you will be mapping 'to'. must be same length and
#' order as \code{from}.
#' @param ... optional arguments passed to \code{match}.
#' @export
#' @seealso \code{\link{match}}
#' @examples
#' x <- c(1, 2, 2, 3)
#' from <- c(1, 2)
#' to <- c(10, 20)
#' swap( x, from, to )
#' 
#' ## alternatively, we can submit a named character vector
#' ## we translate from value to name. note that this forces
#' ## a conversion to character
#' names(from) <- to
#' swap( x, from )
#' 
#' ## NAs are handled sensibly. Types are coerced as needed.
#' x <- c(1, NA, 2, 2, 3)
#' swap(x, c(1, 2), c("a", "b") )
#' 
swap <- function( vec, from, to=names(from), ... ) {
  return( .Call( "Kmisc_swap", vec, from, to, PACKAGE="Kmisc" ) )
}
