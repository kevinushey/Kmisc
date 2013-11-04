##' Transpose a List
##' 
##' @param x A matrix-like list; ie, a list with each element of
##'   the same length. Types are coerced as necessary.
##' @export
##' @examples
##' l <- replicate(100, rnorm(100), simplify=FALSE)
##' stopifnot( identical(
##'   transpose_list( transpose_list(l) ), l
##' ) )
transpose_list <- function(x) {
  return( .Call("transpose_list", as.list(x), PACKAGE="Kmisc") )
}
