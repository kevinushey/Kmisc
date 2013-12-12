##' Generate Counts of Values in a Vector
##' 
##' This function uses Rcpp sugar to implement a fast \code{table}, for
##' unique counts of a single vector.
##' 
##' @param x A numeric, integer, character or logical vector, or a (potentially
##'   nested) list of such vectors. If \code{x} is a list, we recursively apply
##'   counts throughout elements in the list.
##' @export
counts <- function(x) {
  if( is.list(x) ) {
    return( rapply(x, counts, how="list") )
  } else {
    return( .Call(CKmisc_counts, x) )
  }
}
