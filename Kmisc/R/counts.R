#' Generate Counts of Values in a Vector
#' 
#' This function uses Rcpp sugar to implement a fast \code{table}, for
#' unique counts of a single vector.
#' 
#' @param x A numeric, integer, or character vector.
#' @export
counts <- function(x) {
  return( .Call("Kmisc_counts", x, PACKAGE="Kmisc"))
}
