##' Generate Counts of Values in a Vector
##' 
##' This function uses Rcpp sugar to implement a fast \code{table}, for
##' unique counts of a single vector. This implementation seeks to
##' produce identical output to \code{table(x, useNA="ifany")}.
##' 
##' @param x A numeric, integer, character or logical vector, or a (potentially
##'   nested) list of such vectors. If \code{x} is a list, we recursively apply
##'   counts throughout elements in the list.
##' @export
counts <- function(x) {
  if (is.list(x)) {
    output <- rapply(x, counts, how="list")
  } else {
    output <- .Call(CKmisc_counts, x)
  }
  if (is.na( names(output)[1] )) {
    output <- output[ c(2:length(output), 1) ]
  }
  return(output)
}
