##' Generate Counts of Values in a Vector
##' 
##' This function uses Rcpp sugar to implement a fast \code{table}, for
##' unique counts of a single vector.
##' 
##' @param x A numeric, integer, or character vector.
##' @export
counts <- function(x) {
  if( is.list(x) ) {
    return( rapply(x, counts, how="list") )
  } else {
    ## TODO: update this once new Rcpp is released to CRAN
    if( packageVersion("Rcpp") > "0.10.3.2" ) {
      return( .Call("Kmisc_counts", x, PACKAGE="Kmisc"))
    } else {
      return( table(x) )
    }
  }
}
