#' Fast Factor Generation
#' 
#' This function generates factors quickly using faster sorting and
#' matching algorithms available in Rcpp.
#' 
#' @param x An object of atomic type \code{integer}, \code{numeric},
#' \code{character} or \code{logical}.
#' @export
fast_factor <- function(x) {
  
  if( is.factor(x) ) {
    return(x)
  }
  
  out <- .Call( "Kmisc_fast_factor", x, PACKAGE="Kmisc" )
  if( is.logical(x) ) {
    levels(out) <- c("FALSE", "TRUE")
  }
  return( out )
}