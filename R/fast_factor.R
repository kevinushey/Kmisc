##' Fast Factor Generation
##' 
##' This function generates factors quickly using faster sorting and
##' matching algorithms available in Rcpp.
##' 
##' @importFrom data.table setattr
##' @param x An object of atomic type \code{integer}, \code{numeric},
##' \code{character} or \code{logical}.
##' @param levels An optional character vector of levels. Is coerced to the same type as
##' \code{x}. By default, we compute the levels as \code{sort(unique(x))}.
##' @export
factor_ <- function(x, levels=NULL) {
  
  if( is.factor(x) ) {
    return(x)
  }
  
  out <- .Call( CKmisc_fast_factor, x, levels )
  if( is.logical(x) ) {
    setattr(out, "levels", c("FALSE", "TRUE"))
  }
  return(out)
}
