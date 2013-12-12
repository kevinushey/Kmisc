##' Check whether there are any Missing Values in a Vector
##' 
##' This function checks whether there are any missing values in an \R
##' object. For list objects, we recurse over each entry. This is typically 
##' faster than \code{any(is.na(x))} as we exit and return \code{TRUE} as soon 
##' as an \code{NA} is discovered.
##' 
##' @param x An \R object.
##' @param how The simplification to use if \code{x} is a list. See
##' \code{\link{rapply}} for more details.
##' @export
any_na <- function(x, how="unlist") {
  if( is.list(x) ) {
    f <- function(x) {
      .Call(Cany_na, x)
    }
    return( rapply( x, how=how, f ) )
  } else {
    return(.Call(Cany_na, x))
  }
}
