##' Fast Split
##' 
##' A faster version of \code{\link{split}} -- this divides the data in the
##' vector \code{x} into the groups defined by \code{f}.
##' 
##' @param x A vector containing values to be divided into groups.
##' @param f A grouping variable defining the grouping.
##' 
##' @export
split_ <- function(x, f) {
  if (is.character(x))
    return( split(x, factor_(f)) )
  else
    return( .Call(CKmisc_split, x, f) )
}
