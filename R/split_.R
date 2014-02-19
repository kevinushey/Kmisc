##' Fast Split
##' 
##' A faster version of \code{\link{split}} -- this divides the data in the
##' vector \code{x} into the groups defined by \code{f}.
##' 
##' @param x A vector containing values to be divided into groups.
##' @param f A grouping variable defining the grouping.
##' @param na.last Boolean, if \code{TRUE} then \code{NA} elements are
##'   split as well. Note that we diverge from \code{split} in that we
##'   allow \code{NA} elements to be split on.
##' @export
split_ <- function(x, f, na.last=TRUE) {
  na.last <- if (isTRUE(na.last)) TRUE else NA
  if (is.character(x))
    return( split(x, factor_(f, na.last=na.last)) )
  else
    return( .Call(CKmisc_split, x, f, isTRUE(na.last)) )
}
