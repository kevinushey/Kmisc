##' Test if an Object is Sorted
##' 
##' Test if an object is sorted, without the cost of sorting it.
##' Wrapper to \code{\link{is.unsorted}}.
##' 
##' @param x an \R object with a class or a numeric, complex, character or logical vector.
##' @param na.rm logical. Should missing values be removed before checking?
##' @param strictly logical indicating if the check should be for strictly increasing values.
##' @export
##' @seealso \code{\link{is.unsorted}}
##' @examples
##' stopifnot( is.sorted(1, 2, 4) )
is.sorted <- function(x, na.rm=FALSE, strictly=FALSE) {
  return( !is.unsorted(x, na.rm=na.rm, strictly=strictly) )
}
