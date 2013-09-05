##' Value Matching
##' 
##' These are a couple of mostly self-explanatory wrappers around \code{\%in\%}.
##' 
##' \code{\%nin\%} returns a logical vector indicating if there is
##' no match for its left operand. It is the inverse of \code{x \%in\% y}.
##' 
##' \code{\%kin\%} returns the actual values of \code{x} for which 
##' \code{x \%in\% y}.
##' 
##' \code{\%knin\%} returns the actual values of \code{x} for which
##' \code{x \%nin\% y}.
##' 
##' @param x Vector or \code{NULL}: the values to be matched.
##' @param y Vector or \code{NULL}: the values to be matched against.
##' @name value_matching
NULL

##' @rdname value_matching
##' @usage x \%nin\% y
##' @export
"%nin%" <- function(x, y) {
  return( !(x %in% y) )
}

##' @rdname value_matching
##' @usage x \%kin\% y
##' @export
"%kin%" <- function(x, y) {
  return( x[ x %in% y] )
}

##' @rdname value_matching
##' @usage x \%knin\% y
##' @export
"%knin%" <- function(x, y) {
  return( x[ x %nin% y ] )
}
