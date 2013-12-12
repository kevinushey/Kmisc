##' Determine if Value Lies within Interval
##' 
##' This function determines whether elements of a numeric vector \code{x} lie
##' within boundaries \code{[lo, hi)}. Marginally slower than the \R equivalent
##' code \code{x >= lo & x < hi} for small vectors; much faster for very large
##' vectors.
##' 
##' @export
##' @param x numeric. vector of numbers.
##' @param lo numeric, length 1. lower boundary.
##' @param hi numeric, length 1. upper boundary.
##' @param include.lower boolean. include the lower endpoint?
##' @param include.upper boolean. include the upper endpoint?
##' @examples
##' x <- runif(100); lo <- 0.5; hi <- 1
##' f <- function(x, lo, hi) {
##'   return( x >= lo & x < hi )
##' }
##' stopifnot( all( in_interval( x, lo, hi ) == f(x, lo, hi) ) )
in_interval <- function(x, lo, hi, include.lower=TRUE, include.upper=FALSE) {
  
  .Call(Cin_interval, 
    as.numeric(x), 
    as.numeric(lo), 
    as.numeric(hi), 
    as.logical(include.lower),
    as.logical(include.upper)
  )
  
}
