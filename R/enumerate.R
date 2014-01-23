##' Enumerate over a Vector
##'
##' This function extends `lapply` to operate on functions taking two
##' arguments -- the first refers to each element within a vector, while
##' the second refers to the current index.
##'
##' @param X A vector.
##' @param FUN A function, taking two arguments.
##' @param ... Optional arguments to \code{FUN}.
##' @export
##' @examples
##' data <- setNames( list(1, 2, 3), c('a', 'b', 'c') )
##' v <- replicate(10, rnorm(1E3), simplify=FALSE)
##' identical( lapply(v, sum), enumerate(v, sum) )
##' f <- function(x, i) i
##' enumerate(v, f)
##' enumerate(v, function(x, i) i)
enumerate <- function(X, FUN, ...) {
  call <- match.call(expand.dots=FALSE)
  nargs <- sum( as.character(formals(FUN)) == "" )
  return( .Call(Cenumerate, call, environment(), as.integer(nargs)) )
}
