##' Faster tapply
##' 
##' This function acts as a faster version of \code{tapply} for the common case of
##' splitting an atomic vector by another atomic vector, and then applying a
##' function.
##' 
##' @param X An atomic vector.
##' @param INDEX A vector coercable to factor; must be one of the common atomic types:
##' factor, integer, numeric, or character.
##' @param FUN The function to be applied. See more details at \code{\link{lapply}}.
##' @param ... Optional arguments to pass to \code{FUN}.
##' @param simplify boolean; if \code{TRUE}, we unlist the output and hence return
##' a named vector of values.
##' @export
##' @examples
##' x <- rnorm(100)
##' gp <- sample( 1:10, 100, TRUE )
##' stopifnot( all(
##'   tapply(x, gp, mean) == tapply_(x, gp, mean)
##' ) )
tapply_ <- function(X, INDEX, FUN=NULL, ..., simplify=TRUE) {
  if( simplify ) {
    return( unlist( lapply( split(X, factor_(INDEX)), FUN, ... ) ) )
  } else {
    return( lapply( split(X, factor_(INDEX)), FUN, ... ) )
  }
}
