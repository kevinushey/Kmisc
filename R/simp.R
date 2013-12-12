##' Area Under the Curve with Simpson's Rule
##' 
##' This function computes the area under the curve using Simpson's
##' (composite) rule, for a function \code{f(x)} evaluated over equally
##' spaced points \code{x}.
##' 
##' @param x A vector of values \code{x}.
##' @param y A vector of values \code{f(x)}.
simp <- function(x, y) {
  return( .Call(Csimp, as.numeric(x), as.numeric(y) ) )
}
