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
  FUN <- match.fun(FUN)
  formals <- formals(FUN)
  if (is.null(formals)) return(lapply(X=X, FUN=FUN, ...))
  nms <- names(formals)
  if (length(nms) == 1) {
    return(lapply(X=X, FUN=FUN, ...))
  } else if (length(nms) > 1) {
    n <- length(X)
    output <- vector("list", n)
    i <- 1
    while (i <= n) {
      tmp <- FUN(X[[i]], i, ...)
      if (is.null(tmp)) output[[i]] <- list(NULL)
      else output[[i]] <- tmp
      i <- i + 1
    }
    if (!is.null(names(X))) names(output) <- names(X)
    return(output)
  } else {
    stop("Internal error in 'enumerate'")
  }
}
