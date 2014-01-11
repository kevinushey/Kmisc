##' Enumerate over a Vector
##' 
##' This function extends `lapply` to operate on functions taking two
##' arguments -- the first refers to each element within a vector, while
##' the second refers to the current index.
##' 
##' Dot arguments are currently unimplemented.
##' 
##' @param X A vector.
##' @param FUN A function, taking two arguments.
##' @export
##' @examples
##' data <- setNames( list(1, 2, 3), c('a', 'b', 'c') )
##' enumerate(data, function(x, i) {
##'   cat("List element '", i, "' has value '", x, "' and name '", 
##'     names(data)[i], "'.\n", sep="")
##' })
enumerate <- function(X, FUN, ...) {
  FUN <- match.fun(FUN)
  formals <- formals(FUN)
  nms <- names(formals)
  if (length(nms) == 1) {
    return(lapply(X=X, FUN=FUN, ...))
  } else if (length(nms) > 1) {
    n <- length(X)
    output <- vector("list", n)
    i <- 1
    while (i <= n) {
      output[[i]] <- FUN(X[[i]], i)
      i <- i + 1
    }
    return(output)
  } else {
    stop("Cannot pass a function with no arguments")
  }
}
